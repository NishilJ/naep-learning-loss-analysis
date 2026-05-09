required_pkgs <- c("xgboost", "Matrix", "ggplot2")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(xgboost)
library(Matrix)
library(ggplot2)

options(xgboost_score_by_grade.autorun = FALSE)
source("xgboost_score_by_grade.R")

numeric_covariates <- c(
  "median_income", "gini_coefficient", "child_poverty_rate", "unemployment_rate",
  "pct_black", "pct_hispanic", "bach_or_higher_rate",
  "no_computer_rate", "internet_rate", "year_scaled"
)

lookup_feature_display_value <- function(test_row_df, fname) {
  if (fname %in% names(test_row_df)) {
    return(as.character(test_row_df[[fname]][1]))
  }
  m <- regexec("^state(.+)$", fname, perl = TRUE)
  r <- regmatches(fname, m)
  if (length(r[[1]]) >= 2L) {
    return(r[[1]][2])
  }
  NA_character_
}

#' TreeSHAP by grade: mean |SHAP|, dependence plots, CSV + waterfall-style locals.
#'
#' @param cached_fit_path RDS from run_xgboost_score_by_grade (list of grade_* bundles).
#' @param local_force_n PNGs per grade: ranked bar chart of bias + contributions for sampled rows.
#' @param local_long_max_rows Rows (after sampling) exported in long CSV per grade.
#' @param local_top_features For long CSV force rows only: widest features recorded per observation.
#' @param shap_sample_max If NULL, all post-2020 test rows for that grade model.
#'
#' Outputs under \code{file.path(base_out_dir)} (default \code{output/xgboost_by_grade/xgboost_shap_by_grade/grade_<g>/}).
run_xgboost_shap_by_grade <- function(
    path = "dataset.csv",
    use_cached_fit = TRUE,
    cached_fit_path = file.path("output", "xgboost_by_grade", "xgboost_score_by_grade_fit.rds"),
    force_refit = FALSE,
    grades = NULL,
    shap_sample_max = NULL,
    random_seed = 42L,
    summary_top_n = 15L,
    dependence_top_k = 5L,
    validation_tol = 1e-4,
    base_out_dir = file.path("output", "xgboost_by_grade", "xgboost_shap_by_grade"),
    local_force_n = 8L,
    local_long_max_rows = 50L,
    local_top_features = 40L,
    residual_weight_local = TRUE) {

  load_cached_fit <- isTRUE(use_cached_fit) && !isTRUE(force_refit) && file.exists(cached_fit_path)

  if (load_cached_fit) {
    fits <- readRDS(cached_fit_path)
    cat(sprintf("Loaded cached grade fits: %s\n", cached_fit_path))
  } else {
    fits <- run_xgboost_score_by_grade(
      path = path,
      grades = if (is.null(grades)) c(4, 8) else grades,
      cache_fit = TRUE,
      cache_path = cached_fit_path
    )
  }

  required_fit_fields <- c("model", "test_matrix", "test_df")
  grade_keys <- names(fits)
  if (!length(grade_keys)) {
    stop("Grade fit bundle is empty.")
  }

  if (!is.null(grades)) {
    want <- paste0("grade_", as.character(grades))
    grade_keys <- intersect(grade_keys, want)
    if (!length(grade_keys)) {
      stop("No matching grade_* keys found for requested grades.")
    }
    grade_keys <- sort(grade_keys)
  }

  if (!dir.exists(base_out_dir)) {
    dir.create(base_out_dir, recursive = TRUE)
  }

  all_out <- list()

  for (gk in grade_keys) {
    fg <- fits[[gk]]
    if (!is.list(fg) || !all(required_fit_fields %in% names(fg))) {
      warning(sprintf("Skipping %s: missing model/test fields.", gk))
      next
    }

    model <- fg$model
    test_matrix <- fg$test_matrix
    test_df <- fg$test_df
    test_label <- test_df$score

    n <- nrow(test_matrix)
    idx <- seq_len(n)
    if (!is.null(shap_sample_max) && is.finite(shap_sample_max) && shap_sample_max < n) {
      set.seed(random_seed)
      idx <- sort(sample.int(n, shap_sample_max))
    }

    dm <- xgb.DMatrix(test_matrix[idx, , drop = FALSE], label = test_label[idx])
    pred_from_model <- predict(model, dm)
    contrib <- predict(model, dm, predcontrib = TRUE)

    cn <- colnames(contrib)
    bias_idx <- if (!is.null(cn)) {
      w <- grep("^BIAS$|^\\(Intercept\\)$", cn, ignore.case = TRUE)
      if (length(w) == 1L) w else ncol(contrib)
    } else {
      ncol(contrib)
    }
    bias_term <- contrib[, bias_idx]
    feat_contrib <- contrib[, -bias_idx, drop = FALSE]

    recon <- rowSums(feat_contrib) + bias_term
    max_abs_err <- max(abs(recon - pred_from_model))
    cat(sprintf("\n=== Grade %s: SHAP decomposition check ===\n", gk))
    cat(sprintf("max |sum(SHAP) + bias - prediction|: %.2e (tol %.2e)\n", max_abs_err, validation_tol))
    if (max_abs_err > validation_tol) {
      warning(sprintf("Grade %s: SHAP rows do not reconstruct predictions within tolerance.", gk))
    }

    mean_abs <- sort(colMeans(abs(feat_contrib)), decreasing = TRUE)
    mean_signed <- colMeans(feat_contrib)
    pct_pos <- as.numeric(colMeans(feat_contrib > 0))
    names(pct_pos) <- colnames(feat_contrib)

    imp_df <- data.frame(
      feature = names(mean_abs),
      mean_shap = mean_signed[names(mean_abs)],
      mean_abs_shap = as.numeric(mean_abs),
      mean_abs_shap_frac_of_total = as.numeric(mean_abs) / sum(as.numeric(mean_abs)),
      pct_positive = pct_pos[names(mean_abs)],
      descriptive_corr_x_vs_shap_marginal_naive = NA_real_,
      row.names = NULL
    )

    test_sub <- test_df[idx, , drop = FALSE]
    corr_col <- "descriptive_corr_x_vs_shap_marginal_naive"
    raw_match <- colnames(feat_contrib)[colnames(feat_contrib) %in% names(test_sub)]
    for (v in raw_match) {
      x <- test_sub[[v]]
      sh <- feat_contrib[, v, drop = TRUE]
      ok <- !is.na(x) & !is.na(sh)
      if (sum(ok) > 2L) {
        imp_df[[corr_col]][imp_df$feature == v] <- cor(x[ok], sh[ok])
      }
    }

    grade_out <- file.path(base_out_dir, gk)
    if (!dir.exists(grade_out)) {
      dir.create(grade_out, recursive = TRUE)
    }

    write.csv(imp_df, file.path(grade_out, paste0(gk, "_global_importance.csv")), row.names = FALSE)

    imp_bar_df <- imp_df[imp_df$mean_abs_shap_frac_of_total >= 0.001, , drop = FALSE]
    if (nrow(imp_bar_df) == 0) {
      imp_bar_df <- head(imp_df, 1L)
    }

    p_bar <- ggplot(imp_bar_df, aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = sprintf("Mean |SHAP| - %s (features with >=0.1%% total contribution)", gk),
        x = NULL,
        y = "Mean |contribution|"
      ) +
      theme_bw(base_size = 11) +
      theme(axis.text.y = element_text(size = 8))

    ggsave(file.path(grade_out, paste0(gk, "_mean_abs_shap.png")), p_bar, width = 8, height = 6, dpi = 150)

    top_summary <- head(names(mean_abs), min(summary_top_n, length(mean_abs)))
    long_sum <- do.call(
      rbind,
      lapply(top_summary, function(f) {
        data.frame(feature = f, shap = feat_contrib[, f], stringsAsFactors = FALSE)
      })
    )
    long_sum$feature <- factor(long_sum$feature, levels = rev(top_summary))

    p_summary <- ggplot(long_sum, aes(x = feature, y = shap)) +
      geom_violin(fill = "grey85", color = NA, alpha = 0.9) +
      geom_jitter(width = 0.15, height = 0, alpha = 0.35, size = 0.6) +
      geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3) +
      coord_flip() +
      labs(
        title = sprintf("SHAP distribution (top %d by mean |SHAP|) — %s", length(top_summary), gk),
        x = NULL,
        y = "SHAP"
      ) +
      theme_bw(base_size = 11)

    ggsave(
      file.path(grade_out, paste0(gk, "_summary_distribution.png")),
      p_summary,
      width = 9,
      height = 7,
      dpi = 150
    )

    dep_candidates <- names(mean_abs)[names(mean_abs) %in% numeric_covariates]
    dep_feats <- head(dep_candidates, min(dependence_top_k, length(dep_candidates)))

    for (v in dep_feats) {
      x <- test_sub[[v]]
      sh <- feat_contrib[, v, drop = TRUE]
      dep_df <- data.frame(feature_value = x, shap = sh, stringsAsFactors = FALSE)
      n_unique_x <- length(unique(dep_df$feature_value[!is.na(dep_df$feature_value)]))
      p_dep <- ggplot(dep_df, aes(x = feature_value, y = shap)) +
        geom_point(alpha = 0.35, size = 1.2) +
        geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3) +
        labs(
          title = sprintf("Dependence (%s): %s", gk, v),
          subtitle = "SHAP vs feature value",
          x = v,
          y = "SHAP"
        ) +
        theme_bw(base_size = 11)
      if (n_unique_x >= 4L) {
        p_dep <- p_dep + geom_smooth(
          method = "loess",
          formula = y ~ x,
          se = TRUE,
          linewidth = 0.6,
          color = "darkred"
        )
      }

      ggsave(
        file.path(grade_out, paste0(gk, "_dependence_", v, ".png")),
        p_dep,
        width = 7,
        height = 5,
        dpi = 150
      )
    }

    # --- Local explanations: long CSV (top-|SHAP| features per retained row)
    residuals <- pred_from_model - test_label[idx]
    n_keep <- min(as.integer(local_long_max_rows), length(idx))
    if (n_keep < 1L) {
      long_rows <- list()
      ord <- integer(0)
    } else if (isTRUE(residual_weight_local)) {
      ord <- order(-abs(residuals))[seq_len(n_keep)]
    } else {
      ord <- seq_len(n_keep)
    }

    long_rows <- list()
    feats_all <- colnames(feat_contrib)
    for (j in seq_along(ord)) {
      r_within <- ord[j]
      r_global <- idx[r_within]
      tr <- test_sub[r_within, , drop = FALSE]
      vec <- feat_contrib[r_within, , drop = TRUE]
      o <- order(-abs(vec))
      take <- head(o, min(local_top_features, length(o)))
      for (ti in take) {
        fn <- feats_all[ti]
        long_rows[[length(long_rows) + 1L]] <- data.frame(
          grade_key = gk,
          sample_rank = j,
          test_row_index = r_global,
          state = as.character(tr$state[1]),
          year = tr$year[1],
          actual_score = test_label[idx][r_within],
          predicted_score = pred_from_model[r_within],
          bias = bias_term[r_within],
          feature = fn,
          shap = vec[ti],
          feature_value = lookup_feature_display_value(tr, fn),
          stringsAsFactors = FALSE
        )
      }
    }
    local_long <- if (length(long_rows)) do.call(rbind, long_rows) else data.frame()
    write.csv(local_long, file.path(grade_out, paste0(gk, "_local_explanations_long.csv")), row.names = FALSE)

    # --- Local feature-only bar charts (top contributions by |SHAP|)
    local_plot_dir <- file.path(grade_out, "local_plots")
    if (!dir.exists(local_plot_dir)) {
      dir.create(local_plot_dir, recursive = TRUE)
    }

    n_force <- min(as.integer(local_force_n), length(idx))
    if (isTRUE(residual_weight_local)) {
      force_ord <- order(-abs(residuals))[seq_len(n_force)]
    } else {
      force_ord <- seq_len(n_force)
    }
    top_k_bar <- min(25L, ncol(feat_contrib))

    if (!length(force_ord)) {
      warning(sprintf("Grade %s: no rows for local force plots.", gk))
    }

    for (j in seq_len(n_force)) {
      r_within <- force_ord[j]
      tr <- test_sub[r_within, , drop = FALSE]
      vec <- feat_contrib[r_within, , drop = TRUE]
      o <- order(-abs(vec))
      topf <- head(o, top_k_bar)
      fdf <- data.frame(
        feature = feats_all[topf],
        shap = vec[topf],
        stringsAsFactors = FALSE
      )
      fdf$posneg <- ifelse(fdf$shap >= 0, "positive", "negative")
      fdf$feature <- factor(fdf$feature, levels = rev(fdf$feature))

      ttl <- sprintf(
        "Local SHAP — %s — %s %s | pred=%.3f actual=%.3f",
        gk,
        as.character(tr$state),
        format(tr$year),
        pred_from_model[r_within],
        test_label[idx][r_within]
      )
      subtitle_txt <- sprintf(
        "Feature-only bars (top %d |SHAP|). bias=%.3f, sum(features)=%.3f",
        top_k_bar,
        bias_term[r_within],
        sum(vec)
      )

      p_loc <- ggplot(fdf, aes(x = feature, y = shap, fill = posneg)) +
        geom_col(width = 0.85) +
        coord_flip() +
        geom_hline(yintercept = 0, linewidth = 0.3) +
        scale_fill_manual(
          values = c(positive = "darkred", negative = "steelblue"),
          drop = FALSE
        ) +
        labs(
          title = ttl,
          subtitle = subtitle_txt,
          x = NULL,
          y = "Contribution"
        ) +
        theme_bw(base_size = 10) +
        theme(legend.position = "none")

      fpng <- sprintf("%s_local_force_%02d.png", gk, j)
      ggsave(file.path(local_plot_dir, fpng), p_loc, width = 7.5, height = 6.2, dpi = 150)
    }

    all_out[[gk]] <- list(
      fit_grade = fg,
      shap_sample_index = idx,
      feature_contributions = feat_contrib,
      bias = bias_term,
      mean_abs_shap = mean_abs,
      importance_table = imp_df,
      dependence_features = dep_feats,
      validation = list(max_abs_reconstruction_error = max_abs_err, tolerance = validation_tol)
    )

    cat(sprintf("\n=== SHAP by grade: %s ===\n", gk))
    cat(sprintf("Rows used: %d of %d test observations.\n", length(idx), nrow(test_matrix)))
    cat(sprintf("Output directory: %s\n", grade_out))
  }

  invisible(all_out)
}

if (isTRUE(getOption("xgboost_shap_by_grade.autorun", default = TRUE))) {
  run_xgboost_shap_by_grade("dataset.csv")
}
