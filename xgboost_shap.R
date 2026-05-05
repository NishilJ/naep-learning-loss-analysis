required_pkgs <- c("xgboost", "Matrix", "ggplot2")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(xgboost)
library(Matrix)
library(ggplot2)

options(xgboost_score.autorun = FALSE)
source("xgboost_score.R")

numeric_covariates <- c(
  "median_income", "poverty_rate", "unemployment_rate",
  "pct_black", "pct_hispanic", "bach_or_higher_rate",
  "no_computer_rate", "internet_rate"
)

#' TreeSHAP via predcontrib: global importance, direction, summary plot, dependence plots.
#' CSV includes mean_abs_shap_frac_of_total (relative magnitude) and descriptive_corr_* —
#' Pearson cor(raw x, SHAP), marginal/descriptive only, not causal or conditional.
#'
#' @param shap_sample_max If NULL, use all post-2020 test rows. Set (e.g. 500) to cap runtime.
run_xgboost_shap <- function(
    path = "dataset.csv",
    use_cached_fit = TRUE,
    cached_fit_path = file.path("output", "xgboost_score_fit.rds"),
    force_refit = FALSE,
    shap_sample_max = NULL,
    random_seed = 42L,
    summary_top_n = 15L,
    dependence_top_k = 5L,
    validation_tol = 1e-4) {
  required_fit_fields <- c("model", "test_matrix", "test_df")
  load_cached_fit <- isTRUE(use_cached_fit) && !isTRUE(force_refit) && file.exists(cached_fit_path)

  if (load_cached_fit) {
    fit <- readRDS(cached_fit_path)
    if (!all(required_fit_fields %in% names(fit))) {
      warning("Cached fit missing expected fields; refitting model instead.")
      fit <- run_xgboost_score(path, cache_fit = TRUE, cache_path = cached_fit_path)
    } else {
      cat(sprintf("Loaded cached fit: %s\n", cached_fit_path))
    }
  } else {
    fit <- run_xgboost_score(path, cache_fit = isTRUE(use_cached_fit), cache_path = cached_fit_path)
  }
  model <- fit$model
  test_matrix <- fit$test_matrix
  test_df <- fit$test_df
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
  cat("\n=== SHAP decomposition check ===\n")
  cat(sprintf("max |sum(SHAP) + bias - prediction|: %.2e (tol %.2e)\n", max_abs_err, validation_tol))
  if (max_abs_err > validation_tol) {
    warning("SHAP rows do not reconstruct predictions within tolerance; check xgboost version / matrix alignment.")
  }

  mean_abs <- sort(colMeans(abs(feat_contrib)), decreasing = TRUE)
  mean_signed <- colMeans(feat_contrib)

  pct_pos <- as.numeric(colMeans(feat_contrib > 0))
  names(pct_pos) <- colnames(feat_contrib)

  # mean_abs_shap_frac_of_total: comparable relative importance across features.
  # descriptive_corr_*: Pearson cor(raw x, SHAP); marginal only, not causal or conditional.
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
  numeric_in_model <- intersect(numeric_covariates, colnames(feat_contrib))
  corr_col <- "descriptive_corr_x_vs_shap_marginal_naive"
  for (v in numeric_in_model) {
    if (!v %in% names(test_sub)) {
      next
    }
    x <- test_sub[[v]]
    sh <- feat_contrib[, v, drop = TRUE]
    ok <- !is.na(x) & !is.na(sh)
    if (sum(ok) > 2L) {
      imp_df[[corr_col]][imp_df$feature == v] <- cor(x[ok], sh[ok])
    }
  }

  out_dir <- "output"
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  write.csv(imp_df, file.path(out_dir, "xgboost_shap_global_importance.csv"), row.names = FALSE)

  p_bar <- ggplot(imp_df, aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Mean |SHAP| (post-2020 test)",
      x = NULL,
      y = "Mean |contribution|"
    ) +
    theme_bw(base_size = 11)

  ggsave(file.path(out_dir, "xgboost_shap_mean_abs.png"), p_bar, width = 8, height = 6, dpi = 150)

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
      title = paste0("SHAP value distribution (top ", length(top_summary), " features by mean |SHAP|)"),
      x = NULL,
      y = "SHAP"
    ) +
    theme_bw(base_size = 11)

  ggsave(file.path(out_dir, "xgboost_shap_summary_distribution.png"), p_summary, width = 9, height = 7, dpi = 150)

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
        title = paste0("Dependence: ", v),
        subtitle = "SHAP reflects effect on prediction vs baseline",
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

    fn <- file.path(out_dir, paste0("xgboost_shap_dependence_", v, ".png"))
    ggsave(fn, p_dep, width = 7, height = 5, dpi = 150)
  }

  top_deep <- head(imp_df, min(5L, nrow(imp_df)))
  write.csv(top_deep, file.path(out_dir, "xgboost_shap_top5_directional.csv"), row.names = FALSE)

  cat("\n=== SHAP (TreeSHAP via predcontrib) ===\n")
  cat(sprintf("Rows used: %d of %d test observations.\n", length(idx), nrow(test_matrix)))
  cat("Outputs:\n")
  cat(sprintf("  %s\n", file.path(out_dir, "xgboost_shap_global_importance.csv")))
  cat(sprintf("  %s\n", file.path(out_dir, "xgboost_shap_mean_abs.png")))
  cat(sprintf("  %s\n", file.path(out_dir, "xgboost_shap_summary_distribution.png")))
  for (v in dep_feats) {
    cat(sprintf("  %s\n", file.path(out_dir, paste0("xgboost_shap_dependence_", v, ".png"))))
  }
  cat(sprintf("  %s\n", file.path(out_dir, "xgboost_shap_top5_directional.csv")))

  invisible(list(
    fit = fit,
    shap_sample_index = idx,
    contributions = contrib,
    feature_contributions = feat_contrib,
    bias = bias_term,
    mean_abs_shap = mean_abs,
    importance_table = imp_df,
    validation = list(max_abs_reconstruction_error = max_abs_err, tolerance = validation_tol),
    dependence_features = dep_feats
  ))
}

if (isTRUE(getOption("xgboost_shap.autorun", default = TRUE))) {
  run_xgboost_shap("dataset.csv")
}
