required_pkgs <- c("ggplot2")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(ggplot2)

options(xgboost_shap.autorun = FALSE)
source("xgboost_shap.R")

run_grade_shap_split_analysis <- function(path = "dataset.csv", shap_sample_max = NULL, random_seed = 42L) {
  res <- run_xgboost_shap(
    path = path,
    shap_sample_max = shap_sample_max,
    random_seed = random_seed,
    use_cached_fit = TRUE
  )

  feat_contrib <- res$feature_contributions
  idx <- res$shap_sample_index
  test_sub <- res$fit$test_df[idx, , drop = FALSE]
  test_matrix_sub <- res$fit$test_matrix[idx, , drop = FALSE]

  grade8_candidates <- grep("^grade8$|grade.*8", colnames(feat_contrib), value = TRUE)
  if (length(grade8_candidates) == 0) {
    stop("Could not find a grade-8 SHAP feature column.")
  }
  grade8_col <- grade8_candidates[1]

  grade8_indicator <- as.numeric(test_matrix_sub[, grade8_col])
  keep_rows <- grade8_indicator %in% c(0, 1)
  if (sum(keep_rows, na.rm = TRUE) == 0) {
    stop("No rows found for grade split after sampling. Increase shap_sample_max or use full test rows.")
  }

  shap_grade <- as.numeric(feat_contrib[keep_rows, grade8_col])
  test_sub <- test_sub[keep_rows, , drop = FALSE]
  grade_label <- ifelse(grade8_indicator[keep_rows] == 1, "Grade 8", "Grade 4")

  split_df <- data.frame(
    shap = shap_grade,
    grade = factor(grade_label, levels = c("Grade 4", "Grade 8")),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path("output", "grade_split")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  candidate_vars <- c("median_income", "poverty_rate", "bach_or_higher_rate", "internet_rate")
  candidate_vars <- candidate_vars[candidate_vars %in% names(test_sub)]

  numeric_summary <- do.call(
    rbind,
    lapply(candidate_vars, function(v) {
      x <- test_sub[[v]]
      out_rows <- do.call(
        rbind,
        lapply(c("Grade 4", "Grade 8"), function(g) {
          in_g <- split_df$grade == g
          pos <- split_df$shap > 0 & in_g
          neg <- split_df$shap < 0 & in_g
          data.frame(
            grade = g,
            variable = v,
            n_pos = sum(pos & !is.na(x)),
            n_neg = sum(neg & !is.na(x)),
            mean_pos = mean(x[pos], na.rm = TRUE),
            mean_neg = mean(x[neg], na.rm = TRUE),
            mean_diff_pos_minus_neg = mean(x[pos], na.rm = TRUE) - mean(x[neg], na.rm = TRUE),
            median_pos = median(x[pos], na.rm = TRUE),
            median_neg = median(x[neg], na.rm = TRUE),
            median_diff_pos_minus_neg = median(x[pos], na.rm = TRUE) - median(x[neg], na.rm = TRUE),
            stringsAsFactors = FALSE
          )
        })
      )
      out_rows
    })
  )

  write.csv(
    numeric_summary,
    file.path(out_dir, "xgboost_shap_grade_split_pos_neg_numeric_summary.csv"),
    row.names = FALSE
  )

  if ("state" %in% names(test_sub)) {
    states <- as.character(test_sub$state)
    state_levels <- sort(unique(states))
    state_summary <- do.call(
      rbind,
      lapply(c("Grade 4", "Grade 8"), function(g) {
        in_g <- split_df$grade == g
        do.call(
          rbind,
          lapply(state_levels, function(st) {
            in_st <- states == st
            use <- in_g & in_st
            data.frame(
              grade = g,
              state = st,
              n = sum(use),
              pos_share = mean((split_df$shap > 0)[use], na.rm = TRUE),
              neg_share = mean((split_df$shap < 0)[use], na.rm = TRUE),
              stringsAsFactors = FALSE
            )
          })
        )
      })
    )
    state_summary <- state_summary[order(state_summary$grade, -state_summary$pos_share), ]
    write.csv(
      state_summary,
      file.path(out_dir, "xgboost_shap_grade_split_pos_neg_state_summary.csv"),
      row.names = FALSE
    )
  }

  for (v in candidate_vars) {
    plot_df <- data.frame(
      shap = split_df$shap,
      grade = split_df$grade,
      value = test_sub[[v]],
      stringsAsFactors = FALSE
    )

    p <- ggplot(plot_df, aes(x = shap, y = grade, color = value)) +
      geom_jitter(height = 0.15, width = 0, alpha = 0.75, size = 1.8) +
      geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3) +
      scale_color_gradient(low = "blue", high = "red") +
      labs(
        title = paste0("SHAP(grade8) split by grade, colored by ", v),
        subtitle = "Grade 4 is baseline (grade8 = 0); Grade 8 uses grade8 = 1",
        x = "SHAP(grade8)",
        y = NULL,
        color = v
      ) +
      theme_bw(base_size = 11)

    ggsave(
      file.path(out_dir, paste0("xgboost_shap_grade_split_jitter_color_", v, ".png")),
      p,
      width = 8.5,
      height = 5.2,
      dpi = 150
    )
  }

  if ("state" %in% names(test_sub)) {
    state_char <- as.character(test_sub$state)
    state_count <- sort(table(state_char), decreasing = TRUE)
    keep_states <- names(state_count)[seq_len(min(12L, length(state_count)))]
    state_compact <- ifelse(state_char %in% keep_states, state_char, "Other")

    state_df <- data.frame(
      shap = split_df$shap,
      grade = split_df$grade,
      state = state_compact,
      stringsAsFactors = FALSE
    )

    p_state <- ggplot(state_df, aes(x = shap, y = grade, color = state)) +
      geom_jitter(height = 0.15, width = 0, alpha = 0.75, size = 1.8) +
      geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3) +
      labs(
        title = "SHAP(grade8) split by grade, colored by state",
        subtitle = "Top states by sample size shown explicitly; others grouped",
        x = "SHAP(grade8)",
        y = NULL,
        color = "state"
      ) +
      theme_bw(base_size = 11)

    ggsave(
      file.path(out_dir, "xgboost_shap_grade_split_jitter_color_state.png"),
      p_state,
      width = 9.2,
      height = 5.6,
      dpi = 150
    )
  }

  cat("\n=== Grade split SHAP analysis (Grade 4 + Grade 8) ===\n")
  cat(sprintf("Using SHAP column: %s\n", grade8_col))
  cat(sprintf("Rows kept: %d\n", nrow(test_sub)))
  cat(sprintf("Grade 4 rows: %d\n", sum(split_df$grade == "Grade 4")))
  cat(sprintf("Grade 8 rows: %d\n", sum(split_df$grade == "Grade 8")))
  cat(sprintf("Saved: %s\n", file.path(out_dir, "xgboost_shap_grade_split_pos_neg_numeric_summary.csv")))
  if ("state" %in% names(test_sub)) {
    cat(sprintf("Saved: %s\n", file.path(out_dir, "xgboost_shap_grade_split_pos_neg_state_summary.csv")))
    cat(sprintf("Saved: %s\n", file.path(out_dir, "xgboost_shap_grade_split_jitter_color_state.png")))
  }
  for (v in candidate_vars) {
    cat(sprintf("Saved: %s\n", file.path(out_dir, paste0("xgboost_shap_grade_split_jitter_color_", v, ".png"))))
  }

  invisible(list(
    grade_feature = grade8_col,
    split_df = split_df,
    numeric_summary = numeric_summary
  ))
}

run_grade_shap_split_analysis("dataset.csv")
