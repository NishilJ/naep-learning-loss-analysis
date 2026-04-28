required_pkgs <- c("xgboost", "Matrix")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(xgboost)
library(Matrix)

run_xgboost_score_by_grade <- function(path = "dataset.csv", grades = c(4, 8)) {
  df <- read.csv(path, stringsAsFactors = FALSE)

  keep_cols <- c(
    "state", "year", "grade", "score",
    "median_income", "poverty_rate", "unemployment_rate",
    "pct_black", "pct_hispanic", "bach_or_higher_rate",
    "no_computer_rate", "internet_rate"
  )

  missing_cols <- setdiff(keep_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  model_df <- df[, keep_cols]
  model_df$state <- as.factor(model_df$state)
  model_df$year_scaled <- model_df$year - min(model_df$year)

  grade_labels <- as.character(model_df$grade)
  requested_grades <- as.character(grades)

  fixed_params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.05,
    max_depth = 3L,
    min_child_weight = 3,
    subsample = 0.8,
    colsample_bytree = 0.8,
    lambda = 1,
    alpha = 0
  )

  eta_vals <- c(0.03, 0.05, 0.1)
  depth_vals <- c(3L, 4L, 6L)
  max_nrounds <- 1000L
  early_stopping_rounds <- 20L

  results <- list()

  for (grade_value in requested_grades) {
    grade_df <- model_df[grade_labels == grade_value, ]

    if (nrow(grade_df) == 0) {
      warning(sprintf("Skipping grade %s: no rows found.", grade_value))
      next
    }

    train_df <- grade_df[grade_df$year < 2020, ]
    test_df <- grade_df[grade_df$year >= 2020, ]

    if (nrow(train_df) == 0 || nrow(test_df) == 0) {
      warning(sprintf("Skipping grade %s: need both pre-2020 and post-2020 rows.", grade_value))
      next
    }

    train_years <- sort(unique(train_df$year))
    if (length(train_years) < 3) {
      warning(sprintf("Skipping grade %s: need at least 3 distinct pre-2020 years.", grade_value))
      next
    }

    numeric_features <- c(
      "median_income", "poverty_rate", "unemployment_rate",
      "pct_black", "pct_hispanic", "bach_or_higher_rate",
      "no_computer_rate", "internet_rate", "year_scaled"
    )

    medians <- sapply(train_df[, numeric_features, drop = FALSE], function(x) median(x, na.rm = TRUE))
    for (col in numeric_features) {
      train_df[[col]][is.na(train_df[[col]])] <- medians[[col]]
      test_df[[col]][is.na(test_df[[col]])] <- medians[[col]]
    }

    train_matrix <- sparse.model.matrix(score ~ . - year - grade - 1, data = train_df)
    test_matrix <- sparse.model.matrix(score ~ . - year - grade - 1, data = test_df)
    train_label <- train_df$score
    test_label <- test_df$score

    dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
    dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

    rolling_folds <- lapply(2:length(train_years), function(i) {
      list(
        train_idx = which(train_df$year <= train_years[i - 1]),
        val_idx = which(train_df$year == train_years[i])
      )
    })

    evaluate_rolling_validation <- function(params) {
      fold_rmse <- numeric(length(rolling_folds))
      fold_best_nrounds <- integer(length(rolling_folds))

      for (i in seq_along(rolling_folds)) {
        fold <- rolling_folds[[i]]
        dtrain_fold <- xgb.DMatrix(
          data = train_matrix[fold$train_idx, , drop = FALSE],
          label = train_label[fold$train_idx]
        )
        dval_fold <- xgb.DMatrix(
          data = train_matrix[fold$val_idx, , drop = FALSE],
          label = train_label[fold$val_idx]
        )

        model <- xgb.train(
          params = params,
          data = dtrain_fold,
          nrounds = max_nrounds,
          evals = list(train = dtrain_fold, eval = dval_fold),
          early_stopping_rounds = early_stopping_rounds,
          verbose = 0
        )

        val_pred <- predict(model, dval_fold)
        val_true <- train_label[fold$val_idx]
        fold_rmse[i] <- sqrt(mean((val_true - val_pred)^2))
        fold_best_nrounds[i] <- if (!is.null(model$best_iteration) && !is.na(model$best_iteration)) {
          as.integer(model$best_iteration)
        } else {
          max_nrounds
        }
      }

      list(
        mean_rmse = mean(fold_rmse),
        nrounds = max(1L, as.integer(round(mean(fold_best_nrounds))))
      )
    }

    cat(sprintf("\n=== Grade %s: rolling validation grid search ===\n", grade_value))
    best_rmse <- Inf
    best_params <- NULL
    best_results <- NULL

    for (eta in eta_vals) {
      for (depth in depth_vals) {
        params <- fixed_params
        params$eta <- eta
        params$max_depth <- depth

        result <- evaluate_rolling_validation(params)
        cat(sprintf("grade=%s, eta=%.2f, max_depth=%d -> RMSE: %.4f\n", grade_value, eta, depth, result$mean_rmse))

        if (result$mean_rmse < best_rmse) {
          best_rmse <- result$mean_rmse
          best_params <- params
          best_results <- result
        }
      }
    }

    cat(sprintf("Best rolling validation RMSE (grade %s): %.4f\n", grade_value, best_results$mean_rmse))
    cat(sprintf("Using early-stopped nrounds (grade %s): %d\n", grade_value, best_results$nrounds))
    cat("Best parameters:\n")
    print(best_params)

    final_model <- xgb.train(
      params = best_params,
      data = dtrain,
      nrounds = best_results$nrounds,
      evals = list(train = dtrain, test = dtest),
      verbose = 0
    )

    preds <- predict(final_model, dtest)
    rmse <- sqrt(mean((test_label - preds)^2))
    mae <- mean(abs(test_label - preds))
    r2 <- 1 - sum((test_label - preds)^2) / sum((test_label - mean(test_label))^2)
    train_mean <- mean(train_label)
    r2_alt <- 1 - sum((test_label - preds)^2) / sum((test_label - train_mean)^2)

    cat(sprintf("\n=== Grade %s: Post-2020 Test Performance ===\n", grade_value))
    cat(sprintf("RMSE: %.4f\n", rmse))
    cat(sprintf("MAE : %.4f\n", mae))
    cat(sprintf("R2  : %.4f\n", r2))
    cat(sprintf("R2 alt : %.4f\n", r2_alt))

    cat(sprintf("\nTop 15 feature importances (grade %s):\n", grade_value))
    importance <- xgb.importance(model = final_model, feature_names = colnames(train_matrix))
    print(head(importance, 15))

    results[[paste0("grade_", grade_value)]] <- list(
      model = final_model,
      best_params = best_params,
      rolling_validation = best_results,
      metrics = list(rmse = rmse, mae = mae, r2 = r2),
      predictions = data.frame(
        state = test_df$state,
        year = test_df$year,
        actual_score = test_label,
        predicted_score = preds
      )
    )
  }

  if (length(results) == 0) {
    stop("No grade-specific models were trained. Check grade values and data availability.")
  }

  invisible(results)
}

run_xgboost_score_by_grade("dataset.csv", grades = c(4, 8))
