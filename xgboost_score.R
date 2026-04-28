required_pkgs <- c("xgboost", "Matrix")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(xgboost)
library(Matrix)

run_xgboost_score <- function(path = "dataset.csv") {
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
  model_df$grade <- as.factor(model_df$grade)

  train_df <- model_df[model_df$year < 2020, ]
  test_df <- model_df[model_df$year >= 2020, ]

  if (nrow(train_df) == 0 || nrow(test_df) == 0) {
    stop("Need both pre-2020 training rows and post-2020 testing rows.")
  }

  numeric_features <- c(
    "median_income", "poverty_rate", "unemployment_rate",
    "pct_black", "pct_hispanic", "bach_or_higher_rate",
    "no_computer_rate", "internet_rate"
  )

  # Impute missing numeric values using training medians only.
  medians <- sapply(train_df[, numeric_features, drop = FALSE], function(x) median(x, na.rm = TRUE))
  for (col in numeric_features) {
    train_df[[col]][is.na(train_df[[col]])] <- medians[[col]]
    test_df[[col]][is.na(test_df[[col]])] <- medians[[col]]
  }

  train_matrix <- sparse.model.matrix(score ~ . - year - 1, data = train_df)
  test_matrix <- sparse.model.matrix(score ~ . - year - 1, data = test_df)
  train_label <- train_df$score
  test_label <- test_df$score

  dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
  dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

  train_years <- sort(unique(train_df$year))
  if (length(train_years) < 3) {
    stop("Need at least 3 distinct pre-2020 years for rolling validation.")
  }

  rolling_folds <- lapply(2:length(train_years), function(i) {
    list(
      train_idx = which(train_df$year <= train_years[i - 1]),
      val_idx = which(train_df$year == train_years[i])
    )
  })

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
  max_nrounds <- 1000L
  early_stopping_rounds <- 20L

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

      # Compute validation RMSE directly from predictions to avoid
      # version-specific metadata differences in xgboost objects.
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

  eta_vals <- c(0.03, 0.05, 0.1)
  depth_vals <- c(4L, 6L, 8L)

  best_rmse <- Inf
  best_params <- NULL
  best_results <- NULL

  cat("Running rolling validation grid search...\n")
  for (eta in eta_vals) {
    for (depth in depth_vals) {
      params <- fixed_params
      params$eta <- eta
      params$max_depth <- depth

      result <- evaluate_rolling_validation(params)
      cat(sprintf("eta=%.2f, max_depth=%d -> RMSE: %.4f\n", eta, depth, result$mean_rmse))

      if (result$mean_rmse < best_rmse) {
        best_rmse <- result$mean_rmse
        best_params <- params
        best_results <- result
      }
    }
  }

  rolling_results <- best_results
  cat(sprintf("Best rolling validation RMSE: %.4f\n", rolling_results$mean_rmse))
  cat(sprintf("Using early-stopped nrounds: %d\n", rolling_results$nrounds))
  cat("Best parameters:\n")
  print(best_params)

  final_model <- xgb.train(
    params = best_params,
    data = dtrain,
    nrounds = rolling_results$nrounds,
    evals = list(train = dtrain, test = dtest),
    verbose = 0
  )

  preds <- predict(final_model, dtest)

  rmse <- sqrt(mean((test_label - preds)^2))
  mae <- mean(abs(test_label - preds))
  r2 <- 1 - sum((test_label - preds)^2) / sum((test_label - mean(test_label))^2)

  cat("\n=== Post-2020 Test Performance ===\n")
  cat(sprintf("RMSE: %.4f\n", rmse))
  cat(sprintf("MAE : %.4f\n", mae))
  cat(sprintf("R2  : %.4f\n", r2))

  cat("\nTop 15 feature importances:\n")
  importance <- xgb.importance(model = final_model, feature_names = colnames(train_matrix))
  print(head(importance, 15))

  invisible(list(
    model = final_model,
    best_params = best_params,
    rolling_validation = rolling_results,
    metrics = list(rmse = rmse, mae = mae, r2 = r2),
    predictions = data.frame(
      state = test_df$state,
      year = test_df$year,
      grade = test_df$grade,
      actual_score = test_label,
      predicted_score = preds
    )
  ))
}

run_xgboost_score("dataset.csv")
 