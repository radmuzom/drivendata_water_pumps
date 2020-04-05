library(xgboost)
library(rBayesianOptimization)

train <- train_values[, c(6, 7, 8, 9, 15, 16, 19, 20, 34:177)]
set.seed(412)
train[["Random"]] <- runif(nrow(train))

train_train <- train[Random < 0.7, ]
train_test <- train[Random >= 0.7, ]

train_train_label <- as.numeric(factor(train_train$status_group)) - 1
train_test_label <- as.numeric(factor(train_test$status_group)) - 1
train_train[["Random"]] <- NULL
train_test[["Random"]] <- NULL
train_train[["status_group"]] <- NULL
train_test[["status_group"]] <- NULL

train_train_mat <- xgb.DMatrix(data = as.matrix(train_train),
                               label = train_train_label)
train_test_mat <- xgb.DMatrix(data = as.matrix(train_test),
                              label = train_test_label)

xgb_cv_bayes <- function(gamma, max_depth, min_child_weight, subsample,
                         colsample_bytree) {
  
  # Randomly choose a validation set for unseen OOS error
  nr <- nrow(train_train)
  set.seed(Sys.time())
  oos_idx <- sample(nr, floor(0.2 * nr))
  
  # Prepare the cross-validation train and OOS matrices
  cv_train_mat <- train_train[-oos_idx, ]
  cv_train_label <- train_train_label[-oos_idx]
  oos_mat <- train_train[oos_idx, ]
  oos_label <- train_train_label[oos_idx]
  
  cv_xgb_mat <- xgb.DMatrix(data = as.matrix(cv_train_mat),
                            label = cv_train_label)
  oos_xgb_mat <- xgb.DMatrix(data = as.matrix(oos_mat),
                             label = oos_label)
  
  # XGBoost parameters
  param_list <- list(
    "booster" = "gbtree",
    "verbosity" = 3,
    "validate_parameters" = TRUE,
    "nthread" = 6,
    "objective" = "multi:softprob",
    "eval_metric" = "merror",
    "num_class" = 3,
    "eta" = 0.1,
    "gamma" = gamma,
    "max_depth" = max_depth,
    "min_child_weight" = min_child_weight,
    "subsample" = subsample,
    "colsample_bytree" = colsample_bytree
  )
  
  # Run XGBoost cross-validation
  set.seed(750)
  xgcv <- xgb.cv(
    params = param_list,
    data = cv_xgb_mat,
    nrounds = 1000,
    nfold = 4,
    stratified = TRUE,
    prediction = TRUE,
    showsd = TRUE,
    early_stopping_rounds = 10,
    verbose = 0,
    callbacks = list(cb.cv.predict(save_models = TRUE))
  )
  
  # Update the actual number of rounds used
  niter <- xgcv$niter
  last_round <- which(actual_rounds == 0)[1]
  actual_rounds[last_round] <<- niter

  # Obtain train and test error from cross-validation
  validation_scores <- as.data.frame(xgcv$evaluation_log)
  train_error <- tail(validation_scores$train_merror_mean, 1)
  test_error <- tail(validation_scores$test_merror_mean, 1)
  
  # Calculate the mean out of sample error
  oos_error <- vector(mode = "numeric", length = 4)
  for (i in 1:4) {
    m <- xgcv$models[[i]]
    oos_pred <- predict(m, newdata = oos_xgb_mat)
    oos_pred <- matrix(oos_pred, nrow = 3, ncol = length(oos_pred) / 3)
    oos_pred <- data.frame(t(oos_pred))
    oos_pred[["max_prob"]] <- max.col(oos_pred, "last")
    oos_pred[["label"]] <- oos_label + 1
    oos_error[i] <- sum(oos_pred$label != oos_pred$max_prob) / length(oos_label)
  }
  mean_oos_error <- mean(oos_error)
  
  # Calculate the final validation error as the minimum of test and oos error
  val_error <- min(test_error, mean_oos_error)
  
  # Penalize overfitting
  diff_error <- abs(train_error - val_error)
  if (diff_error < 0.02) {
    score <- -train_error
  } else {
    score <- -val_error
  }
  
  list(Score = score,
       Pred = xgcv$pred)
}

actual_rounds <- vector(mode = "numeric", length = 100)
opt_res <- BayesianOptimization(
  xgb_cv_bayes,
  bounds = list(
    gamma = c(0, 50),
    max_depth = c(2L, 20L),
    min_child_weight = c(1L, 200L),
    subsample = c(0.4, 1.0),
    colsample_bytree = c(0.2, 1.0)
  ),
  init_grid_dt = NULL,
  init_points = 40,
  n_iter = 40,
  acq = "ucb",
  kappa = 2.576,
  eps = 0.0,
  verbose = TRUE
)

# Modify the parameters below using the Bayesian search results

set.seed(750)
actual_rounds[57] # Modify for actual rounds used for best iteration
param_list <- list(
  "booster" = "gbtree",
  "verbosity" = 3,
  "validate_parameters" = TRUE,
  "nthread" = 6,
  "objective" = "multi:softprob",
  "eval_metric" = "merror",
  "num_class" = 3,
  "eta" = 0.1,
  "gamma" = 0,
  "max_depth" = 20,
  "min_child_weight" = 4,
  "subsample" = 1,
  "colsample_bytree" = 0.2
)
bst <- xgb.train(params = param_list, data = train_train_mat, nrounds = 107)
train_test_pred <- predict(bst, newdata = train_test_mat)
train_test_pred <- matrix(train_test_pred, nrow = 3,
                          ncol = length(train_test_pred) / 3)
train_test_pred <- data.frame(t(train_test_pred))
train_test_pred[["max_prob"]] <- max.col(train_test_pred, "last")
train_test_pred[["label"]] <- train_test_label + 1

caret::confusionMatrix(factor(train_test_pred$max_prob),
                       factor(train_test_pred$label),
                       mode = "everything")

# Test Accuracy - 0.8132

train_train_pred <- predict(bst, newdata = train_train_mat)
train_train_pred <- matrix(train_train_pred, nrow = 3,
                           ncol = length(train_train_pred) / 3)
train_train_pred <- data.frame(t(train_train_pred))
train_train_pred[["max_prob"]] <- max.col(train_train_pred, "last")
train_train_pred[["label"]] <- train_train_label + 1

caret::confusionMatrix(factor(train_train_pred$max_prob),
                       factor(train_train_pred$label),
                       mode = "everything")

# Train Accuracy - 0.9593

xgb.ggplot.importance(xgb.importance(model = bst))

final_test_pred <- predict(bst, as.matrix(
  test_values[, c(6, 7, 8, 9, 15, 16, 19, 20, 34:176)]
))
final_test_pred <- matrix(final_test_pred, nrow = 3,
                           ncol = length(final_test_pred) / 3)
final_test_pred <- data.frame(t(final_test_pred))
final_test_pred[["max_prob"]] <- max.col(final_test_pred, "last")
final_test_pred[["id"]] <- test_values$id
final_test_pred[["status_group"]] <- "functional"
final_test_pred[final_test_pred$max_prob == 2, "status_group"] <-
  "functional needs repair"
final_test_pred[final_test_pred$max_prob == 3, "status_group"] <-
  "non functional"

sf <- fread("SubmissionFormat.csv")
sf[["status_group"]] <- NULL
sf <- merge(sf, final_test_pred[, c("id", "status_group")],
            by = "id", sort = FALSE)
fwrite(sf, "Submission_20200319.csv")
