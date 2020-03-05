library(xgboost)
library(rBayesianOptimization)

train <- train_values[, c(2, 3, 4, 5, 11, 12, 33:104)]
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

xgb_cv_bayes <- function(eta, gamma, max_depth, min_child_weight, subsample) {
  
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
    "eta" = eta,
    "gamma" = gamma,
    "max_depth" = max_depth,
    "min_child_weight" = min_child_weight,
    "subsample" = subsample
  )
  
  xgcv <- xgb.cv(
    params = param_list,
    data = cv_xgb_mat,
    nrounds = 500,
    nfold = 4,
    stratified = TRUE,
    prediction = TRUE,
    showsd = TRUE,
    early_stopping_rounds = 10,
    verbose = 0,
    callbacks = list(cb.cv.predict(save_models = TRUE))
  )
  
  niter <- xgcv$niter
  last_round <- which(actual_rounds == 0)[1]
  actual_rounds[last_round] <<- niter

  validation_scores <- as.data.frame(xgcv$evaluation_log)
  train_error <- tail(validation_scores$train_merror_mean, 1)
  test_error <- tail(validation_scores$test_merror_mean, 1)
  
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
  
  list(Score = -mean_oos_error,
       Pred = xgcv$pred)
}

actual_rounds <- vector(mode = "numeric", length = 20)
opt_res <- BayesianOptimization(
  xgb_cv_bayes,
  bounds = list(
    eta = c(0.03, 0.3),
    gamma = c(0, 50),
    max_depth = c(2L, 8L),
    min_child_weight = c(1L, 200L),
    subsample = c(0.5, 1.0)
  ),
  init_grid_dt = NULL,
  init_points = 10,
  n_iter = 10,
  acq = "ucb",
  kappa = 2.576,
  eps = 0.0,
  verbose = TRUE
)

# Modify the parameters below using the Bayesian search results

param_list <- list(
  "booster" = "gbtree",
  "verbosity" = 3,
  "validate_parameters" = TRUE,
  "nthread" = 6,
  "objective" = "multi:softprob",
  "eval_metric" = "merror",
  "num_class" = 3,
  "eta" = 0.0879,
  "gamma" = 0,
  "max_depth" = 7,
  "min_child_weight" = 1,
  "subsample" = 0.7225
)
bst <- xgb.train(params = param_list, data = train_train_mat, nrounds = 219)
train_test_pred <- predict(bst, newdata = train_test_mat)
train_test_pred <- matrix(train_test_pred, nrow = 3,
                          ncol = length(train_test_pred) / 3)
train_test_pred <- data.frame(t(train_test_pred))
train_test_pred[["max_prob"]] <- max.col(train_test_pred, "last")
train_test_pred[["label"]] <- train_test_label + 1

caret::confusionMatrix(factor(train_test_pred$max_prob),
                       factor(train_test_pred$label),
                       mode = "everything")

train_train_pred <- predict(bst, newdata = train_train_mat)
train_train_pred <- matrix(train_train_pred, nrow = 3,
                           ncol = length(train_train_pred) / 3)
train_train_pred <- data.frame(t(train_train_pred))
train_train_pred[["max_prob"]] <- max.col(train_train_pred, "last")
train_train_pred[["label"]] <- train_train_label + 1

caret::confusionMatrix(factor(train_train_pred$max_prob),
                       factor(train_train_pred$label),
                       mode = "everything")

xgb.ggplot.importance(xgb.importance(model = bst))
