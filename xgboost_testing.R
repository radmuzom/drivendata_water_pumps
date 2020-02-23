library(xgboost)
library(rBayesianOptimization)

train <- train_values[, c(1, 2, 33:70)]
train[["Random"]] <- runif(nrow(train))

train_train <- train[Random < 0.7, ]
train_test <- train[Random >= 0.7, ]

train_train_label <- as.numeric(factor(train_train$status_group)) - 1
train_test_label <- as.numeric(factor(train_test$status_group)) - 1
train_train <- train_train[, -c(3, 41)]
train_test <- train_test[, -c(3, 41)]

train_train_mat <- xgb.DMatrix(data = as.matrix(train_train),
                               label = train_train_label)
train_test_mat <- xgb.DMatrix(data = as.matrix(train_test),
                              label = train_test_label)

cv_folds <- KFold(train_train_label, nfolds = 5, stratified = TRUE, seed = 0)
xgb_cv_bayes <- function(eta, gamma, max_depth,
                         min_child_weight, subsample, nrounds) {
  param_list <- list(
    "booster" = "gbtree",
    "verbosity" = 3,
    "validate_parameters" = TRUE,
    "nthread" = 8,
    "objective" = "multi:softprob",
    "eval_metric" = "merror",
    "num_class" = 3,
    "nrounds" = nrounds,
    "eta" = eta,
    "gamma" = gamma,
    "max_depth" = max_depth,
    "min_child_weight" = min_child_weight,
    "subsample" = subsample
  )
  
  xgcv <- xgb.cv(
    params = param_list,
    data = train_train_mat,
    nround = 1000,
    folds = cv_folds,
    prediction = TRUE,
    showsd = TRUE,
    early_stopping_rounds = 10,
    verbose = 0
  )

  validation_scores <- as.data.frame(xgcv$evaluation_log)
  train_error <- tail(validation_scores$train_merror_mean, 1)
  test_error <- tail(validation_scores$test_merror_mean, 1)
  list(Score = -test_error,
       Pred = xgcv$pred)
}

opt_res <- BayesianOptimization(
  xgb_cv_bayes,
  bounds = list(
    eta = c(0.03, 0.3),
    gamma = c(0, 50),
    max_depth = c(2L, 8L),
    min_child_weight = c(25L, 200L),
    subsample = c(0.5, 0.9),
    nrounds = c(50L, 1000L)
  ),
  init_grid_dt = NULL,
  init_points = 10,
  n_iter = 100,
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
  "nthread" = 8,
  "objective" = "multi:softprob",
  "eval_metric" = "merror",
  "num_class" = 3,
  "eta" = 0.3,
  "gamma" = 0,
  "max_depth" = 8,
  "min_child_weight" = 25,
  "subsample" = 0.9
)
bst <- xgb.train(params = param_list, data = train_train_mat, nrounds = 300)
test_pred <- predict(bst, newdata = train_test_mat)
test_pred <- matrix(test_pred, nrow = 3, ncol = length(test_pred) / 3)
test_pred <- data.frame(t(test_pred))
test_pred[["max_prob"]] <- max.col(test_pred, "last")
test_pred[["label"]] <- train_test_label + 1

caret::confusionMatrix(factor(test_pred$max_prob),
                       factor(test_pred$label),
                       mode = "everything")

train_pred <- predict(bst, newdata = train_train_mat)
train_pred <- matrix(train_pred, nrow = 3, ncol = length(train_pred) / 3)
train_pred <- data.frame(t(train_pred))
train_pred[["max_prob"]] <- max.col(train_pred, "last")
train_pred[["label"]] <- train_train_label + 1

caret::confusionMatrix(factor(train_pred$max_prob),
                       factor(train_pred$label),
                       mode = "everything")

xgb.ggplot.importance(xgb.importance(model = bst))
