library(xgboost)

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

search_grid <- expand.grid(
  eta = seq(0.03, 0.3, by = 0.03),
  gamma = c(0, 1, 2, 3, 4, 5),
  max_depth = c(3, 4, 5, 6, 7, 8),
  min_child_weight = seq(25, 200, by = 25),
  subsample = seq(0.4, 1, by = 0.1)
)

# setDT(search_grid)
# set.seed(NULL)
# search_grid <- search_grid[sample(.N, 20), ]

hp_search <- apply(search_grid, 1, function(param_list) {
  c_eta <- param_list[["eta"]]
  c_gamma <- param_list[["gamma"]]
  c_max_depth <- param_list[["max_depth"]]
  c_min_child_weight <- param_list[["min_child_weight"]]
  c_subsample <- param_list[["subsample"]]
  
  param_list <- list(
    "booster" = "gbtree",
    "verbosity" = 3,
    "validate_parameters" = TRUE,
    "nthread" = 8,
    "objective" = "multi:softprob",
    "eval_metric" = "merror",
    "num_class" = 3,
    "eta" = c_eta,
    "gamma" = c_gamma,
    "max_depth" = c_max_depth,
    "min_child_weight" = c_min_child_weight,
    "subsample" = c_subsample
  )
  
  xgcv <- xgb.cv(
    params = param_list,
    data = train_train_mat,
    nrounds = 300,
    early_stopping_rounds = 10,
    nfold = 5
  )
  
  validation_scores <- as.data.frame(xgcv$evaluation_log)
  train_error <- tail(validation_scores$train_merror_mean, 1)
  test_error <- tail(validation_scores$test_merror_mean, 1)
  c(c_eta, c_gamma, c_max_depth, c_min_child_weight, c_subsample,
    train_error, test_error)
})

hp_search <- as.data.frame(t(hp_search))
colnames(hp_search) <- c("eta", "gamma", "max_depth", "min_child_weight",
                         "subsample", "train_error", "test_error")
hp_search <- hp_search[order(hp_search$test_error), ]

param_list <- list(
  "booster" = "gbtree",
  "verbosity" = 3,
  "validate_parameters" = TRUE,
  "nthread" = 8,
  "objective" = "multi:softprob",
  "eval_metric" = "merror",
  "num_class" = 3,
  "eta" = hp_search[1, 1],
  "gamma" = hp_search[1, 2],
  "max_depth" = hp_search[1, 3],
  "min_child_weight" = hp_search[1, 4],
  "subsample" = hp_search[1, 5]
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
