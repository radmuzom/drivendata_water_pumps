library(lightgbm)
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

train_train_mat <- lgb.Dataset(as.matrix(train_train),
                               label = train_train_label)

lgb_search_bayes <- function(eta, max_depth, min_data_in_leaf, num_leaves,
                         bagging_fraction, feature_fraction, nrounds) {
  
  nrounds <- trunc(nrounds)
  
  # Randomly choose a validation set for unseen OOS error
  nr <- nrow(train_train)
  set.seed(Sys.time())
  oos_idx <- sample(nr, floor(0.2 * nr))
  
  # Prepare the cross-validation train and OOS matrices
  val_train_mat <- train_train[-oos_idx, ]
  val_train_label <- train_train_label[-oos_idx]
  oos_mat <- train_train[oos_idx, ]
  oos_label <- train_train_label[oos_idx]
  
  val_lgb_mat <- lgb.Dataset(as.matrix(val_train_mat), label = val_train_label)
  
  # LGBM parameters
  param_list <- list(
    "device_type" = "gpu",
    "boosting" = "gbdt",
    "num_threads" = 6,
    "objective" = "multiclass",
    "metric" = "multi_error",
    "num_class" = 3,
    "learning_rate" = eta,
    "max_depth" = max_depth,
    "min_data_in_leaf" = min_data_in_leaf,
    "num_leaves" = num_leaves,
    "bagging_fraction" = bagging_fraction,
    "feature_fraction" = feature_fraction,
    "num_iterations" = nrounds
  )
  
  # Run LGBM training
  set.seed(750)
  lgt <- lgb.train(
    params = param_list,
    data = val_lgb_mat,
    verbose = -1
  )
  
  # Predictions on val
  val_train_pred <- predict(lgt, as.matrix(val_train_mat))
  val_train_pred <- matrix(val_train_pred, nrow = 3,
                           ncol = length(val_train_pred) / 3)
  val_train_pred <- data.frame(t(val_train_pred))
  val_train_pred[["max_prob"]] <- max.col(val_train_pred, "last")
  val_train_pred[["label"]] <- val_train_label + 1
  val_train_error <-
    sum(val_train_pred$max_prob != val_train_pred$label)/ nrow(val_train_pred)
  
  # Predictions on oos
  oos_pred <- predict(lgt, as.matrix(oos_mat))
  oos_pred <- matrix(oos_pred, nrow = 3,
                     ncol = length(oos_pred) / 3)
  oos_pred <- data.frame(t(oos_pred))
  oos_pred[["max_prob"]] <- max.col(oos_pred, "last")
  oos_pred[["label"]] <- oos_label + 1
  oos_error <-
    sum(oos_pred$max_prob != oos_pred$label)/ nrow(oos_pred)
  
  # Penalize overfitting
  diff_error <- abs(val_train_error - oos_error)
  if (diff_error < 0.02) {
   score <- -val_train_error
  } else {
   score <- -oos_error
  }
  
  list(Score = score,
      Pred = NULL)
}

opt_res <- BayesianOptimization(
  lgb_search_bayes,
  bounds = list(
    eta = c(0.005, 0.1),
    max_depth = c(2L, 20L),
    min_data_in_leaf = c(5L, 100L),
    num_leaves = c(10L, 100L),
    bagging_fraction = c(0.4, 1.0),
    feature_fraction = c(0.2, 1.0),
    nrounds = c(50L, 1000L)
  ),
  init_grid_dt = NULL,
  init_points = 50,
  n_iter = 50,
  acq = "ucb",
  kappa = 2.576,
  eps = 0.0,
  verbose = TRUE
)

set.seed(750)
param_list <- list(
  "device_type" = "gpu",
  "boosting" = "gbdt",
  "num_threads" = 6,
  "objective" = "multiclass",
  "metric" = "multi_error",
  "num_class" = 3,
  "learning_rate" = 0.1,
  "max_depth" = 20,
  "min_data_in_leaf" = 12,
  "num_leaves" = 50,
  "bagging_fraction" = 0.6606,
  "feature_fraction" = 0.2,
  "num_iterations" = 1000
)
bst <- lgb.train(
  params = param_list,
  data = train_train_mat,
  verbose = -1
)

train_test_pred <- predict(bst, as.matrix(train_test))
train_test_pred <- matrix(train_test_pred, nrow = 3,
                          ncol = length(train_test_pred) / 3)
train_test_pred <- data.frame(t(train_test_pred))
train_test_pred[["max_prob"]] <- max.col(train_test_pred, "last")
train_test_pred[["label"]] <- train_test_label + 1

caret::confusionMatrix(factor(train_test_pred$max_prob),
                       factor(train_test_pred$label),
                       mode = "everything")

# Test Accuracy - 0.8108

train_train_pred <- predict(bst, as.matrix(train_train))
train_train_pred <- matrix(train_train_pred, nrow = 3,
                           ncol = length(train_train_pred) / 3)
train_train_pred <- data.frame(t(train_train_pred))
train_train_pred[["max_prob"]] <- max.col(train_train_pred, "last")
train_train_pred[["label"]] <- train_train_label + 1

caret::confusionMatrix(factor(train_train_pred$max_prob),
                       factor(train_train_pred$label),
                       mode = "everything")

# Train Accuracy - 0.9242

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
fwrite(sf, "Submissions/Submission_20200405_v2.csv")