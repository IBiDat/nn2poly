luz_test_data <- function(example) {
  set.seed(42)

  nn2poly_dataset <- torch::dataset(
    name = "nn2poly_dataset",
    initialize = function(x, y) {
      self$x <- torch::torch_tensor(x)
      self$y <- torch::torch_tensor(y)
    },
    .getitem = function(i) {
      x <- self$x[i,]
      y <- self$y[i]
      list(x = x,
           y = y)
    },
    .length = function() {
      self$y$size()[[1]]
    }
  )

  data_full <- nn2poly_dataset(example$train_x, example$train_y)

  all_indices   <- 1:length(data_full)
  train_indices <- sample(all_indices, size = round(length(data_full)) * 0.8)
  val_indices   <- setdiff(all_indices, train_indices)

  data_train <- torch::dataset_subset(data_full, train_indices)
  data_val   <- torch::dataset_subset(data_full, val_indices)

  list(
    train = torch::dataloader(data_train, batch_size = 5, shuffle = TRUE),
    valid = torch::dataloader(data_val, batch_size = 5)
  )
}

luz_test_model <- function() {
  torch::torch_manual_seed(42)

  luz_model_sequential(
    torch::nn_linear(2,2),
    torch::nn_softplus(),
    torch::nn_linear(2,3),
    torch::nn_softplus(),
    torch::nn_linear(3,1)
  )
}
