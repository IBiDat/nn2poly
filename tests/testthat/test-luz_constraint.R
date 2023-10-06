test_that("The constranied training works using the l1 and l2 constraints", {
  nn2poly_dataset <- torch::dataset(
    name = "nn2poly_dataset",

    initialize = function(df) {
      self$x <- torch::torch_tensor(as.matrix(df[,1:2]))
      self$y <- torch::torch_tensor(as.matrix(df[,3]))
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

  example    <- nn2poly_example0
  data_train_full <- nn2poly_dataset(as.data.frame(cbind(example$train_x, example$train_y)))

  all_indices   <- 1:length(data_train_full)
  train_indices <- sample(all_indices, size = round(length(data_train_full)) * 0.8)
  val_indices   <- setdiff(all_indices, train_indices)

  data_train <- torch::dataset_subset(data_train_full, train_indices)
  data_val   <- torch::dataset_subset(data_train_full, val_indices)

  train_dl <- torch::dataloader(data_train, batch_size = 32, shuffle = TRUE)
  val_dl   <- torch::dataloader(data_val, batch_size = 32)

  net <- torch::nn_module(
    "my_network",

    initialize = function() {
      self$linear1  <- torch::nn_linear(2,2)
      self$linear2  <- torch::nn_linear(2,3)
      self$output   <- torch::nn_linear(3,1)
      self$softplus <- torch::nn_softplus()
    },

    forward = function(x) {
      x %>%
        self$linear1() %>%
        self$softplus() %>%
        self$linear2() %>%
        self$softplus() %>%
        self$output()
    }
  )

  # Do the constrained training with the l1 constraint
  fitted <- net %>%
    luz::setup(
      loss = torch::nn_mse_loss(),
      optimizer = torch::optim_adam,
    ) %>%
    luz::fit(train_dl,
             epochs = 3,
             valid_data = val_dl,
             callbacks = list(
               luz_constraint(constraint_type = "l1")
             )
    )

  wb <- torch::torch_tensor(
    rbind(
      t(as.matrix(fitted$model$children$linear1[["bias"]])),
      t(as.matrix(fitted$model$children$linear1[["weight"]]))
    ),
    requires_grad = TRUE
  )

  # Check that all l1-norms are <= 1
  expect_true(
    all(torch::as_array(torch::linalg_vector_norm(wb, dim = 1, ord = 1)) <= 1)
  )

  # Now, the same but with the l2 constraint

  # Do the constrainted training with the l2 constraint
  fitted <- net %>%
    luz::setup(
      loss = torch::nn_mse_loss(),
      optimizer = torch::optim_adam,
    ) %>%
    luz::fit(train_dl,
             epochs = 3,
             valid_data = val_dl,
             callbacks = list(
               luz_constraint(constraint_type = "l2")
             )
    )

  wb <- torch::torch_tensor(
    rbind(
      t(as.matrix(fitted$model$children$linear1[["bias"]])),
      t(as.matrix(fitted$model$children$linear1[["weight"]]))
    ),
    requires_grad = TRUE
  )

  # Check that all l2-norms are <= 1
  expect_true(
    all(torch::as_array(torch::linalg_vector_norm(wb, dim = 1, ord = 2)) <= 1)
  )

})

test_that("For a non valid constraint, the l1 constraint is applied", {
  nn2poly_dataset <- torch::dataset(
    name = "nn2poly_dataset",

    initialize = function(df) {
      self$x <- torch::torch_tensor(as.matrix(df[,1:2]))
      self$y <- torch::torch_tensor(as.matrix(df[,3]))
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

  example    <- nn2poly_example0
  data_train_full <- nn2poly_dataset(as.data.frame(cbind(example$train_x, example$train_y)))

  all_indices   <- 1:length(data_train_full)
  train_indices <- sample(all_indices, size = round(length(data_train_full)) * 0.8)
  val_indices   <- setdiff(all_indices, train_indices)

  data_train <- torch::dataset_subset(data_train_full, train_indices)
  data_val   <- torch::dataset_subset(data_train_full, val_indices)

  train_dl <- torch::dataloader(data_train, batch_size = 32, shuffle = TRUE)
  val_dl   <- torch::dataloader(data_val, batch_size = 32)

  net <- torch::nn_module(
    "my_network",

    initialize = function() {
      self$linear1  <- torch::nn_linear(2,2)
      self$linear2  <- torch::nn_linear(2,3)
      self$output   <- torch::nn_linear(3,1)
      self$softplus <- torch::nn_softplus()
    },

    forward = function(x) {
      x %>%
        self$linear1() %>%
        self$softplus() %>%
        self$linear2() %>%
        self$softplus() %>%
        self$output()
    }
  )

  # Do the constrainted training with a non valid constraint
  fitted <- net %>%
    luz::setup(
      loss = torch::nn_mse_loss(),
      optimizer = torch::optim_adam,
    ) %>%
    luz::fit(train_dl,
             epochs = 3,
             valid_data = val_dl,
             callbacks = list(
               luz_constraint(constraint_type = "Clearly, a non valid constraint")
             )
    )

  wb <- torch::torch_tensor(
    rbind(
      t(as.matrix(fitted$model$children$linear1[["bias"]])),
      t(as.matrix(fitted$model$children$linear1[["weight"]]))
    ),
    requires_grad = TRUE
  )

  # Check that all l1-norms are <= 1
  expect_true(
    all(torch::as_array(torch::linalg_vector_norm(wb, dim = 1, ord = 1)) <= 1)
  )
})
