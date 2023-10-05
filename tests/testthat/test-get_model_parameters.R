test_that("The function works well over a regular Neural Network.", {
  # Skip on cran and on github actions
  skip_on_cran()

  # Setting the seeds for reproducibility purposes
  tensorflow::set_random_seed(42)

  nn <- keras::keras_model_sequential()
  keras::`%>%`(nn, keras::layer_dense(units = 2,
                                      activation = "tanh",
                                      input_shape = 2))
  keras::`%>%`(nn, keras::layer_dense(units = 3,
                                      activation = "softplus"))
  keras::`%>%`(nn, keras::layer_dense(units = 2,
                                      activation = "linear"))

  # Save the parameters into a list
  params <- get_model_parameters(nn)

  expect_equal(params$p, 2)

  expect_equal(params$af_string_list[[1]], "tanh")
  expect_equal(params$af_string_list[[2]], "softplus")

  expect_equal(params$n_neurons[[1]], 2)
  expect_equal(params$n_neurons[[2]], 3)

  expect_equal(params$weights[[1]][[2]], 0.8773805)
  expect_equal(params$weights[[2]][[3]], 0.53210747)

  expect_equal(params$p, 2)

})

test_that("The get_model_parameters functions returns the right list of activation
          functions for a neural network with custom constraints.", {
  # Skip on cran and on github actions
  skip_on_cran()

  # Setting the seeds for reproducibility purposes
  tensorflow::set_random_seed(42)

  nn <- keras::keras_model_sequential()
  keras::`%>%`(nn, keras::layer_dense(units = 2,
                                      activation = "tanh",
                                      input_shape = 2))
  keras::`%>%`(nn, keras::layer_dense(units = 3,
                                      activation = "softplus"))
  keras::`%>%`(nn, keras::layer_dense(units = 2,
                                      activation = "linear"))

  constrained_nn <- add_constraints(nn)
  params <- get_model_parameters(constrained_nn)

  expect_equal(params$af_string_list, list("tanh", "softplus", "linear"))


})

test_that("The get_model_parameters function works for a torch model (nn_module)
          and the list of activation functions is the expected one.", {
  # Skip on cran and on github actions
  skip_on_cran()

  set.seed(42)
  torch::torch_manual_seed(42)

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

  # data_test  <- nn2poly_dataset(as.data.frame(cbind(example$test_x, example$test_y)))
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

  fitted <- net %>%
    luz::setup(
      loss = torch::nn_mse_loss(),
      optimizer = torch::optim_adam,
      metrics = list(
        luz::luz_metric_mse()
      )
    ) %>%
    luz::fit(train_dl, epochs = 5, valid_data = val_dl)

  params <- get_model_parameters(fitted$model)
  expect_equal(params$af_string_list, nn2poly_example0$af_string_list)

})
