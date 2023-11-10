test_that("The function works well over a regular Neural Network.", {
  skip_if_not_installed("keras")
  skip_if_not_installed("tensorflow")
  skip_on_cran()

  nn <- keras_test_model()

  # Save the parameters into a list
  params <- get_parameters(nn)

  expect_equal(params$p, 2)

  expect_equal(params$af_string_list[[1]], "tanh")
  expect_equal(params$af_string_list[[2]], "softplus")

  expect_equal(params$n_neurons[[1]], 2)
  expect_equal(params$n_neurons[[2]], 3)

  expect_equal(params$weights[[1]][[2]], 0.8773805)
  expect_equal(params$weights[[2]][[3]], 0.53210747)

  expect_equal(params$p, 2)
})

test_that("The get_parameters functions returns the right list of activation
          functions for a neural network with custom constraints.", {
  skip_if_not_installed("keras")
  skip_if_not_installed("tensorflow")
  skip_on_cran()

  testing_data <- testing_helper_2()
  nn <- keras_test_model()

  keras::compile(nn,
                 loss = "mse",
                 optimizer = keras::optimizer_adam(),
                 metrics = "mse")

  constrained_nn <- add_constraints(nn)

  fit(constrained_nn,
      testing_data$train_x,
      testing_data$train_y,
      verbose = 0,
      epochs = 3,
      validation_split = 0.2
  )

  params <- get_parameters(constrained_nn)

  expect_equal(params$af_string_list, list("tanh", "softplus", "linear"))
})

test_that("The get_parameters function works for a torch model (nn_module)
          and the list of activation functions is the expected one.", {
  skip_if_not_installed("luz")
  skip_if_not_installed("torch")
  skip_on_cran()

  nn <- luz_test_model()

  params <- get_parameters(nn)
  expect_equal(params$af_string_list, list("softplus", "softplus", "linear"))
})
