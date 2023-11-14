test_that("The function works as expected", {
  skip_if_not_installed("keras")
  skip_if_not_installed("tensorflow")
  skip()
  skip_on_cran()
  skip_on_covr()
  skip_on_ci()

  testing_data <- testing_helper_2()

  nn <- keras_test_model()

  # compile the model
  keras::compile(nn,
                 loss = "mse",
                 optimizer = keras::optimizer_adam(),
                 metrics = "mse")

  # train the model
  keras::fit(nn,
             testing_data$train_x,
             testing_data$train_y,
             verbose = 0,
             epochs = 30,
             validation_split = 0.3
  )

  # compute the different plots
  plots <- plot_taylor_and_activation_potentials(
    object = nn,
    data = cbind(testing_data$train_x, testing_data$train_y),
    taylor_orders = testing_data$taylor_orders,
    max_order = 3,
    constraints = FALSE)

  # test the different plots
  vdiffr::expect_doppelganger("layer1", plots[[1]])
  vdiffr::expect_doppelganger("layer2", plots[[2]])
  vdiffr::expect_doppelganger("layer3", plots[[3]])

})

test_that("tests that the function to which it has been transformed is correct",{

  # List with the four possible functions
  af_string_list   <- list("softplus", "tanh", "sigmoid", "linear")
  # List with the converted strings to functions
  af_function_list <- change_string_to_function(af_string_list)

  # softplus
  expect_equal(
    deparse(af_function_list[[1]]),
    deparse(function(x) log(1 + exp(x)))
  )

  # tanh
  expect_equal(
    deparse(af_function_list[[2]]),
    deparse(function(x) tanh(x))
  )

  # sigmoid
  expect_equal(
    deparse(af_function_list[[3]]),
    deparse(function(x) 1 / (1 + exp(-x)))
  )

  # linear
  expect_equal(
    deparse(af_function_list[[4]]),
    deparse(function(x) x)
  )

})


test_that("test that the value of  each function is the expected for
          some random generated numbers between -1 and 1",{

  # List with the four possible functions
  af_string_list   <- list("softplus", "tanh", "sigmoid", "linear")
  # List with the converted strings to functions
  af_function_list <- change_string_to_function(af_string_list)

  # Generating 1000 random numbers between -1 and 1
  random_numbers <- runif(1000, -1, 1)

  # softplus
  expect_equal(
    af_function_list[[1]](random_numbers),
    sapply(random_numbers, function(x) log(1 + exp(x)))
  )

  # tanh
  expect_equal(
    af_function_list[[2]](random_numbers),
    sapply(random_numbers, function(x) tanh(x))
  )

  # sigmoid
  expect_equal(
    af_function_list[[3]](random_numbers),
    sapply(random_numbers, function(x) 1 / (1 + exp(-x)))
  )

  # linear
  expect_equal(
    af_function_list[[4]](random_numbers),
    sapply(random_numbers, function(x) x)
  )

})
