#Note that other options are tested in nn2poly.

test_that("nn2poly_algorithm against precomputed example", {
  # Load the example:
  testing_data <- testing_helper_1()

  # Get the needed data
  weights_list <- testing_data$weights_list
  af_string_list <- testing_data$af_string_list
  taylor_orders <- testing_data$taylor_orders

  result <- nn2poly_algorithm(
    weights_list,
    af_string_list,
    max_order = 3,
    keep_layers = TRUE,
    taylor_orders = taylor_orders
  )

  n_layers <- length(result)
  n_terms <- length(result[[n_layers]]$output$labels)
  order <- length(result[[n_layers]]$output$labels[[n_terms]])
  expect_equal(order, 3)

  # Desired coefficient in output polynomial at layer 2,
  # neuron 1, coefficient "1,1"
  label <- result$layer_2$output$labels[[4]]
  coeff <- result$layer_2$output$values[4, 1]
  expect_equal(label, c(1, 1))
  expect_equal(coeff, 0.63351833, tolerance = 1e-6)

})

test_that("Taylor vector obtained with single value and mutiple linear layers", {
  af_string_list <- c("softplus", "linear", "softplus", "linear")
  taylor_orders <- 5L
  output <- obtain_taylor_vector(taylor_orders, af_string_list)
  expect_equal(output, c(5,1,5,1))
})

test_that("Taylor vector obtained with vector", {
  af_string_list <- c("softplus", "softplus", "linear")
  taylor_orders <- c(5,5,1)
  output <- obtain_taylor_vector(taylor_orders, af_string_list)
  expect_equal(output, c(5,5,1))
})

test_that("Taylor vector gets error because of dimension missmatch", {
  af_string_list <- c("softplus", "softplus", "linear")
  taylor_orders <- c(5,1)
  expect_error(obtain_taylor_vector(taylor_orders, af_string_list))
})
