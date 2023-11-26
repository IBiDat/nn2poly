test_that("Test the plot for a polynomial generated with `keep_layers = FALSE`", {
  # loading the example
  testing_data <- testing_helper_1()
  object <- testing_data$weights_list
  names(object) <- testing_data$af_string_list
  taylor_orders <- testing_data$taylor_orders

  # computing the polynomial
  result <- nn2poly(
    object = object,
    taylor_orders = taylor_orders,
    keep_layers = FALSE
  )

  # computing the plot with 5 important coefficients
  p <- plot_n_important_coeffs(result, n_important_coeffs = 5)

  # testing the plot
  vdiffr::expect_doppelganger("top 5 important coefficients", p)
})

test_that("Test the plot for a polynomial generated with `keep_layers = FALSE` and vector values input", {
  # loading the example
  testing_data <- testing_helper_1()
  object <- testing_data$weights_list
  names(object) <- testing_data$af_string_list
  taylor_orders <- testing_data$taylor_orders

  # computing the polynomial
  result <- nn2poly(
    object = object,
    taylor_orders = taylor_orders,
    keep_layers = FALSE
  )

  result$vales <- as.vector(result$values)

  # computing the plot with 5 important coefficients
  p <- plot_n_important_coeffs(result, n_important_coeffs = 5)

  # testing the plot
  vdiffr::expect_doppelganger("top 5 important coefficients", p)
})


test_that("Test the plot for a polynomial generated with  `keep_layers = TRUE`", {
  # loading the example
  testing_data <- testing_helper_1()
  object <- testing_data$weights_list
  names(object) <- testing_data$af_string_list
  taylor_orders <- testing_data$taylor_orders

  # computing the polynomial
  result <- nn2poly(
    object = object,
    taylor_orders = taylor_orders,
    keep_layers = TRUE
  )

  # computing the plot with 5 important coefficients
  p <- plot_n_important_coeffs(result, n_important_coeffs = 5)

  # testing the plot
  vdiffr::expect_doppelganger("top 5 important coefficients", p)
})
