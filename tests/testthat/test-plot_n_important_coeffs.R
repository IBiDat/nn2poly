test_that("Test the plot for a polynomial generated `keep_layers = FALSE`", {
  # loading the example
  nn2poly_example <- nn2poly_example0
  object <- nn2poly_example$weights_list
  names(object) <- nn2poly_example$af_string_list
  taylor_orders <- nn2poly_example$q_taylor_vector

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

test_that("Test the plot for a polynomial generated `keep_layers = TRUE`", {
  # loading the example
  nn2poly_example <- nn2poly_example0
  object <- nn2poly_example$weights_list
  names(object) <- nn2poly_example$af_string_list
  taylor_orders <- nn2poly_example$q_taylor_vector

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
