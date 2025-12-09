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
  p <- plot(result, n = 5)

  # testing the plot
  vdiffr::expect_doppelganger("top 5 FALSE", p)
})


test_that("Test the plot for a polynomial generated with n = NULL", {
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

  # computing the plot with no limit for the important coefficients
  p <- plot(result)

  # testing the plot
  vdiffr::expect_doppelganger("top NULL FALSE", p)
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
  p <- plot(result, n = 5)

  # testing the plot
  vdiffr::expect_doppelganger("top 5 TRUE", p)
})

test_that("Test the plot for a polynomial generated with  `keep_layers = TRUE`
          and n=NULL", {
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
  p <- plot(result, n = NULL)

  # testing the plot
  vdiffr::expect_doppelganger("top NULL TRUE", p)
})



test_that("Test the plot for a polynomial generated with  vector values input, all positive coeficients", {
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

  result$values <- as.vector(result$values)

  # computing the plot with 5 important coefficients
  p <- plot(result, n = 5)

  # testing the plot
  vdiffr::expect_doppelganger("top 5", p)
})


test_that("Test the plot for a polynomial generated with  0 valued coeff and positive and negative coefficients", {
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

  result$values[2,] <- 0
  result$values[4,] <- -5

  # computing the plot with 5 important coefficients
  p <- plot(result, n = 5)

  # testing the plot
  vdiffr::expect_doppelganger("top 5 mixed", p)
})


test_that("Test the plot for a polynomial generated with  all negative coefficients", {
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

  result$values <- -result$values

  # computing the plot with 5 important coefficients
  p <- plot(result, n = 5)

  # testing the plot
  vdiffr::expect_doppelganger("top 5 neg", p)
})
