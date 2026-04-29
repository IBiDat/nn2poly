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
  vdiffr::expect_doppelganger("top 5", p)
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
  vdiffr::expect_doppelganger("top NULL", p)
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
  vdiffr::expect_doppelganger("top 5", p)
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
  vdiffr::expect_doppelganger("top NULL", p)
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

test_that("Waterfall/local_contributions plots return ggplot", {
  skip_if_not_installed("ggplot2")

  testing_data <- testing_helper_1()
  object <- testing_data$weights_list
  names(object) <- testing_data$af_string_list
  taylor_orders <- testing_data$taylor_orders

  poly <- nn2poly(
    object = object,
    taylor_orders = taylor_orders,
    keep_layers = FALSE,
    max_order = 3
  )

  set.seed(1)
  newdata <- matrix(rnorm(30), ncol = 2)
  monomials <- predict(poly, newdata, monomials = TRUE)

  p1 <- plot(poly, type = "waterfall", newdata_monomials = monomials, observation_index = 1, waterfall_n = 5)
  expect_s3_class(p1, "ggplot")

  p2 <- plot(poly, type = "local_contributions", newdata_monomials = monomials, observation_index = 1, max_order_to_display = 3)
  expect_s3_class(p2, "ggplot")
})

test_that("Waterfall plot works when keep_layers=TRUE and monomials are a layer list", {
  skip_if_not_installed("ggplot2")

  testing_data <- testing_helper_1()
  object <- testing_data$weights_list
  names(object) <- testing_data$af_string_list
  taylor_orders <- testing_data$taylor_orders

  poly_layers <- nn2poly(
    object = object,
    taylor_orders = taylor_orders,
    keep_layers = TRUE,
    max_order = 3
  )

  set.seed(1)
  newdata <- matrix(rnorm(30), ncol = 2)
  monomials_layers <- predict(poly_layers, newdata, monomials = TRUE)

  p <- plot(poly_layers, type = "waterfall", newdata_monomials = monomials_layers, observation_index = 1, waterfall_n = 5)
  expect_s3_class(p, "ggplot")
})

test_that("Beeswarm plot returns ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggbeeswarm")

  testing_data <- testing_helper_1()
  object <- testing_data$weights_list
  names(object) <- testing_data$af_string_list
  taylor_orders <- testing_data$taylor_orders

  poly <- nn2poly(
    object = object,
    taylor_orders = taylor_orders,
    keep_layers = FALSE,
    max_order = 3
  )

  set.seed(1)
  newdata <- matrix(rnorm(30), ncol = 2)
  colnames(newdata) <- c("x1", "x2")
  monomials <- predict(poly, newdata, monomials = TRUE)

  p <- plot(
    poly,
    type = "beeswarm",
    newdata_monomials = monomials,
    original_feature_data = newdata,
    color_by_feature = "x1",
    top_n_terms = 5
  )
  expect_s3_class(p, "ggplot")
})
