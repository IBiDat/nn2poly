test_that("Test the plot for a polynomial generated with `keep_layers = FALSE`", {
  skip_if_not_installed("vdiffr")

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
  skip_if_not_installed("vdiffr")

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
  skip_if_not_installed("vdiffr")

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
  skip_if_not_installed("vdiffr")

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
  skip_if_not_installed("vdiffr")

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
  skip_if_not_installed("vdiffr")

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
  skip_if_not_installed("vdiffr")

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

test_that("Bar plot exposes correct axis labels and validates count arguments", {
  skip_if_not_installed("ggplot2")

  poly <- structure(
    list(
      labels = list(c(0), c(1), c(2), c(1, 2)),
      values = matrix(c(1, -2, 3, 4), ncol = 1)
    ),
    class = "nn2poly"
  )

  p <- plot(poly, n = 3)
  expect_equal(p$labels$x, "Polynomial Term")
  expect_equal(p$labels$y, "Coefficient (absolute value)")

  multi_output_poly <- structure(
    list(
      labels = list(c(1), c(2)),
      values = matrix(c(10, 1, 1, 10), nrow = 2, ncol = 2)
    ),
    class = "nn2poly"
  )
  p_multi <- plot(multi_output_poly, n = 2)
  expect_equal(
    levels(p_multi$data$term_display_order),
    c("1___1", "2___1", "2___2", "1___2")
  )

  expect_error(plot(poly, n = -1), "'n' must be >= 0")
  expect_error(plot(poly, n = 1.5), "'n' must be a single whole number")
  expect_error(plot(poly, min_order = NA_real_), "'min_order' must be a single whole number")
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
  expect_equal(
    tail(p1$data$end, 1),
    as.numeric(predict(poly, newdata)[1]),
    tolerance = 1e-10
  )

  p2 <- plot(poly, type = "local_contributions", newdata_monomials = monomials, observation_index = 1, max_order_to_display = 3)
  expect_s3_class(p2, "ggplot")

  expect_error(
    plot(poly, type = "waterfall", newdata_monomials = monomials, observation_index = 1.2),
    "'observation_index' must be a single whole number"
  )
  expect_error(
    plot(poly, type = "waterfall", newdata_monomials = monomials, waterfall_n = NA_real_),
    "'waterfall_n' must be a single whole number"
  )
  expect_error(
    plot(poly, type = "local_contributions", newdata_monomials = monomials, max_order_to_display = 0),
    "'max_order_to_display' must be >= 1"
  )
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
  expect_equal(
    tail(p$data$end, 1),
    sum(monomials_layers$layer_3$output[1, , 1]),
    tolerance = 1e-10
  )
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

test_that("Beeswarm validates selected color feature before optional geoms", {
  skip_if_not_installed("ggplot2")

  poly <- structure(
    list(
      labels = list(c(0), c(1), c(2), c(1, 2)),
      values = matrix(c(1, -2, 3, 4), ncol = 1)
    ),
    class = "nn2poly"
  )
  newdata <- matrix(rnorm(20), ncol = 2)
  colnames(newdata) <- c("x1", "x2")
  monomials <- predict(poly, newdata, monomials = TRUE)

  expect_error(
    plot(
      poly,
      type = "beeswarm",
      newdata_monomials = monomials,
      original_feature_data = newdata,
      color_by_feature = 3
    ),
    "'color_by_feature' must be <= 2"
  )
  expect_error(
    plot(
      poly,
      type = "beeswarm",
      newdata_monomials = monomials,
      original_feature_data = newdata,
      color_by_feature = "missing"
    ),
    "'color_by_feature' did not match"
  )
  expect_error(
    plot(
      poly,
      type = "beeswarm",
      newdata_monomials = monomials,
      original_feature_data = newdata,
      top_n_terms = 0
    ),
    "'top_n_terms' must be >= 1"
  )
})

test_that("Interaction plots validate scalar arguments and accept 3D monomial input", {
  skip_if_not_installed("ggplot2")

  poly <- structure(
    list(
      labels = list(c(0), c(1), c(2), c(1, 2)),
      values = matrix(c(1, 2, -3, 4), ncol = 1)
    ),
    class = "nn2poly"
  )
  newdata <- matrix(rnorm(20), ncol = 2)
  monomials <- predict(poly, newdata, monomials = TRUE)

  expect_error(
    plot(
      poly,
      type = "interaction_surface",
      feature_pair = c(1, 2),
      original_feature_data = newdata,
      grid_resolution = 1
    ),
    "'grid_resolution' must be >= 2"
  )
  expect_error(
    plot(
      poly,
      type = "interaction_network",
      interaction_order_network = 1
    ),
    "'interaction_order_network' must be >= 2"
  )

  p_surface <- plot(
    poly,
    type = "interaction_surface",
    feature_pair = c(1, 2),
    original_feature_data = newdata,
    grid_resolution = 3
  )
  expect_s3_class(p_surface, "ggplot")
  expect_true(all(is.finite(p_surface$data$Prediction)))

  p <- plot(
    poly,
    type = "interaction_network",
    metric_network = "mean_monomial_abs",
    newdata_monomials = monomials
  )
  expect_s3_class(p, "ggplot")
})
