test_that("Check algorithm against precomputed example, without restricting
          the maximum order Q", {
  # Load the example:
  nn2poly_example <- nn2poly_example0

  # Get the needed data
  weights_list <- nn2poly_example$weights_list
  af_string_list <- nn2poly_example$af_string_list
  taylor_orders <- nn2poly_example$q_taylor_vector

  result <- nn2poly_algorithm(
    weights_list = weights_list,
    af_string_list = af_string_list,
    taylor_orders = taylor_orders,
    keep_layers = TRUE
  )

  # Output polynomial order is 2, as no max_order is specified and default is 2
  n_terms <- length(result[[length(result)]]$labels)
  order <- length(result[[length(result)]]$labels[[n_terms]])
  expect_equal(order, 2)

  # Desired coefficient is  output y at layer 2, neuron 1,
  # coefficient "1,1"
  label <- result[[4]]$labels[[4]]
  coeff <- result[[4]]$values[1,4]
  expect_equal(label,c(1,1))
  expect_equal(coeff,0.63351833)

})


test_that("Check that the algortihm provides a correct value for a certain
          coefficient from a given example that has been computed manually,
          using the optional parameter `max_order`", {

  # Load the example:
  nn2poly_example <- nn2poly_example0

  # Get the needed data
  weights_list <- nn2poly_example$weights_list
  af_string_list <- nn2poly_example$af_string_list
  taylor_orders <- nn2poly_example$q_taylor_vector

  result <- nn2poly_algorithm(
    weights_list = weights_list,
    af_string_list = af_string_list,
    taylor_orders = taylor_orders,
    keep_layers = TRUE,
    max_order = 2
  )

  # Output polynomial order is 2, as it is forced to be 2 instead of 4.
  n_terms <- length(result[[length(result)]][[1]])
  order <- length(result[[length(result)]][[1]][[n_terms]])
  expect_equal(order, 2)

  # Desired coeffcient is  output y at layer 2, neuron 1,
  # coefficient "1,1"
  label <- result[[4]][[1]][[4]]
  coeff <- result[[4]]$values[1,4]
  expect_equal(label,c(1,1))
  expect_equal(coeff,0.63351833)


})
