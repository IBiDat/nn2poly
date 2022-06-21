test_that("Check algorithm against precomputed example", {
  if (!reticulate::py_module_available("sympy"))
    skip("sympy not available for testing")

  # Load the example:
  nn2poly_example <- nn2poly_example0


  # Get the needed data
  weights_list <- nn2poly_example$weights_list
  af_string_list <- nn2poly_example$af_string_list
  q_taylor_vector <- nn2poly_example$q_taylor_vector

  result <- nn2poly_algorithm(
    weights_list = weights_list,
    af_string_list = af_string_list,
    q_taylor_vector = q_taylor_vector,
    store_coeffs = TRUE
  )

  # Output polynomial order is 4, as no order is forced and taylor
  # vector is 2,2,1, so the product is 4:
  n_terms <- length(result[[length(result)]][[1]])
  order <- length(result[[length(result)]][[1]][[n_terms]])
  expect_equal(order, 4)

  # Desired coeffcient is  output y at layer 2, neuron 1,
  # coefficient "1,1"
  label <- result[[4]][[1]][[4]]
  coeff <- result[[4]][[2]][4]
  expect_equal(label,c(1,1))
  expect_equal(coeff,0.63351833)

})


test_that("Check that the algortihm provides a correct value for a certain
          coefficient from a given example that has been computed manually,
          using the optional parameter `forced_max_Q`", {

  # Load the example:
  nn2poly_example <- nn2poly_example0

  # Get the needed data
  weights_list <- nn2poly_example$weights_list
  af_string_list <- nn2poly_example$af_string_list
  q_taylor_vector <- nn2poly_example$q_taylor_vector

  result <- nn2poly_algorithm(
    weights_list = weights_list,
    af_string_list = af_string_list,
    q_taylor_vector = q_taylor_vector,
    store_coeffs = TRUE,
    forced_max_Q = 2
  )

  # Output polynomial order is 2, as it is forced to be 2 instead of 4.
  n_terms <- length(result[[length(result)]][[1]])
  order <- length(result[[length(result)]][[1]][[n_terms]])
  expect_equal(order, 2)

  # Desired coeffcient is  output y at layer 2, neuron 1,
  # coefficient "1,1"
  label <- result[[4]][[1]][[4]]
  coeff <- result[[4]][[2]][4]
  expect_equal(label,c(1,1))
  expect_equal(coeff,0.63351833)


})
