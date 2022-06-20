test_that("Check algorithm against precomputed example", {
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
    historical_coeffs = TRUE
  )

  # Desired coeffcient is  output y at layer 2, neuron 1,
  # coefficient "1,1"
  label <- result[[4]][[1]][[4]]
  coeff <- result[[4]][[2]][4]
  expect_equal(label,c(1,1))
  expect_equal(coeff,0.63351833)
})
