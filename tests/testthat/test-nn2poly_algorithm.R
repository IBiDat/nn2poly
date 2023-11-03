#Note that other options are tested in nn2poly.

test_that("nn2poly_algorithm against precomputed example", {
  # Load the example:
  testing_data <- testing_helper_1()

  # Get the needed data
  weights_list <- testing_data$weights_list
  af_string_list <- testing_data$af_string_list
  taylor_orders <- testing_data$taylor_orders

  result <- nn2poly_algorithm(
    weights_list = weights_list,
    af_string_list = af_string_list,
    max_order = 3,
    keep_layers = TRUE,
    taylor_orders = taylor_orders
  )

  n_terms <- length(result[[length(result)]]$labels)
  order <- length(result[[length(result)]]$labels[[n_terms]])
  expect_equal(order, 3)

  # Desired coefficient in output polynomial at layer 2 (element 2*2=4 in list),
  # neuron 1, coefficient "1,1"
  label <- result[[4]]$labels[[4]]
  coeff <- result[[4]]$values[1,4]
  expect_equal(label,c(1,1))
  expect_equal(coeff,0.63351833)

})
