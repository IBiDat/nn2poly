test_that("nn2poly_algorithm:
          Check algorithm against precomputed example", {
  nn2poly_example <- nn2poly_example0

  # Get the needed data
  object <- nn2poly_example$weights_list
  names(object) <- nn2poly_example$af_string_list

  q_taylor_vector <- nn2poly_example$q_taylor_vector


  result <- nn2poly(
    object = object,
    q_taylor_vector = q_taylor_vector,
    store_coeffs = TRUE
  )

  # Output polynomial order is 4, as no order is forced and taylor
  # vector is 2,2,1, so the product is 4:
  n_terms <- length(result[[length(result)]]$labels)
  order <- length(result[[length(result)]]$labels[[n_terms]])
  expect_equal(order, 4)

  # Desired coefficient is  output y at layer 2, neuron 1,
  # coefficient "1,1"
  label <- result[[4]]$labels[[4]]
  coeff <- result[[4]]$values[1,4]
  expect_equal(label,c(1,1))
  expect_equal(coeff,0.63351833)
})



test_that("nn2poly_algorithm:
          Check that the algortihm provides a correct value for a certain
          coefficient from a given example that has been computed manually,
          using the optional parameter `forced_max_Q`", {

  # Load the example:
  nn2poly_example <- nn2poly_example0

  # Get the needed data
  object <- nn2poly_example$weights_list
  names(object) <- nn2poly_example$af_string_list
  q_taylor_vector <- nn2poly_example$q_taylor_vector

  result <- nn2poly(
    object = object,
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
  coeff <- result[[4]]$values[1,4]
  expect_equal(label,c(1,1))
  expect_equal(coeff,0.63351833)


})

test_that("nn2poly for a keras.engine.training.Model object", {
  skip_on_cran()

  tensorflow::set_random_seed(42)

  nn <- keras::keras_model_sequential()
  keras::`%>%`(nn, keras::layer_dense(units = 2,
                                      activation = "tanh",
                                      input_shape = 2))
  keras::`%>%`(nn, keras::layer_dense(units = 3,
                                      activation = "softplus"))
  keras::`%>%`(nn, keras::layer_dense(units = 2,
                                      activation = "linear"))
  result <- nn2poly(nn,
                    q_taylor_vector = c(2,2,1),
                    forced_max_Q = 2)

  expect_equal(result$values[1,1],  0.18148204)
  expect_equal(result$values[2,3], -0.71466625)
  expect_equal(result$labels[[6]], c(2,2))
})

test_that("nn2poly for a constrained keras.engine.training.Model object", {
  skip_on_cran()

  tensorflow::set_random_seed(42)

  nn <- keras::keras_model_sequential()
  keras::`%>%`(nn, keras::layer_dense(units = 2,
                                      activation = "tanh",
                                      input_shape = 2))
  keras::`%>%`(nn, keras::layer_dense(units = 3,
                                      activation = "softplus"))
  keras::`%>%`(nn, keras::layer_dense(units = 2,
                                      activation = "linear"))

  constrained_nn <- add_constraints(nn)

  result <- nn2poly(constrained_nn,
                    q_taylor_vector = c(2,2,1),
                    forced_max_Q = 2)

  expect_equal(result$values[1,1],  1.1253606)
  expect_equal(result$values[2,1], -0.45410551)
  expect_equal(result$labels[[6]], c(2,2))
})

