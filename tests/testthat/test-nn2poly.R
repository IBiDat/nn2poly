test_that("nn2poly_algorithm:
          Check algorithm against precomputed example", {
  nn2poly_example <- nn2poly_example0

  # Get the needed data
  object <- nn2poly_example$weights_list
  names(object) <- nn2poly_example$af_string_list

  taylor_orders <- nn2poly_example$q_taylor_vector


  result <- nn2poly(
    object = object,
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


test_that("nn2poly_algorithm:
          Check algorithm against precomputed example but with the default taylor_orders", {
  nn2poly_example <- nn2poly_example0

  # Get the needed data
  object <- nn2poly_example$weights_list
  names(object) <- nn2poly_example$af_string_list

  result <- nn2poly(
    object = object,
    keep_layers = TRUE,
    max_order = 3

  )

  # Output polynomial order is 4, as no order is forced and taylor
  # vector is 2,2,1, so the product is 4:
  n_terms <- length(result[[length(result)]]$labels)
  order <- length(result[[length(result)]]$labels[[n_terms]])
  expect_equal(order, 3)

  # Desired coefficient is  output y at layer 2, neuron 1,
  # coefficient "1,1"
  label <- result[[4]]$labels[[4]]
  coeff <- result[[4]]$values[1,4]
  expect_equal(label,c(1,1))
  expect_equal(round(coeff,4),-2.2147)
})

test_that("nn2poly_algorithm:
          Check that the algortihm provides a correct value for a certain
          coefficient from a given example that has been computed manually,
          using the optional parameter `max_order`", {

  # Load the example:
  nn2poly_example <- nn2poly_example0

  # Get the needed data
  object <- nn2poly_example$weights_list
  names(object)   <- nn2poly_example$af_string_list
  taylor_orders <- nn2poly_example$q_taylor_vector

  result <- nn2poly(
    object = object,
    taylor_orders = taylor_orders,
    keep_layers = TRUE,
    max_order = 2
  )

  # Output polynomial order is 2, as it is forced to be 2 instead of 4.
  n_terms <- length(result[[length(result)]][[1]])
  order <- length(result[[length(result)]][[1]][[n_terms]])
  expect_equal(order, 2)

  # Desired coefficient is  output y at layer 2, neuron 1,
  # coefficient "1,1"
  label <- result[[4]][[1]][[4]]
  coeff <- result[[4]]$values[1,4]
  expect_equal(label,c(1,1))
  expect_equal(coeff,0.63351833)


})

test_that("nn2poly for a keras.engine.training.Model object", {
  skip_if_not_installed("keras")
  skip_if_not_installed("tensorflow")
  skip_on_cran()

  nn <- keras_test_model()

  result <- nn2poly(nn,
                    taylor_orders = c(2,2,1),
                    max_order = 2)

  expect_equal(result$values[1,1],  0.18148204)
  expect_equal(result$values[2,3], -0.71466625)
  expect_equal(result$labels[[6]], c(2,2))
})

test_that("nn2poly for a constrained keras.engine.training.Model object", {
  skip_if_not_installed("keras")
  skip_if_not_installed("tensorflow")
  skip_on_cran()

  nn <- keras_test_model()

  constrained_nn <- add_constraints(nn)

  result <- nn2poly(constrained_nn,
                    taylor_orders = c(2,2,1),
                    max_order = 2)

  expect_equal(result$values[1,1],  1.1253606)
  expect_equal(result$values[2,1], -0.45410551)
  expect_equal(result$labels[[6]], c(2,2))
})


test_that("nn2poly for a nn_module object", {
  skip_if_not_installed("luz")
  skip_if_not_installed("torch")
  skip_on_cran()

  data <- luz_test_data(nn2poly_example0)

  fitted <- luz_test_model() %>%
    luz::setup(
      loss = torch::nn_mse_loss(),
      optimizer = torch::optim_adam,
      metrics = list(
        luz::luz_metric_mse()
      )
    ) %>%
    luz::fit(data$train, epochs = 5, valid_data = data$valid)

  result <- nn2poly(fitted,
                    taylor_orders = nn2poly_example0$q_taylor_vector,
                    max_order = 3)

  expect_equal(round(result$values[1,1],2), 0.11)
  # expect_equal(result$values[2,1], -0.45410551)
  expect_equal(result$labels[[7]], c(1,1,1))

})

test_that("Check that it throws an error when the dimensions of the weights list
          are not right.", {
  nn2poly_example <- nn2poly_example0

  # Get the needed data
  object <- nn2poly_example$weights_list
  names(object) <- nn2poly_example$af_string_list
  object[[2]] <- rbind(object[[2]], c(1,1))

  taylor_orders <- nn2poly_example$q_taylor_vector

  expect_error(
    nn2poly(
      object = object,
      taylor_orders = taylor_orders,
      keep_layers = TRUE
    )
  )
})
