test_that("Test that both the activation function and neurons are the same for
          the constrained and unconstrained version of the nn", {
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

  for (layer in seq_along(nn$layers)) {
    expect_equal(nn$layers[[layer]]$get_config()$activation,
                 constrained_nn$layers[[layer]]$get_config()$activation)
    expect_equal(nn$layers[[layer]]$get_config()$units,
                 constrained_nn$layers[[layer]]$get_config()$units)
  }

})

test_that("Test that the 'keep_old_weights' parameter works as intended", {
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

  constrained_nn <- add_constraints(nn, keep_old_weights = TRUE)

  nn_weights             <- get_model_parameters(nn)$weights_list
  constrained_nn_weights <- get_model_parameters(constrained_nn)$weights_list

  # check if the weights are the same
  expect_identical(nn_weights, constrained_nn_weights)

  # generate a new constrained nn without keeping the old weights
  # (the function defaults `keep_old_weights = FALSE` but lets make it explicit)

  constrained_nn2 <- add_constraints(nn, keep_old_weights = FALSE)

  constrained_nn_weights2 <- get_model_parameters(constrained_nn2)$weights_list

  # check if the weights are NOT the same
  expect_false(identical(nn_weights, constrained_nn_weights2))

})
