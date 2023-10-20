test_that("Test that both the activation function and neurons are the same for
          the constrained and unconstrained version of the nn", {
  skip_if_not_installed("keras")
  skip_if_not_installed("tensorflow")
  skip_on_cran()

  nn <- keras_test_model()

  constrained_nn <- add_constraints(nn)

  for (layer in seq_along(nn$layers)) {
    expect_equal(nn$layers[[layer]]$get_config()$activation,
                 constrained_nn$layers[[layer]]$get_config()$activation)
    expect_equal(nn$layers[[layer]]$get_config()$units,
                 constrained_nn$layers[[layer]]$get_config()$units)
  }

})

test_that("Test that the 'keep_old_weights' parameter works as intended", {
  skip_if_not_installed("keras")
  skip_if_not_installed("tensorflow")
  skip_on_cran()

  nn <- keras_test_model()

  constrained_nn <- add_constraints(nn, keep_old_weights = TRUE)

  nn_weights             <- get_parameters(nn)$weights_list
  constrained_nn_weights <- get_parameters(constrained_nn)$weights_list

  # check if the weights are the same
  expect_identical(nn_weights, constrained_nn_weights)

  # generate a new constrained nn without keeping the old weights
  # (the function defaults `keep_old_weights = FALSE` but lets make it explicit)

  constrained_nn2 <- add_constraints(nn, keep_old_weights = FALSE)

  constrained_nn_weights2 <- get_parameters(constrained_nn2)$weights_list

  # check if the weights are NOT the same
  expect_false(identical(nn_weights, constrained_nn_weights2))

})

test_that("The function works over an already trained network and the connstraints
          are fulfiled after training for the constrained network", {
  skip_if_not_installed("keras")
  skip_if_not_installed("tensorflow")
  skip_on_cran()

  nn <- keras_test_model()

  # compile the model
  keras::compile(nn,
                 loss = "mse",
                 optimizer = keras::optimizer_adam(),
                 metrics = "mse")

  # train the model
  keras::fit(nn,
             nn2poly_example0$train_x,
             nn2poly_example0$train_y,
             verbose = 0,
             epochs = 5,
             validation_split = 0.2
  )

  constrained_nn <- add_constraints(nn, constraint_type = "l1_norm")



  # compile the model
  keras::compile(constrained_nn,
                 loss = "mse",
                 optimizer = keras::optimizer_adam(),
                 metrics = "mse")

  # train the model
  keras::fit(constrained_nn,
             nn2poly_example0$train_x,
             nn2poly_example0$train_y,
             verbose = 0,
             epochs = 5,
             validation_split = 0.2
  )

  constrained_nn_parameters <- get_parameters(constrained_nn)
  constrained_nn_weights  <- constrained_nn_parameters$weights_list

  # compute the l1-norm for all the weight matrices
  weights_l1_norm <- check_weight_constraints(constrained_nn_weights,
                                              maxnorm = list("l1_norm"))

  # check if the condition is fulfilled for every matrix
  expect_true(
    all(
      sapply(weights_l1_norm[1:length(weights_l1_norm)-1], # skip the output layer
             function(l1_norm) all(l1_norm < 1))
    )
  )

})
