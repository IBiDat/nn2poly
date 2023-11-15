test_that("The function works over an already trained network and the connstraints
          are fulfiled after training for the constrained network", {
  skip_if_not_installed("keras")
  skip_if_not_installed("tensorflow")
  skip_on_cran()

  testing_data <- testing_helper_2()

  nn <- keras_test_model()

  # compile the model
  keras::compile(nn,
                 loss = "mse",
                 optimizer = keras::optimizer_adam(),
                 metrics = "mse")

  constrained_nn <- add_constraints(nn, constraint_type = "l1_norm")

  # train the non-constrained model
  fit(nn,
      x = testing_data$train_x,
      y = testing_data$train_y,
      verbose = 0,
      epochs = 3,
      validation_split = 0.2
  )

  # train the constrained model
  fit(constrained_nn,
      testing_data$train_x,
      testing_data$train_y,
      verbose = 0,
      epochs = 3,
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
      sapply(weights_l1_norm[1:length(weights_l1_norm) - 1], # skip the output layer
             function(l1_norm) all(l1_norm < 1))
    )
  )

})
