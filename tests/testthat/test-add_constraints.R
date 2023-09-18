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

test_that("The function works over an already trained network and the connstraints
          are fulfiled after training for the constrained network", {
  tensorflow::set_random_seed(42)

  # load the example
  nn2poly_example <- nn2poly_example0

  # save some of the variables of the example to improve readability
  train_x <- nn2poly_example$train_x
  train_y <- nn2poly_example$train_y
  af_string_list <- nn2poly_example$af_string_list
  q_taylor_vector <- nn2poly_example$q_taylor_vector

  # save the number of parameters as the number of columns in train_x
  p <- ncol(train_x)

  # save the train data as an unique variable
  train <- cbind(train_x, train_y)


  nn <- keras::keras_model_sequential()
  keras::`%>%`(nn, keras::layer_dense(units = 2,
                                      activation = "tanh",
                                      input_shape = 2))
  keras::`%>%`(nn, keras::layer_dense(units = 3,
                                      activation = "softplus"))
  keras::`%>%`(nn, keras::layer_dense(units = 2,
                                      activation = "linear"))

  # compile the model
  keras::compile(nn,
                 loss = "mse",
                 optimizer = keras::optimizer_adam(),
                 metrics = "mse")

  # train the model
  keras::fit(nn,
             train_x,
             train_y,
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
             train_x,
             train_y,
             verbose = 0,
             epochs = 5,
             validation_split = 0.2
  )

  constrained_nn_parameters <- get_model_parameters(constrained_nn)
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
