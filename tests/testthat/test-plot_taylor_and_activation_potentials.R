test_that("The function works as expected", {
  # set the seeds
  tensorflow::set_random_seed(33)

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

  # generate the nn
  nn <- keras::keras_model_sequential()
  keras::`%>%`(nn, keras::layer_dense(
    units = 3,
    activation = "tanh",
    input_shape = p))
  keras::`%>%`(nn, keras::layer_dense(
    units = 3,
    activation = "tanh"))
  keras::`%>%`(nn, keras::layer_dense(
    units = 1,
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
             epochs = 30,
             validation_split = 0.3
  )

  # save the weights of the nn
  nn_weights <- get_model_parameters(nn)$weights_list

  # compute the different plots
  plots <- plot_taylor_and_activation_potentials(
    data = train,
    weights_list = nn_weights,
    af_string_list = af_string_list,
    q_taylor_vector = q_taylor_vector,
    forced_max_Q = 3,
    my_max_norm = list("unconstrained",1))

  # test the different plots
  vdiffr::expect_doppelganger("layer1", plots[[1]])
  vdiffr::expect_doppelganger("layer2", plots[[2]])
  vdiffr::expect_doppelganger("layer3", plots[[3]])

})
