test_that("The function works well over a regular Neural Network.", {

  # setting the seeds for reproducibility purposes
  tensorflow::set_random_seed(42)

  nn <- keras::keras_model_sequential()
  keras::`%>%`(nn, keras::layer_dense(units = 2,
                                      activation = "tanh",
                                      input_shape = 2))
  keras::`%>%`(nn, keras::layer_dense(units = 3,
                                      activation = "softplus"))
  keras::`%>%`(nn, keras::layer_dense(units = 2,
                                      activation = "linear"))

  # save the parameters into a list
  params <- get_model_parameters(nn)

  expect_equal(params$p, 2)

  expect_equal(params$af_string_list[[1]], "tanh")
  expect_equal(params$af_string_list[[2]], "softplus")

  expect_equal(params$n_neurons[[1]], 2)
  expect_equal(params$n_neurons[[2]], 3)

  expect_equal(params$weights[[1]][[2]], 0.8773805)
  expect_equal(params$weights[[2]][[3]], 0.53210747)

  expect_equal(params$p, 2)

})

test_that("The get_model_parameters functions returns the right list of activation
          functions for a neural network with custom constraints.", {
  # setting the seeds for reproducibility purposes
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
  params <- get_model_parameters(constrained_nn)

  expect_equal(params$af_string_list, list("tanh", "softplus", "linear"))


})

