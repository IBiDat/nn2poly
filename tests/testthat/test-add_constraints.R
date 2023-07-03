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
