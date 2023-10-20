test_that("The function works as expected", {
  skip_if_not_installed("keras")
  skip_if_not_installed("tensorflow")
  skip()
  skip_on_cran()
  skip_on_covr()
  skip_on_ci()

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
             epochs = 30,
             validation_split = 0.3
  )

  # compute the different plots
  plots <- plot_taylor_and_activation_potentials(
    object = nn,
    data = cbind(nn2poly_example0$train_x, nn2poly_example0$train_y),
    q_taylor_vector = nn2poly_example0$q_taylor_vector,
    forced_max_Q = 3,
    constraints = FALSE)

  # test the different plots
  vdiffr::expect_doppelganger("layer1", plots[[1]])
  vdiffr::expect_doppelganger("layer2", plots[[2]])
  vdiffr::expect_doppelganger("layer3", plots[[3]])

})
