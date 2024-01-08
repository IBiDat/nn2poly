test_that("The constranied training works using the l1 and l2 constraints", {
  skip_if_not_installed("luz")
  skip_if_not_installed("torch")
  skip_on_os("mac") # Runner out of memory
  skip_on_cran()

  testing_data <- testing_helper_2()

  data <- luz_test_data(testing_data)

  # Do the constrained training with the l1 constraint
  fitted <- luz_test_model() %>%
    luz::setup(
      loss = torch::nn_mse_loss(),
      optimizer = torch::optim_adam,
    ) %>%
    add_constraints("l1_norm") %>%
    fit(data$train, epochs = 3, valid_data = data$valid, verbose = FALSE)

  wb <- torch::torch_tensor(
    rbind(
      t(as.matrix(fitted$model$children[[1]][["bias"]])),
      t(as.matrix(fitted$model$children[[1]][["weight"]]))
    ),
    requires_grad = TRUE
  )

  # Check that all l1-norms are <= 1
  expect_true(
    all(torch::as_array(torch::linalg_vector_norm(wb, dim = 1, ord = 1)) <= 1)
  )

  # Now, the same but with the l2 constraint

  # Do the constrainted training with the l2 constraint
  fitted <- luz_test_model() %>%
    luz::setup(
      loss = torch::nn_mse_loss(),
      optimizer = torch::optim_adam,
    ) %>%
    add_constraints("l2_norm") %>%
    fit(data$train, epochs = 3, valid_data = data$valid, verbose = FALSE)

  wb <- torch::torch_tensor(
    rbind(
      t(as.matrix(fitted$model$children[[1]][["bias"]])),
      t(as.matrix(fitted$model$children[[1]][["weight"]]))
    ),
    requires_grad = TRUE
  )

  # Check that all l2-norms are <= 1
  expect_true(
    all(torch::as_array(torch::linalg_vector_norm(wb, dim = 1, ord = 2)) <= 1)
  )

})
