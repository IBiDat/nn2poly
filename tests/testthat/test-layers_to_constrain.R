# This file also tests the correct behavior of torch_forward_parser

test_that("The output of the function actually are the expected layers to constraint", {
  net <- torch::nn_module(
    "my_network",

    initialize = function() {
      self$linear1  <- torch::nn_linear(2,2)
      self$linear2  <- torch::nn_linear(2,3)
      self$linear3  <- torch::nn_linear(3,3)
      self$output   <- torch::nn_linear(3,1)
      self$softplus <- torch::nn_softplus()
      self$relu     <- torch::nn_relu()
    },

    forward = function(x) {
      x %>%
        self$linear1() %>%
        self$softplus() %>%
        self$relu() %>%
        self$linear2() %>%
        self$softplus() %>%
        self$linear3() %>%
        self$relu() %>%
        self$output()
    }
  )

  expect_equal(
    layers_to_constrain(net()),
    c("linear1", "linear2", "linear3")
  )
})
