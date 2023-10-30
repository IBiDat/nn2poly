keras_test_model <- function() {
  tensorflow::set_random_seed(42)

  nn <- keras::keras_model_sequential()
  nn <- keras::layer_dense(nn, units = 2, activation = "tanh", input_shape = 2)
  nn <- keras::layer_dense(nn, units = 3, activation = "softplus")
  nn <- keras::layer_dense(nn, units = 2, activation = "linear")

  nn
}
