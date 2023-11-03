test_that("Taylor vector obtained with single value and mutiple linear layers", {
  af_string_list <- c("softplus", "linear", "softplus", "linear")
  taylor_orders <- 5L
  output <- obtain_taylor_vector(taylor_orders, af_string_list)
  expect_equal(output, c(5,1,5,1))
})

test_that("Taylor vector obtained with vector", {
  af_string_list <- c("softplus", "softplus", "linear")
  taylor_orders <- c(5,5,1)
  output <- obtain_taylor_vector(taylor_orders, af_string_list)
  expect_equal(output, c(5,5,1))
})

test_that("Taylor vector gets error because of dimension missmatch", {
  af_string_list <- c("softplus", "softplus", "linear")
  taylor_orders <- c(5,1)
  expect_error(obtain_taylor_vector(taylor_orders, af_string_list))
})


test_that("Taylor vector gets error becauseof non numeric value", {
  af_string_list <- c("softplus", "softplus", "linear")
  taylor_orders <- c(5.4, 2.3, 1)
  expect_error(obtain_taylor_vector(taylor_orders, af_string_list))
})

