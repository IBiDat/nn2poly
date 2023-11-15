test_that("tests that the function to which it has been transformed is correct",{

  # List with the four possible functions
  af_string_list   <- list("softplus", "tanh", "sigmoid", "linear")
  # List with the converted strings to functions
  af_function_list <- string_to_function(af_string_list)

  # softplus
  expect_equal(
    deparse(af_function_list[[1]]),
    deparse(function(x) log(1 + exp(x)))
  )

  # tanh
  expect_equal(
    deparse(af_function_list[[2]]),
    deparse(function(x) tanh(x))
  )

  # sigmoid
  expect_equal(
    deparse(af_function_list[[3]]),
    deparse(function(x) 1 / (1 + exp(-x)))
  )

  # linear
  expect_equal(
    deparse(af_function_list[[4]]),
    deparse(function(x) x)
  )

  expect_error(string_to_function(list("asdf")))

})


test_that("test that the value of  each function is the expected for
          some random generated numbers between -1 and 1",{

  # List with the four possible functions
  af_string_list   <- list("softplus", "tanh", "sigmoid", "linear")
  # List with the converted strings to functions
  af_function_list <- string_to_function(af_string_list)

  # Generating 1000 random numbers between -1 and 1
  random_numbers <- runif(1000, -1, 1)

  # softplus
  expect_equal(
    af_function_list[[1]](random_numbers),
    sapply(random_numbers, function(x) log(1 + exp(x)))
  )

  # tanh
  expect_equal(
    af_function_list[[2]](random_numbers),
    sapply(random_numbers, function(x) tanh(x))
  )

  # sigmoid
  expect_equal(
    af_function_list[[3]](random_numbers),
    sapply(random_numbers, function(x) 1 / (1 + exp(-x)))
  )

  # linear
  expect_equal(
    af_function_list[[4]](random_numbers),
    sapply(random_numbers, function(x) x)
  )

})
