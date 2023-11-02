test_that("Final order works with integer", {
  max_order <- 4L
  # Note that Taylor orders will always be a vector in the desired form
  # as it is built inside nn2poly_algorithm
  taylor_orders <- c(8,8,1)
  output <- obtain_final_poly_order(max_order, taylor_orders)
  expect_equal(output, 4L)
})

test_that("Final order works with integer in numeric form", {
  max_order <- 4.0
  # Note that Taylor orders will always be a vector in the desired form
  # as it is built inside nn2poly_algorithm
  taylor_orders <- c(8,8,1)
  output <- obtain_final_poly_order(max_order, taylor_orders)
  expect_equal(output, 4L)
})

test_that("Final order warns if max_order is not reached", {
  max_order <- 8L
  # Note that Taylor orders will always be a vector in the desired form
  # as it is built inside nn2poly_algorithm
  taylor_orders <- c(2,1,3)
  expect_warning(output <- obtain_final_poly_order(max_order, taylor_orders))
  expect_equal(output, 6L)
})


test_that("Final order stops with an error if max_order is not an integer", {
  max_order <- 4.3
  # Note that Taylor orders will always be a vector in the desired form
  # as it is built inside nn2poly_algorithm
  taylor_orders <- c(2,1,3)
  expect_error(obtain_final_poly_order(max_order, taylor_orders))
})
