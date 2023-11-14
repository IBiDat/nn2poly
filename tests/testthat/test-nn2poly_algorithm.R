#Note that other options are tested in nn2poly.

test_that("nn2poly_algorithm against precomputed example", {
  # Load the example:
  testing_data <- testing_helper_1()

  # Get the needed data
  weights_list <- testing_data$weights_list
  af_string_list <- testing_data$af_string_list
  taylor_orders <- testing_data$taylor_orders

  result <- nn2poly_algorithm(
    weights_list = weights_list,
    af_string_list = af_string_list,
    max_order = 3,
    keep_layers = TRUE,
    taylor_orders = taylor_orders
  )

  n_terms <- length(result[[length(result)]]$labels)
  order <- length(result[[length(result)]]$labels[[n_terms]])
  expect_equal(order, 3)

  # Desired coefficient in output polynomial at layer 2 (element 2*2=4 in list),
  # neuron 1, coefficient "1,1"
  label <- result[[4]]$labels[[4]]
  coeff <- result[[4]]$values[1,4]
  expect_equal(label,c(1,1))
  expect_equal(coeff,0.63351833)

})

test_that("Check that the first partition of the multiset is allways the
          multiset itself, and, therefore, the label that we are looking for.", {

  # Using `expect_setequal` to ignore the order of the elements in the list.
  #
  # If we use `expect_equal`:
  #
  # `expect_equal(partitions_with_labels1$labels,
  #              expected_labels1`)
  # will pass, but
  # `expect_equal(partitions_with_labels1$labels,
  #              expected_labels1[c(2,1,3)])`
  # will fail


  # CASE 1

  # p = 2
  # q = 2

  partitions_with_labels1 <- obtain_partitions_with_labels(2,2)
  expected_labels1 <- list(1, c(1,1), 1:2)

  expect_setequal(partitions_with_labels1$labels,
                  expected_labels1)

  expect_setequal(partitions_with_labels1$labels,
                  expected_labels1[c(2,1,3)])

  # CASE 2

  # p = 2
  # q = 3

  partitions_with_labels2 <- obtain_partitions_with_labels(2,3)
  expected_labels2 <- list(1, c(1,1), 1:2, c(1,1,1), c(1,1,2))

  expect_setequal(partitions_with_labels2$labels,
                  expected_labels2)

  # CASE 3

  # p = 2
  # q = 4

  partitions_with_labels3 <- obtain_partitions_with_labels(2,4)
  expected_labels3 <- list(1, c(1,1), 1:2, c(1,1,1), c(1,1,2), c(1,1,1,1),
                           c(1,1,1,2), c(1,1,2,2))

  expect_setequal(partitions_with_labels3$labels,
                  expected_labels3)

  # CASE 4

  # p = 3
  # q = 3

  partitions_with_labels4 <- obtain_partitions_with_labels(3,3)
  expected_labels4 <- list(1, c(1,1), 1:2, c(1,1,1), c(1,1,2), 1:3)

  expect_setequal(partitions_with_labels4$labels,
                  expected_labels4)

  # CASE 5

  # p = 4
  # q = 4

  partitions_with_labels5 <- obtain_partitions_with_labels(4,4)
  expected_labels5 <- list(1, c(1,1), 1:2, c(1,1,1), c(1,1,2),
                           1:3, c(1,1,1,1), c(1,1,1,2), c(1,1,2,2),
                           c(1,1,2,3), 1:4)

  expect_setequal(partitions_with_labels5$labels,
                  expected_labels5)

})

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
