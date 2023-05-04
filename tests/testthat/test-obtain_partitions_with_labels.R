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
