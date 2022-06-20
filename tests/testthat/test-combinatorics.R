test_that("combinations with repetition algorithm works", {
  expect_snapshot(combinations_with_repetition(5, 3), cran=TRUE)
  expect_snapshot(combinations_with_repetition(3, 5), cran=TRUE)
})

test_that("multiset partitions are correctly generated", {
  expect_snapshot(generate_partitions(5, 3), cran=TRUE)
  expect_snapshot(generate_partitions(3, 5), cran=TRUE)
})
