test_that("multiplication works", {
  # skip on cran and on github acions
  skip_on_cran()
  skip_on_covr()
  skip_on_ci()

  expect_equal(2 * 2, 4)
})
