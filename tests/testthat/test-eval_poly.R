test_that("Single polynomial evaluation and single observation works", {

  # With intercept and ordered labels
  poly <- list()
  poly$values <- c(1,-1,1,0.5)
  poly$labels <- list(c(0),c(1),c(2),c(1,1))

  x <- c(1,1)
  expect_equal(eval_poly(x,poly), as.vector(1.5))

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- c(1,-1,1,0.5)
  poly$labels <- list(c(1),c(0),c(1,1),c(2))

  x <- c(2,-1)
  expect_equal(eval_poly(x,poly), as.vector(4.5))
})

test_that("Multiple polynomial evaluation and single observation works", {

  # Without intercept
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(1),c(2),c(1,1))

  x <- c(1,2)
  expect_equal(eval_poly(x,poly), as.matrix(c(0,6)))

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(2),c(0),c(2,1))

  x <- c(2,-1)
  expect_equal(eval_poly(x,poly), as.matrix(c(-4,5)))
})


test_that("Single polynomial evaluation and multiple observations (matrix) works", {

  # Without intercept
  poly <- list()
  poly$values <- c(1,-1,1)
  poly$labels <- list(c(1),c(2),c(1,1))

  x <- rbind(c(1,2), c(1,1))
  expect_equal(eval_poly(x,poly), as.vector(c(0,1)))
})

test_that("Multiple polynomial evaluation and multiple observations (matrix) works", {

  # Without intercept
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(1),c(2),c(1,1))

  x <- rbind(c(1,2), c(1,1))
  expect_equal(eval_poly(x,poly), cbind(c(0,6),c(1,3)))

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(2),c(0),c(2,1))

  x <- rbind(c(2,-1), c(1,1))
  expect_equal(eval_poly(x,poly), cbind(c(-4,5),c(1,3)))
})


test_that("Observation as dataframe works", {
  # Single Observation, multiple polynomials
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(1),c(2),c(1,1))

  x <- c(1,2)
  x <- as.data.frame(x)
  expect_equal(eval_poly(x,poly), as.matrix(c(0,6)))


  # Multiple Observations
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(1),c(2),c(1,1))

  x <- rbind(c(1,2), c(1,1))
  x <- as.data.frame(x)
  expect_equal(eval_poly(x,poly), cbind(c(0,6),c(1,3)))
})
