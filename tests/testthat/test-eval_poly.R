test_that("Single polynomial evaluation and single observation works", {

  # With intercept and ordered labels
  poly <- list()
  poly$values <- c(1,-1,1,0.5)
  poly$labels <- list(c(0),c(1),c(2),c(1,1))

  newdata <- c(1,1)
  expect_equal(eval_poly(poly, newdata), as.vector(1.5))

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- c(1,-1,1,0.5)
  poly$labels <- list(c(1),c(0),c(1,1),c(2))

  newdata <- c(2,-1)
  expect_equal(eval_poly(poly, newdata), as.vector(4.5))
})

test_that("Multiple polynomial evaluation and single observation works", {

  # Without intercept
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1),c(2),c(1,1))

  newdata <- c(1,2)
  expect_equal(eval_poly(poly, newdata), t(as.matrix(c(0,6))))

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(2),c(0),c(2,1))

  newdata <- c(2,-1)
  expect_equal(eval_poly(poly, newdata), t(as.matrix(c(-4,5))))
})


test_that("Single polynomial evaluation and multiple observations (matrix) works", {

  # Without intercept
  poly <- list()
  poly$values <- c(1,-1,1)
  poly$labels <- list(c(1),c(2),c(1,1))

  newdata <- rbind(c(1,2), c(1,1))
  expect_equal(eval_poly(poly, newdata), as.vector(c(0,1)))
})

test_that("Multiple polynomial evaluation and multiple observations (matrix) works", {

  # Without intercept
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1),c(2),c(1,1))

  newdata <- rbind(c(1,2), c(1,1))
  expect_equal(eval_poly(poly, newdata), cbind(c(0,1),c(6,3)))

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(2),c(0),c(2,1))

  newdata <- rbind(c(2,-1), c(1,1))
  expect_equal(eval_poly(poly, newdata), cbind(c(-4,1),c(5,3)))
})


test_that("Observation as dataframe works", {
  # Single Observation, multiple polynomials
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1),c(2),c(1,1))

  newdata <- c(1,2)
  newdata <- as.data.frame(newdata)
  expect_equal(eval_poly(poly, newdata), t(as.matrix(c(0,6))))


  # Multiple Observations
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1),c(2),c(1,1))

  newdata <- rbind(c(1,2), c(1,1))
  newdata <- as.data.frame(newdata)
  expect_equal(eval_poly(poly, newdata), cbind(c(0,1),c(6,3)))
})


test_that("Works with higher order elements at the start and no intercept", {

  # This check is beacuse in an older version we had a problem if
  # a multiple element label was at first position and no intercept was created.
  # This should not be a problem with the new version but we'll keep the test.

  # Without intercept
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          -2,3,2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1,1),c(2),c(1))

  newdata <- rbind(c(1,2), c(1,1))
  expect_equal(eval_poly(poly, newdata), cbind(c(0,1),c(6,3)))
})
