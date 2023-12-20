test_that("eval_poly: Single polynomial evaluation and single observation works", {

  # With intercept and ordered labels
  poly <- list()
  poly$values <- c(1,-1,1,0.5)
  poly$labels <- list(c(0),c(1),c(2),c(1,1))
  class(poly) <- "nn2poly"

  newdata <- c(1,1)
  expect_equal(predict(poly, newdata), as.vector(1.5))

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- c(1,-1,1,0.5)
  poly$labels <- list(c(1),c(0),c(1,1),c(2))
  class(poly) <- "nn2poly"

  newdata <- c(2,-1)
  expect_equal(predict(poly, newdata), as.vector(4.5))
})


test_that("eval_poly: Multiple polynomial evaluation and single observation works", {

  # Without intercept
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(1),c(2),c(1,1))
  class(poly) <- "nn2poly"

  newdata <- c(1,2)
  expect_equal(predict(poly, newdata), as.matrix(c(0,6)))

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(2),c(0),c(2,1))
  class(poly) <- "nn2poly"

  newdata <- c(2,-1)
  expect_equal(predict(poly, newdata), as.matrix(c(-4,5)))
})

test_that("eval_poly: Observation as dataframe works", {
  # Single observation, multiple polynomials
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(1),c(2),c(1,1))
  class(poly) <- "nn2poly"

  newdata <- c(1,2)
  newdata <- as.data.frame(x)

  expect_equal(predict(poly, newdata), as.matrix(c(0,6)))


  # Multiple Observations

  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(1),c(2),c(1,1))
  class(poly) <- "nn2poly"

  newdata <- rbind(c(1,2), c(1,1))
  newdata <- as.data.frame(x)

  expect_equal(predict(poly, newdata), cbind(c(0,6),c(1,3)))
})
