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


test_that("(Monomials) Single polynomial evaluation and single observation works", {

  # With intercept and ordered labels
  poly <- list()
  poly$values <- c(1,-1,1,0.5)
  poly$labels <- list(c(0),c(1),c(2),c(1,1))

  newdata <- c(1,1)
  expect_equal(eval_poly(poly, newdata, monomials = TRUE), array(c(1,-1,1,0.5),dim = c(1,4,1)))

  # With intercept and unordered labels
  poly <- list()
  poly$values <- c(1,-1,1,0.5)
  poly$labels <- list(c(1),c(0),c(1,1),c(2))

  newdata <- c(2,-1)
  expect_equal(eval_poly(poly, newdata, monomials = TRUE), array(c(2,-1,4,-0.5),c(1,4,1)))
})

test_that("(Monomials) Multiple polynomial evaluation and single observation works", {

  # Without intercept
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1),c(2),c(1,1))

  newdata <- c(1,1)

  # As newdata is full of 1s, the monomials will be exactly
  # the polynomial coefficient values, so we build it
  # as an array with the right dimensions.
  aux_dims <- c(1, nrow(poly$values), ncol(poly$values))
  aux_expected <- array(0, dim = aux_dims)

  for(i in 1:ncol(poly$values)){
    aux_expected[,,i] <- poly$values[,i]
  }

  expect_equal(eval_poly(poly, newdata, monomials = TRUE), aux_expected)

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(2),c(0),c(2,1))

  newdata <- c(1,1)

  # As newdata is full of 1s, the monomials will be exactly
  # the polynomial coefficient values, so we build it
  # as an array with the right dimensions.
  aux_dims <- c(1, nrow(poly$values), ncol(poly$values))
  aux_expected <- array(0, dim = aux_dims)

  for(i in 1:ncol(poly$values)){
    aux_expected[,,i] <- poly$values[,i]
  }

  expect_equal(eval_poly(poly, newdata, monomials = TRUE), aux_expected)
})

test_that("(Monomials) Single polynomial evaluation and multiple observations (matrix) works", {

  # Without intercept
  poly <- list()
  poly$values <- c(1,-1,1)
  poly$labels <- list(c(1),c(2),c(1,1))

  newdata <- rbind(c(1,1), c(1,1))

  # As newdata is full of 1s, the monomials will be exactly
  # the polynomial coefficient values, so we build it
  # as an array with the right dimensions.
  aux_dims <- c(2, 3, 1)
  aux_expected <- array(matrix(poly$values, ncol=3, nrow = 2,  byrow = TRUE), dim = aux_dims)

  expect_equal(eval_poly(poly, newdata, monomials = TRUE), aux_expected)
})

test_that("(Monomials) Multiple polynomial evaluation and multiple observations (matrix) works", {

  # Without intercept
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1),c(2),c(1,1))

  newdata <- rbind(c(1,1), c(1,1))

  # As newdata is full of 1s, the monomials will be exactly
  # the polynomial coefficient values, so we build it
  # as an array with the right dimensions.
  aux_dims <- c(2, 3, 2)
  aux_expected <- array(0, dim = aux_dims)
  aux_expected[,,1] <- matrix(poly$values[,1], ncol=3, nrow = 2,  byrow = TRUE)
  aux_expected[,,2] <- matrix(poly$values[,2], ncol=3, nrow = 2,  byrow = TRUE)

  expect_equal(eval_poly(poly, newdata, monomials = TRUE), aux_expected)

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(2),c(0),c(2,1))

  newdata <- rbind(c(1,1), c(1,1))

  # As newdata is full of 1s, the monomials will be exactly
  # the polynomial coefficient values, so we build it
  # as an array with the right dimensions.
  aux_dims <- c(2, 3, 2)
  aux_expected <- array(0, dim = aux_dims)
  aux_expected[,,1] <- matrix(poly$values[,1], ncol=3, nrow = 2,  byrow = TRUE)
  aux_expected[,,2] <- matrix(poly$values[,2], ncol=3, nrow = 2,  byrow = TRUE)

  expect_equal(eval_poly(poly, newdata, monomials = TRUE), aux_expected)
})


test_that("(Monomials) Check that adding the monomials gives the final poly prediction", {

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(2),c(0),c(2,1))

  newdata <- rbind(c(5,2), c(-1,5.4))

  A <- eval_poly(poly, newdata)
  B <- eval_poly(poly, newdata, monomials = TRUE)

  C <- cbind(rowSums(B[,,1]), rowSums(B[,,2]))

  expect_equal(A, C)

})

