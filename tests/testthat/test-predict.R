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
  newdata <- as.data.frame(newdata)

  expect_equal(predict(poly, newdata), as.matrix(c(0,6)))


  # Multiple Observations

  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(1),c(2),c(1,1))
  class(poly) <- "nn2poly"

  newdata <- rbind(c(1,2), c(1,1))
  newdata <- as.data.frame(newdata)

  expect_equal(predict(poly, newdata), cbind(c(0,6),c(1,3)))
})


test_that("Multiple layers: eval_poly works on each layer(input/output)", {
  # Define a poly object with 2 polynomials
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(1),c(2),c(1,1))

  # Replicate that polynomial 3 times as if it was the output
  # of a 1 hidden layer NN with linear output (2+1)
  object <- list()
  object[[1]] <- poly
  object[[2]] <- poly
  object[[3]] <- poly

  class(object) <- "nn2poly"

  newdata <- c(1,2)
  prediction <- predict(object, newdata)
  # All pollys are the same so we loop over them
  for (pred_layer in prediction){
    for (pred_layer_i in pred_layer)
      expect_equal(pred_layer_i, as.matrix(c(0,6)))
  }

  # Same but with 2 outputs in final layer
  object[[4]] <- poly
  prediction <- predict(object, newdata)
  # All pollys are the same so we loop over them
  for (pred_layer in prediction){
    for (pred_layer_i in pred_layer)
      expect_equal(pred_layer_i, as.matrix(c(0,6)))
  }

})


test_that("Multiple layers: choosing last layer returns the same as evaluation
          on single single polynomial", {
  # Define a poly object with 2 polynomials
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(1),c(2),c(1,1))

  # Replicate that polynomial 3 times as if it was the output
  # of a 1 hidden layer NN with linear output (2+1)
  object <- list()
  object[[1]] <- poly
  object[[2]] <- poly
  object[[3]] <- poly

  class(object) <- "nn2poly"

  newdata <- c(1,2)
  prediction1 <- predict(object, newdata)

  # Get prediction for a single polynomial
  class(poly) <- "nn2poly"
  prediction2 <- predict(poly, newdata)

  expect_equal(prediction1$layer_2$output, prediction2)

  # Also test that input and output are equal when having linear output:
  expect_equal(prediction1$layer_2$output, prediction1$layer_2$input)

})


test_that("Multiple layers: layers argument testing", {
  # Define a poly object with 2 polynomials
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), nrow = 2, byrow = TRUE)
  poly$labels <- list(c(1),c(2),c(1,1))

  # Replicate that polynomial 3 times as if it was the output
  # of a 1 hidden layer NN with linear output (2+1)
  object <- list()
  object[[1]] <- poly
  object[[2]] <- poly
  object[[3]] <- poly

  class(object) <- "nn2poly"

  newdata <- c(1,2)
  # Some errors
  expect_error(predict(object, newdata, layers = c(1,3)))
  expect_error(predict(object, newdata, layers = FALSE))

  # Choose only last layer and obtain the same as with full layers:
  prediction1 <- predict(object, newdata, layers = 2)

  # Get prediction for a single polynomial
  class(poly) <- "nn2poly"
  prediction2 <- predict(poly, newdata)

  expect_equal(prediction1$layer_2$output, prediction2)

  # Also test that layer_1 ins null if not chosen
  expect_null(prediction1$layer_1)

})


