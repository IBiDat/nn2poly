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

test_that("(Monomials) Check that adding the monomials gives the final poly prediction", {

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(2),c(0),c(2,1))
  class(poly) <- "nn2poly"

  newdata <- rbind(c(5,2), c(-1,5.4))

  A <- predict(poly, newdata)
  B <- predict(poly, newdata, monomials = TRUE)

  C <- cbind(rowSums(B[,,1]), rowSums(B[,,2]))

  expect_equal(A, C)

})


test_that("eval_poly: Multiple polynomial evaluation and single observation works", {

  # Without intercept
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1),c(2),c(1,1))
  class(poly) <- "nn2poly"

  newdata <- c(1,2)
  expect_equal(predict(poly, newdata), t(as.matrix(c(0,6))))

  # With intercept and unnordered labels
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(2),c(0),c(2,1))
  class(poly) <- "nn2poly"

  newdata <- c(2,-1)
  expect_equal(predict(poly, newdata), t(as.matrix(c(-4,5))))
})

test_that("eval_poly: Observation as dataframe works", {
  # Single observation, multiple polynomials
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1),c(2),c(1,1))
  class(poly) <- "nn2poly"

  newdata <- c(1,2)
  newdata <- as.data.frame(newdata)

  expect_equal(predict(poly, newdata), t(as.matrix(c(0,6))))


  # Multiple Observations

  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1),c(2),c(1,1))
  class(poly) <- "nn2poly"

  newdata <- rbind(c(1,2), c(1,1))
  newdata <- as.data.frame(newdata)

  expect_equal(predict(poly, newdata), cbind(c(0,1),c(6,3)))
})


test_that("Multiple layers (and Monomials): eval_poly works on each layer(input/output)", {
  # Define a poly object with 2 polynomials
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1),c(2),c(1,1))

  # Replicate that polynomial 4 times as if it was the output
  # of a 1 hidden layer NN
  object <- list()
  object[["layer_1"]][["input"]] <- poly
  object[["layer_1"]][["output"]] <- poly
  object[["layer_2"]][["input"]] <- poly
  object[["layer_2"]][["output"]] <- poly

  class(object) <- "nn2poly"

  newdata <- c(1,2)
  prediction <- predict(object, newdata)
  # All polys are the same so we loop over them
  for (pred_layer in prediction){
    for (pred_layer_i in pred_layer)
      expect_equal(pred_layer_i, t(as.matrix(c(0,6))))
  }

  # Monomials:
  prediction_monomials <- predict(object, newdata, monomials = TRUE)

  # Build expected
  A <- array(0, dim=c(1,3,2))
  A[,,1] <- c(1,-2,1)
  A[,,2] <- c(2,6,-2)
  # All monomials are the same so we loop over them
  for (pred_layer in prediction_monomials){
    for (pred_layer_i in pred_layer)
      expect_equal(pred_layer_i, A)
  }
})




test_that("Multiple layers: choosing last layer returns the same as evaluation
          on single single polynomial", {
  # Define a poly object with 2 polynomials
  poly <- list()
  poly$values <- matrix(c(1,-1,1,
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1),c(2),c(1,1))

  # Replicate that polynomial 4 times as if it was the output
  # of a 1 hidden layer NN
  object <- list()
  object[["layer_1"]][["input"]] <- poly
  object[["layer_1"]][["output"]] <- poly
  object[["layer_2"]][["input"]] <- poly
  object[["layer_2"]][["output"]] <- poly

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
                          2,3,-2), ncol = 2, byrow = FALSE)
  poly$labels <- list(c(1),c(2),c(1,1))

  # Replicate that polynomial 4 times as if it was the output
  # of a 1 hidden layer NN
  object <- list()
  object[["layer_1"]][["input"]] <- poly
  object[["layer_1"]][["output"]] <- poly
  object[["layer_2"]][["input"]] <- poly
  object[["layer_2"]][["output"]] <- poly

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


test_that("Multiple layers: output from nn2poly also works", {
  # This tests is here to detect if nn2poly output changes, as the previous
  # multiple layers tests are created with manual polynomials (needed to control)
  # the output.

  # In this test we instead obtain the polynomial from using nn2poly

  testing_data <- testing_helper_1()

  # Get the needed data
  object <- testing_data$weights_list
  names(object) <- testing_data$af_string_list
  taylor_orders <- testing_data$taylor_orders

  result <- nn2poly(
    object = object,
    max_order = 3,
    keep_layers = TRUE,
    taylor_orders = taylor_orders
  )

  newdata <- c(1,2)

  # Some errors
  expect_error(predict(result, newdata, layers = c(1,4)))
  expect_error(predict(result, newdata, layers = FALSE))

  # Choose a layer and obtain the same as with full layers:
  prediction1 <- predict(result, newdata, layers = 2)

  # Get prediction for a single polynomial
  prediction2 <- eval_poly(result$layer_2$output, newdata)

  expect_equal(prediction1$layer_2$output, prediction2)

  # Also test that layer_1 is null if not chosen
  expect_null(prediction1$layer_1)

})





