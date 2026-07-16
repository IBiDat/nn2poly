# Example that is known to fail for some high-order terms in the old
# implementation due to a bug in label to column matching for indexing

smoke_test_data <- function() {
  set.seed(1234)
  list("tanh"   = matrix(rnorm(8, sd=0.3), nrow = 4, ncol = 2),
       "linear" = matrix(rnorm(3, sd=100), nrow = 3, ncol = 1))
}

# Manual NN2Poly example: polynomial order 4, Taylor order 5 ------------
#
# This script computes every coefficient explicitly. It intentionally does
# not use nn2poly, symbolic algebra, loops, or a generic polynomial product.

smoke_test_manual <- function(data) {

  # Network weights ---------------------------------------------------------
  # The first value in each weight vector is the bias.

  a0 <- data$tanh[1, 1] # -0.36211972
  a1 <- data$tanh[2, 1] #  0.08322877
  a2 <- data$tanh[3, 1] #  0.32533235
  a3 <- data$tanh[4, 1] # -0.70370931

  b0 <- data$tanh[1, 2] #  0.1287374
  b1 <- data$tanh[2, 2] #  0.1518168
  b2 <- data$tanh[3, 2] # -0.1724220
  b3 <- data$tanh[4, 2] # -0.1639896

  w30 <- data$linear[1]  # -56.44520 output bias
  w31 <- data$linear[2]  # -89.00378 weight from hidden neuron c
  w32 <- data$linear[3]  # -47.71927 weight from hidden neuron d

  w1 <- c(bias = a0, x1 = a1, x2 = a2, x3 = a3)
  w2 <- c(bias = b0, x1 = b1, x2 = b2, x3 = b3)
  w3 <- c(bias = w30, c = w31, d = w32)

  # Taylor approximation and polynomial truncation -------------------------
  #
  #   T5(z) = z - z^3 / 3 + 2*z^5 / 15.
  #
  # Only monomials in x1, x2, x3 of total degree at most 4 are retained.
  # The factors 3 and 6 below come from the expansion of p^3. The factors
  # 5, 10, 20, 30 and 60 come from the expansion of p^5.

  # Hidden neuron 1: c = T5(a), retaining degree <= 4 ----------------------

  c0 <- a0 - a0^3 / 3 + (2 * a0^5) / 15

  c1 <- a1 - (3 * a0^2 * a1) / 3 + (2 * 5 * a0^4 * a1) / 15
  c2 <- a2 - (3 * a0^2 * a2) / 3 + (2 * 5 * a0^4 * a2) / 15
  c3 <- a3 - (3 * a0^2 * a3) / 3 + (2 * 5 * a0^4 * a3) / 15

  c11 <- -(3 * a0 * a1^2) / 3 + (2 * 10 * a0^3 * a1^2) / 15
  c12 <- -(6 * a0 * a1 * a2) / 3 + (2 * 20 * a0^3 * a1 * a2) / 15
  c13 <- -(6 * a0 * a1 * a3) / 3 + (2 * 20 * a0^3 * a1 * a3) / 15
  c22 <- -(3 * a0 * a2^2) / 3 + (2 * 10 * a0^3 * a2^2) / 15
  c23 <- -(6 * a0 * a2 * a3) / 3 + (2 * 20 * a0^3 * a2 * a3) / 15
  c33 <- -(3 * a0 * a3^2) / 3 + (2 * 10 * a0^3 * a3^2) / 15

  c111 <- -a1^3 / 3 + (2 * 10 * a0^2 * a1^3) / 15
  c112 <- -(3 * a1^2 * a2) / 3 + (2 * 30 * a0^2 * a1^2 * a2) / 15
  c113 <- -(3 * a1^2 * a3) / 3 + (2 * 30 * a0^2 * a1^2 * a3) / 15
  c122 <- -(3 * a1 * a2^2) / 3 + (2 * 30 * a0^2 * a1 * a2^2) / 15
  c123 <- -(6 * a1 * a2 * a3) / 3 + (2 * 60 * a0^2 * a1 * a2 * a3) / 15
  c133 <- -(3 * a1 * a3^2) / 3 + (2 * 30 * a0^2 * a1 * a3^2) / 15
  c222 <- -a2^3 / 3 + (2 * 10 * a0^2 * a2^3) / 15
  c223 <- -(3 * a2^2 * a3) / 3 + (2 * 30 * a0^2 * a2^2 * a3) / 15
  c233 <- -(3 * a2 * a3^2) / 3 + (2 * 30 * a0^2 * a2 * a3^2) / 15
  c333 <- -a3^3 / 3 + (2 * 10 * a0^2 * a3^3) / 15

  c1111 <- (2 * 5 * a0 * a1^4) / 15
  c1112 <- (2 * 20 * a0 * a1^3 * a2) / 15
  c1113 <- (2 * 20 * a0 * a1^3 * a3) / 15
  c1122 <- (2 * 30 * a0 * a1^2 * a2^2) / 15
  c1123 <- (2 * 60 * a0 * a1^2 * a2 * a3) / 15
  c1133 <- (2 * 30 * a0 * a1^2 * a3^2) / 15
  c1222 <- (2 * 20 * a0 * a1 * a2^3) / 15
  c1223 <- (2 * 60 * a0 * a1 * a2^2 * a3) / 15
  c1233 <- (2 * 60 * a0 * a1 * a2 * a3^2) / 15
  c1333 <- (2 * 20 * a0 * a1 * a3^3) / 15
  c2222 <- (2 * 5 * a0 * a2^4) / 15
  c2223 <- (2 * 20 * a0 * a2^3 * a3) / 15
  c2233 <- (2 * 30 * a0 * a2^2 * a3^2) / 15
  c2333 <- (2 * 20 * a0 * a2 * a3^3) / 15
  c3333 <- (2 * 5 * a0 * a3^4) / 15

  coefficients_c <- c(
    "0" = c0,
    "1" = c1, "2" = c2, "3" = c3,
    "11" = c11, "12" = c12, "13" = c13,
    "22" = c22, "23" = c23, "33" = c33,
    "111" = c111, "112" = c112, "113" = c113,
    "122" = c122, "123" = c123, "133" = c133,
    "222" = c222, "223" = c223, "233" = c233,
    "333" = c333,
    "1111" = c1111, "1112" = c1112, "1113" = c1113,
    "1122" = c1122, "1123" = c1123, "1133" = c1133,
    "1222" = c1222, "1223" = c1223, "1233" = c1233,
    "1333" = c1333, "2222" = c2222, "2223" = c2223,
    "2233" = c2233, "2333" = c2333, "3333" = c3333
  )

  # Hidden neuron 2: d = T5(b), retaining degree <= 4 ----------------------

  d0 <- b0 - b0^3 / 3 + (2 * b0^5) / 15

  d1 <- b1 - (3 * b0^2 * b1) / 3 + (2 * 5 * b0^4 * b1) / 15
  d2 <- b2 - (3 * b0^2 * b2) / 3 + (2 * 5 * b0^4 * b2) / 15
  d3 <- b3 - (3 * b0^2 * b3) / 3 + (2 * 5 * b0^4 * b3) / 15

  d11 <- -(3 * b0 * b1^2) / 3 + (2 * 10 * b0^3 * b1^2) / 15
  d12 <- -(6 * b0 * b1 * b2) / 3 + (2 * 20 * b0^3 * b1 * b2) / 15
  d13 <- -(6 * b0 * b1 * b3) / 3 + (2 * 20 * b0^3 * b1 * b3) / 15
  d22 <- -(3 * b0 * b2^2) / 3 + (2 * 10 * b0^3 * b2^2) / 15
  d23 <- -(6 * b0 * b2 * b3) / 3 + (2 * 20 * b0^3 * b2 * b3) / 15
  d33 <- -(3 * b0 * b3^2) / 3 + (2 * 10 * b0^3 * b3^2) / 15

  d111 <- -b1^3 / 3 + (2 * 10 * b0^2 * b1^3) / 15
  d112 <- -(3 * b1^2 * b2) / 3 + (2 * 30 * b0^2 * b1^2 * b2) / 15
  d113 <- -(3 * b1^2 * b3) / 3 + (2 * 30 * b0^2 * b1^2 * b3) / 15
  d122 <- -(3 * b1 * b2^2) / 3 + (2 * 30 * b0^2 * b1 * b2^2) / 15
  d123 <- -(6 * b1 * b2 * b3) / 3 + (2 * 60 * b0^2 * b1 * b2 * b3) / 15
  d133 <- -(3 * b1 * b3^2) / 3 + (2 * 30 * b0^2 * b1 * b3^2) / 15
  d222 <- -b2^3 / 3 + (2 * 10 * b0^2 * b2^3) / 15
  d223 <- -(3 * b2^2 * b3) / 3 + (2 * 30 * b0^2 * b2^2 * b3) / 15
  d233 <- -(3 * b2 * b3^2) / 3 + (2 * 30 * b0^2 * b2 * b3^2) / 15
  d333 <- -b3^3 / 3 + (2 * 10 * b0^2 * b3^3) / 15

  d1111 <- (2 * 5 * b0 * b1^4) / 15
  d1112 <- (2 * 20 * b0 * b1^3 * b2) / 15
  d1113 <- (2 * 20 * b0 * b1^3 * b3) / 15
  d1122 <- (2 * 30 * b0 * b1^2 * b2^2) / 15
  d1123 <- (2 * 60 * b0 * b1^2 * b2 * b3) / 15
  d1133 <- (2 * 30 * b0 * b1^2 * b3^2) / 15
  d1222 <- (2 * 20 * b0 * b1 * b2^3) / 15
  d1223 <- (2 * 60 * b0 * b1 * b2^2 * b3) / 15
  d1233 <- (2 * 60 * b0 * b1 * b2 * b3^2) / 15
  d1333 <- (2 * 20 * b0 * b1 * b3^3) / 15
  d2222 <- (2 * 5 * b0 * b2^4) / 15
  d2223 <- (2 * 20 * b0 * b2^3 * b3) / 15
  d2233 <- (2 * 30 * b0 * b2^2 * b3^2) / 15
  d2333 <- (2 * 20 * b0 * b2 * b3^3) / 15
  d3333 <- (2 * 5 * b0 * b3^4) / 15

  coefficients_d <- c(
    "0" = d0,
    "1" = d1, "2" = d2, "3" = d3,
    "11" = d11, "12" = d12, "13" = d13,
    "22" = d22, "23" = d23, "33" = d33,
    "111" = d111, "112" = d112, "113" = d113,
    "122" = d122, "123" = d123, "133" = d133,
    "222" = d222, "223" = d223, "233" = d233,
    "333" = d333,
    "1111" = d1111, "1112" = d1112, "1113" = d1113,
    "1122" = d1122, "1123" = d1123, "1133" = d1133,
    "1222" = d1222, "1223" = d1223, "1233" = d1233,
    "1333" = d1333, "2222" = d2222, "2223" = d2223,
    "2233" = d2233, "2333" = d2333, "3333" = d3333
  )

  # Linear output: e = w30 + w31*c + w32*d -------------------------------

  e0 <- w30 + w31 * c0 + w32 * d0

  e1 <- w31 * c1 + w32 * d1
  e2 <- w31 * c2 + w32 * d2
  e3 <- w31 * c3 + w32 * d3

  e11 <- w31 * c11 + w32 * d11
  e12 <- w31 * c12 + w32 * d12
  e13 <- w31 * c13 + w32 * d13
  e22 <- w31 * c22 + w32 * d22
  e23 <- w31 * c23 + w32 * d23
  e33 <- w31 * c33 + w32 * d33

  e111 <- w31 * c111 + w32 * d111
  e112 <- w31 * c112 + w32 * d112
  e113 <- w31 * c113 + w32 * d113
  e122 <- w31 * c122 + w32 * d122
  e123 <- w31 * c123 + w32 * d123
  e133 <- w31 * c133 + w32 * d133
  e222 <- w31 * c222 + w32 * d222
  e223 <- w31 * c223 + w32 * d223
  e233 <- w31 * c233 + w32 * d233
  e333 <- w31 * c333 + w32 * d333

  e1111 <- w31 * c1111 + w32 * d1111
  e1112 <- w31 * c1112 + w32 * d1112
  e1113 <- w31 * c1113 + w32 * d1113
  e1122 <- w31 * c1122 + w32 * d1122
  e1123 <- w31 * c1123 + w32 * d1123
  e1133 <- w31 * c1133 + w32 * d1133
  e1222 <- w31 * c1222 + w32 * d1222
  e1223 <- w31 * c1223 + w32 * d1223
  e1233 <- w31 * c1233 + w32 * d1233
  e1333 <- w31 * c1333 + w32 * d1333
  e2222 <- w31 * c2222 + w32 * d2222
  e2223 <- w31 * c2223 + w32 * d2223
  e2233 <- w31 * c2233 + w32 * d2233
  e2333 <- w31 * c2333 + w32 * d2333
  e3333 <- w31 * c3333 + w32 * d3333

  coefficients_e <- c(
    "0" = e0,
    "1" = e1, "2" = e2, "3" = e3,
    "11" = e11, "12" = e12, "13" = e13,
    "22" = e22, "23" = e23, "33" = e33,
    "111" = e111, "112" = e112, "113" = e113,
    "122" = e122, "123" = e123, "133" = e133,
    "222" = e222, "223" = e223, "233" = e233,
    "333" = e333,
    "1111" = e1111, "1112" = e1112, "1113" = e1113,
    "1122" = e1122, "1123" = e1123, "1133" = e1133,
    "1222" = e1222, "1223" = e1223, "1233" = e1233,
    "1333" = e1333, "2222" = e2222, "2223" = e2223,
    "2233" = e2233, "2333" = e2333, "3333" = e3333
  )

  # Final results -----------------------------------------------------------

  obj <- list(
    layer_1 = list(
      input = list(
        labels = list(0, 1, 2, 3),
        values = data$tanh
      ),
      output = list(
        labels = sapply(strsplit(names(coefficients_c), ""), as.numeric),
        values = cbind(unname(coefficients_c), unname(coefficients_d))
      )
    ),
    layer_2 = list(
      input = list(
        labels = sapply(strsplit(names(coefficients_e), ""), as.numeric),
        values = cbind(unname(coefficients_e))
      )
    )
  )
  obj$layer_2$output <- obj$layer_2$input
  class(obj) <- "nn2poly"
  obj
}

test_that("manual example passes", {
  data <- smoke_test_data()
  x <- smoke_test_manual(data)
  y <- nn2poly(data, max_order = 4, taylor = 5, keep_layers = TRUE)
  expect_equal(x, y)
})
