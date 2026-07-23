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

smoke_test_manual <- function(data, taylor_center=0) {

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

  # Fifth-order Taylor expansion of tanh at z = taylor_center ---------------
  #
  # tanh(z) is approximated by
  #
  #   tau0 + tau1*q + tau2*q^2 + tau3*q^3 + tau4*q^4 + tau5*q^5,
  #
  # where q = z - taylor_center and tau_k = tanh^(k)(taylor_center) / k!.

  tanh_center <- tanh(taylor_center)

  tau0 <- tanh_center
  tau1 <- 1 - tanh_center^2
  tau2 <- (-2 * tanh_center + 2 * tanh_center^3) / 2
  tau3 <- (-2 + 8 * tanh_center^2 - 6 * tanh_center^4) / 6
  tau4 <- (
    16 * tanh_center - 40 * tanh_center^3 + 24 * tanh_center^5
  ) / 24
  tau5 <- (
    16 - 136 * tanh_center^2 + 240 * tanh_center^4 -
      120 * tanh_center^6
  ) / 120

  taylor_coefficients <- c(
    order_0 = tau0,
    order_1 = tau1,
    order_2 = tau2,
    order_3 = tau3,
    order_4 = tau4,
    order_5 = tau5
  )

  # Shifted intercepts used only in q = z - taylor_center.

  a0_centered <- a0 - taylor_center
  b0_centered <- b0 - taylor_center

  # Hidden neuron 1: c -----------------------------------------------------

  # Degree 0.

  c0 <- tau0 +
    tau1 * a0_centered +
    tau2 * a0_centered^2 +
    tau3 * a0_centered^3 +
    tau4 * a0_centered^4 +
    tau5 * a0_centered^5

  # Degree 1. Multiplicities in q^1,...,q^5: 1, 2, 3, 4, 5.

  c1 <- tau1 * a1 +
    tau2 * (2 * a0_centered * a1) +
    tau3 * (3 * a0_centered^2 * a1) +
    tau4 * (4 * a0_centered^3 * a1) +
    tau5 * (5 * a0_centered^4 * a1)
  c2 <- tau1 * a2 +
    tau2 * (2 * a0_centered * a2) +
    tau3 * (3 * a0_centered^2 * a2) +
    tau4 * (4 * a0_centered^3 * a2) +
    tau5 * (5 * a0_centered^4 * a2)
  c3 <- tau1 * a3 +
    tau2 * (2 * a0_centered * a3) +
    tau3 * (3 * a0_centered^2 * a3) +
    tau4 * (4 * a0_centered^3 * a3) +
    tau5 * (5 * a0_centered^4 * a3)

  # Degree 2.
  # Repeated variable (ii): 1, 3, 6, 10 in q^2,...,q^5.
  # Distinct variables (ij): 2, 6, 12, 20 in q^2,...,q^5.

  c11 <- tau2 * a1^2 +
    tau3 * (3 * a0_centered * a1^2) +
    tau4 * (6 * a0_centered^2 * a1^2) +
    tau5 * (10 * a0_centered^3 * a1^2)
  c12 <- tau2 * (2 * a1 * a2) +
    tau3 * (6 * a0_centered * a1 * a2) +
    tau4 * (12 * a0_centered^2 * a1 * a2) +
    tau5 * (20 * a0_centered^3 * a1 * a2)
  c13 <- tau2 * (2 * a1 * a3) +
    tau3 * (6 * a0_centered * a1 * a3) +
    tau4 * (12 * a0_centered^2 * a1 * a3) +
    tau5 * (20 * a0_centered^3 * a1 * a3)
  c22 <- tau2 * a2^2 +
    tau3 * (3 * a0_centered * a2^2) +
    tau4 * (6 * a0_centered^2 * a2^2) +
    tau5 * (10 * a0_centered^3 * a2^2)
  c23 <- tau2 * (2 * a2 * a3) +
    tau3 * (6 * a0_centered * a2 * a3) +
    tau4 * (12 * a0_centered^2 * a2 * a3) +
    tau5 * (20 * a0_centered^3 * a2 * a3)
  c33 <- tau2 * a3^2 +
    tau3 * (3 * a0_centered * a3^2) +
    tau4 * (6 * a0_centered^2 * a3^2) +
    tau5 * (10 * a0_centered^3 * a3^2)

  # Degree 3.
  # Patterns iii, iij and ijk have multiplicities (1,4,10), (3,12,30)
  # and (6,24,60), respectively, in q^3, q^4 and q^5.

  c111 <- tau3 * a1^3 +
    tau4 * (4 * a0_centered * a1^3) +
    tau5 * (10 * a0_centered^2 * a1^3)
  c112 <- tau3 * (3 * a1^2 * a2) +
    tau4 * (12 * a0_centered * a1^2 * a2) +
    tau5 * (30 * a0_centered^2 * a1^2 * a2)
  c113 <- tau3 * (3 * a1^2 * a3) +
    tau4 * (12 * a0_centered * a1^2 * a3) +
    tau5 * (30 * a0_centered^2 * a1^2 * a3)
  c122 <- tau3 * (3 * a1 * a2^2) +
    tau4 * (12 * a0_centered * a1 * a2^2) +
    tau5 * (30 * a0_centered^2 * a1 * a2^2)
  c123 <- tau3 * (6 * a1 * a2 * a3) +
    tau4 * (24 * a0_centered * a1 * a2 * a3) +
    tau5 * (60 * a0_centered^2 * a1 * a2 * a3)
  c133 <- tau3 * (3 * a1 * a3^2) +
    tau4 * (12 * a0_centered * a1 * a3^2) +
    tau5 * (30 * a0_centered^2 * a1 * a3^2)
  c222 <- tau3 * a2^3 +
    tau4 * (4 * a0_centered * a2^3) +
    tau5 * (10 * a0_centered^2 * a2^3)
  c223 <- tau3 * (3 * a2^2 * a3) +
    tau4 * (12 * a0_centered * a2^2 * a3) +
    tau5 * (30 * a0_centered^2 * a2^2 * a3)
  c233 <- tau3 * (3 * a2 * a3^2) +
    tau4 * (12 * a0_centered * a2 * a3^2) +
    tau5 * (30 * a0_centered^2 * a2 * a3^2)
  c333 <- tau3 * a3^3 +
    tau4 * (4 * a0_centered * a3^3) +
    tau5 * (10 * a0_centered^2 * a3^3)

  # Degree 4.
  # Patterns iiii, iiij, iijj and iijk have multiplicities (1,5), (4,20),
  # (6,30) and (12,60), respectively, in q^4 and q^5.

  c1111 <- tau4 * a1^4 +
    tau5 * (5 * a0_centered * a1^4)
  c1112 <- tau4 * (4 * a1^3 * a2) +
    tau5 * (20 * a0_centered * a1^3 * a2)
  c1113 <- tau4 * (4 * a1^3 * a3) +
    tau5 * (20 * a0_centered * a1^3 * a3)
  c1122 <- tau4 * (6 * a1^2 * a2^2) +
    tau5 * (30 * a0_centered * a1^2 * a2^2)
  c1123 <- tau4 * (12 * a1^2 * a2 * a3) +
    tau5 * (60 * a0_centered * a1^2 * a2 * a3)
  c1133 <- tau4 * (6 * a1^2 * a3^2) +
    tau5 * (30 * a0_centered * a1^2 * a3^2)
  c1222 <- tau4 * (4 * a1 * a2^3) +
    tau5 * (20 * a0_centered * a1 * a2^3)
  c1223 <- tau4 * (12 * a1 * a2^2 * a3) +
    tau5 * (60 * a0_centered * a1 * a2^2 * a3)
  c1233 <- tau4 * (12 * a1 * a2 * a3^2) +
    tau5 * (60 * a0_centered * a1 * a2 * a3^2)
  c1333 <- tau4 * (4 * a1 * a3^3) +
    tau5 * (20 * a0_centered * a1 * a3^3)
  c2222 <- tau4 * a2^4 +
    tau5 * (5 * a0_centered * a2^4)
  c2223 <- tau4 * (4 * a2^3 * a3) +
    tau5 * (20 * a0_centered * a2^3 * a3)
  c2233 <- tau4 * (6 * a2^2 * a3^2) +
    tau5 * (30 * a0_centered * a2^2 * a3^2)
  c2333 <- tau4 * (4 * a2 * a3^3) +
    tau5 * (20 * a0_centered * a2 * a3^3)
  c3333 <- tau4 * a3^4 +
    tau5 * (5 * a0_centered * a3^4)

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

  # Hidden neuron 2: d -----------------------------------------------------

  # Degree 0.

  d0 <- tau0 +
    tau1 * b0_centered +
    tau2 * b0_centered^2 +
    tau3 * b0_centered^3 +
    tau4 * b0_centered^4 +
    tau5 * b0_centered^5

  # Degree 1. Multiplicities in q^1,...,q^5: 1, 2, 3, 4, 5.

  d1 <- tau1 * b1 +
    tau2 * (2 * b0_centered * b1) +
    tau3 * (3 * b0_centered^2 * b1) +
    tau4 * (4 * b0_centered^3 * b1) +
    tau5 * (5 * b0_centered^4 * b1)
  d2 <- tau1 * b2 +
    tau2 * (2 * b0_centered * b2) +
    tau3 * (3 * b0_centered^2 * b2) +
    tau4 * (4 * b0_centered^3 * b2) +
    tau5 * (5 * b0_centered^4 * b2)
  d3 <- tau1 * b3 +
    tau2 * (2 * b0_centered * b3) +
    tau3 * (3 * b0_centered^2 * b3) +
    tau4 * (4 * b0_centered^3 * b3) +
    tau5 * (5 * b0_centered^4 * b3)

  # Degree 2.

  d11 <- tau2 * b1^2 +
    tau3 * (3 * b0_centered * b1^2) +
    tau4 * (6 * b0_centered^2 * b1^2) +
    tau5 * (10 * b0_centered^3 * b1^2)
  d12 <- tau2 * (2 * b1 * b2) +
    tau3 * (6 * b0_centered * b1 * b2) +
    tau4 * (12 * b0_centered^2 * b1 * b2) +
    tau5 * (20 * b0_centered^3 * b1 * b2)
  d13 <- tau2 * (2 * b1 * b3) +
    tau3 * (6 * b0_centered * b1 * b3) +
    tau4 * (12 * b0_centered^2 * b1 * b3) +
    tau5 * (20 * b0_centered^3 * b1 * b3)
  d22 <- tau2 * b2^2 +
    tau3 * (3 * b0_centered * b2^2) +
    tau4 * (6 * b0_centered^2 * b2^2) +
    tau5 * (10 * b0_centered^3 * b2^2)
  d23 <- tau2 * (2 * b2 * b3) +
    tau3 * (6 * b0_centered * b2 * b3) +
    tau4 * (12 * b0_centered^2 * b2 * b3) +
    tau5 * (20 * b0_centered^3 * b2 * b3)
  d33 <- tau2 * b3^2 +
    tau3 * (3 * b0_centered * b3^2) +
    tau4 * (6 * b0_centered^2 * b3^2) +
    tau5 * (10 * b0_centered^3 * b3^2)

  # Degree 3.

  d111 <- tau3 * b1^3 +
    tau4 * (4 * b0_centered * b1^3) +
    tau5 * (10 * b0_centered^2 * b1^3)
  d112 <- tau3 * (3 * b1^2 * b2) +
    tau4 * (12 * b0_centered * b1^2 * b2) +
    tau5 * (30 * b0_centered^2 * b1^2 * b2)
  d113 <- tau3 * (3 * b1^2 * b3) +
    tau4 * (12 * b0_centered * b1^2 * b3) +
    tau5 * (30 * b0_centered^2 * b1^2 * b3)
  d122 <- tau3 * (3 * b1 * b2^2) +
    tau4 * (12 * b0_centered * b1 * b2^2) +
    tau5 * (30 * b0_centered^2 * b1 * b2^2)
  d123 <- tau3 * (6 * b1 * b2 * b3) +
    tau4 * (24 * b0_centered * b1 * b2 * b3) +
    tau5 * (60 * b0_centered^2 * b1 * b2 * b3)
  d133 <- tau3 * (3 * b1 * b3^2) +
    tau4 * (12 * b0_centered * b1 * b3^2) +
    tau5 * (30 * b0_centered^2 * b1 * b3^2)
  d222 <- tau3 * b2^3 +
    tau4 * (4 * b0_centered * b2^3) +
    tau5 * (10 * b0_centered^2 * b2^3)
  d223 <- tau3 * (3 * b2^2 * b3) +
    tau4 * (12 * b0_centered * b2^2 * b3) +
    tau5 * (30 * b0_centered^2 * b2^2 * b3)
  d233 <- tau3 * (3 * b2 * b3^2) +
    tau4 * (12 * b0_centered * b2 * b3^2) +
    tau5 * (30 * b0_centered^2 * b2 * b3^2)
  d333 <- tau3 * b3^3 +
    tau4 * (4 * b0_centered * b3^3) +
    tau5 * (10 * b0_centered^2 * b3^3)

  # Degree 4.

  d1111 <- tau4 * b1^4 +
    tau5 * (5 * b0_centered * b1^4)
  d1112 <- tau4 * (4 * b1^3 * b2) +
    tau5 * (20 * b0_centered * b1^3 * b2)
  d1113 <- tau4 * (4 * b1^3 * b3) +
    tau5 * (20 * b0_centered * b1^3 * b3)
  d1122 <- tau4 * (6 * b1^2 * b2^2) +
    tau5 * (30 * b0_centered * b1^2 * b2^2)
  d1123 <- tau4 * (12 * b1^2 * b2 * b3) +
    tau5 * (60 * b0_centered * b1^2 * b2 * b3)
  d1133 <- tau4 * (6 * b1^2 * b3^2) +
    tau5 * (30 * b0_centered * b1^2 * b3^2)
  d1222 <- tau4 * (4 * b1 * b2^3) +
    tau5 * (20 * b0_centered * b1 * b2^3)
  d1223 <- tau4 * (12 * b1 * b2^2 * b3) +
    tau5 * (60 * b0_centered * b1 * b2^2 * b3)
  d1233 <- tau4 * (12 * b1 * b2 * b3^2) +
    tau5 * (60 * b0_centered * b1 * b2 * b3^2)
  d1333 <- tau4 * (4 * b1 * b3^3) +
    tau5 * (20 * b0_centered * b1 * b3^3)
  d2222 <- tau4 * b2^4 +
    tau5 * (5 * b0_centered * b2^4)
  d2223 <- tau4 * (4 * b2^3 * b3) +
    tau5 * (20 * b0_centered * b2^3 * b3)
  d2233 <- tau4 * (6 * b2^2 * b3^2) +
    tau5 * (30 * b0_centered * b2^2 * b3^2)
  d2333 <- tau4 * (4 * b2 * b3^3) +
    tau5 * (20 * b0_centered * b2 * b3^3)
  d3333 <- tau4 * b3^4 +
    tau5 * (5 * b0_centered * b3^4)

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

  # Linear output: e -------------------------------------------------------

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

  # NN2Poly keep_layers structure -----------------------------------------
  # Labels are integer vectors, matching the representation used by nn2poly.

  input_labels <- list(0L, 1L, 2L, 3L)

  polynomial_labels <- list(
    0L,
    1L, 2L, 3L,
    c(1L, 1L), c(1L, 2L), c(1L, 3L),
    c(2L, 2L), c(2L, 3L), c(3L, 3L),
    c(1L, 1L, 1L), c(1L, 1L, 2L), c(1L, 1L, 3L),
    c(1L, 2L, 2L), c(1L, 2L, 3L), c(1L, 3L, 3L),
    c(2L, 2L, 2L), c(2L, 2L, 3L), c(2L, 3L, 3L),
    c(3L, 3L, 3L),
    c(1L, 1L, 1L, 1L), c(1L, 1L, 1L, 2L),
    c(1L, 1L, 1L, 3L), c(1L, 1L, 2L, 2L),
    c(1L, 1L, 2L, 3L), c(1L, 1L, 3L, 3L),
    c(1L, 2L, 2L, 2L), c(1L, 2L, 2L, 3L),
    c(1L, 2L, 3L, 3L), c(1L, 3L, 3L, 3L),
    c(2L, 2L, 2L, 2L), c(2L, 2L, 2L, 3L),
    c(2L, 2L, 3L, 3L), c(2L, 3L, 3L, 3L),
    c(3L, 3L, 3L, 3L)
  )

  layer_1_input_values <- matrix(
    c(a0, a1, a2, a3, b0, b1, b2, b3),
    nrow = 4L,
    ncol = 2L,
    dimnames = NULL
  )

  layer_1_output_values <- matrix(
    c(unname(coefficients_c), unname(coefficients_d)),
    nrow = length(polynomial_labels),
    ncol = 2L,
    dimnames = NULL
  )

  layer_2_values <- matrix(
    unname(coefficients_e),
    nrow = length(polynomial_labels),
    ncol = 1L,
    dimnames = NULL
  )

  layer_2_polynomial <- list(
    labels = polynomial_labels,
    values = layer_2_values
  )

  obj <- list(
    layer_1 = list(
      input = list(
        labels = input_labels,
        values = layer_1_input_values
      ),
      output = list(
        labels = polynomial_labels,
        values = layer_1_output_values
      )
    ),
    layer_2 = list(
      input = layer_2_polynomial,
      output = layer_2_polynomial
    )
  )
  class(obj) <- "nn2poly"
  obj
}

test_that("manual example passes", {
  data <- smoke_test_data()

  x <- smoke_test_manual(data)
  y <- nn2poly(data, max_order = 4, keep_layers = TRUE, taylor_orders = 5)
  expect_equal(x, y)

  x <- smoke_test_manual(data, 0.5)
  y <- nn2poly(data, max_order = 4, keep_layers = TRUE,
               taylor_orders = 5, taylor_center = 0.5)
  expect_equal(x, y)
})
