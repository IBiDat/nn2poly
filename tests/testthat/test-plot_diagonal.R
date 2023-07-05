test_that("plot_diagonal for two arbitraty sequence of numbers", {
  # generating two sequences of numbers
  a <- 1:20
  b <- seq(from = 1, to = 39, by = 2)

  # computing the plot
  p <- plot_diagonal(a, b, xlab = "this is a test", ylab = "yes, this is a test")

  # testing the plot
  vdiffr::expect_doppelganger("Diagonal plot for a and b", p)
})

test_that("plot_diagonal for two arbitraty sequence of numbers
          but without line", {
  # generating two sequences of numbers
  a <- 1:20
  b <- seq(from = 1, to = 39, by = 2)

  # computing the plot
  p <- plot_diagonal(a, b, xlab = "this is a test", ylab = "yes, this is a test",
                    plot.line = FALSE)

  # testing te plot
  vdiffr::expect_doppelganger("Diagonal plot for a and b without line", p)
})
