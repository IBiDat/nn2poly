#' Plots a comparison between two sets of points.
#'
#' If the points come from the predictions of an NN and a PM and the line
#' `(plot.line = TRUE)` is displayed, in case the method does exhibit
#' asymptotic behavior, the points should not fall in the line.
#'
#'
#'
#' @param x_axis Values to plot in the `x` axis.
#' @param y_axis Values to plot in the `y` axis.
#' @param xlab Lab of the `x` axis
#' @param ylab Lab of the `y` axis.
#' @param title Title of the plot.
#' @param plot.line If a red line with `slope = 1` and `intercept = 0` should
#'   be plotted.
#'
#' @return Plot (ggplot object).
#'
plot_diagonal <- function(x_axis, y_axis,
                          xlab       = NULL,
                          ylab       = NULL,
                          title      = NULL,
                          plot.line  = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("package 'ggplot2' is required for this functionality", call.=FALSE)

  # Join the data in a data frame
  df.plot <- data.frame("x_axis" = x_axis, "y_axis" = y_axis)


  plot <- ggplot2::ggplot(df.plot, ggplot2::aes(x = .data$x_axis, y = .data$y_axis)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10)) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 8), axis.title = ggplot2::element_text(size = 8))

  # Add the red line if plot.line is true
  plot <- if (plot.line) plot + ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") else plot

  return(plot)
}
