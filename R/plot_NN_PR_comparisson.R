

#' Plots a comparison between NN and PR predictions.
#' If the method doesn't present asymptotic behavior, the point should fall
#' in the red line.
#'
#' @param prediction_PR prediction values obtained with the PR
#' @param prediction_NN prediction values obtained with the NN
#'
#'
#' @return plot (ggplot object)
#' @export
#'

plot_NN_PR_comparison <- function(prediction_PR, prediction_NN) {

  # Join the data in a data frame
  df.plot <- data.frame(prediction_PR, prediction_NN)


  plot <- ggplot2::ggplot(df.plot, ggplot2::aes(x = .data$prediction_PR, y = .data$prediction_NN)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
    ggplot2::labs(y = "Predicted Y with NN") +
    ggplot2::labs(x = "Predicted Y with PR") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10)) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 8), axis.title = ggplot2::element_text(size = 8))

  return(plot)
}
