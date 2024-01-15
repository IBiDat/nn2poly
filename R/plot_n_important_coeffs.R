#' Plot n most important coefficients.
#'
#' A function that takes a polynomial (or several ones) as given by the
#' \pkg{nn2poly} algorithm, and then plots their absolute magnitude as barplots
#' to be able to compare the most important coefficients. The number of plotted
#' coefficients is controlled by `n_important_coeffs`.
#'
#' @param poly A polynomial represented as a list with "labels" and "values",
#' in the same manner as returned by \pkg{nn2poly} algorithm.
#' @param n_important_coeffs An integer denoting the number of coefficients to
#' be plotted, after ordering them by absolute magnitude.
#'
#' @return A plot showing the n most important coefficients.
#'
#' @export
plot_n_important_coeffs <- function(poly, n_important_coeffs) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package 'ggplot2' is required for this functionality", call. = FALSE)
  }

  if (!requireNamespace("tidytext", quietly = TRUE)) {
    stop("package 'tidytext' is required for this functionality", call. = FALSE)
  }

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("package 'patchwork' is required for this functionality", call. = FALSE)
  }

  # Check if poly$values is a vector and transform it into a column matrix
  if (is.vector(poly$values)){
    poly$values <- matrix(poly$values, ncol = 1)
  }

  # a special case is needed for the case in which the polynomial was generated
  # with `keep_layers = TRUE`

  if (is.null(poly$values)) {
    poly <- poly[[length(poly)]][["output"]]
  }

  # Transpose values to be polynomials as rows instead of columns
  # Needed to work as in previous nn2poly output format
  M <- t(poly$values)
  all_labels <- poly$labels

  all_df <- data.frame()

  for (r in 1:nrow(M)) {
    Mr <- M[r, ]
    aux_total <- sort(abs(Mr), decreasing = TRUE, index.return = TRUE)
    aux_values <- aux_total$x[1:n_important_coeffs]
    aux_index <- aux_total$ix[1:n_important_coeffs]

    # Obtain labels of chosen coefficients:
    list_labels <- all_labels[aux_index]

    string_labels <- rep("0", n_important_coeffs)
    for (i in 1:n_important_coeffs) {
      # Create the label as a string of the form "l_1 l_2 ... l_t"
      string_labels[i] <- paste(as.character(list_labels[[i]]), collapse = ",")
    }

    aux_sign <- sign(Mr)[aux_index]

    df <- data.frame(
      name = string_labels,
      sign = as.factor(aux_sign),
      value = aux_values,
      type = r
    )

    all_df <- rbind(all_df, df)
  }

  # If a coefficient is exactly 0, assign it to positive
  if (any(all_df$sign == 0)){
    all_df$sign[which(all_df$sign==0)] = 1
  }



  # Define different scale for multiple or single sign cases.
  if (all(levels(all_df$sign) == c("-1", "1"))){
    scale_values <- c("#F8766D", "#00BA38")
    scale_labels <- c("-", "+")
  } else if (levels(all_df$sign) == c("1")) {
    scale_values <- c("#00BA38")
    scale_labels <- c("+")
  } else if (levels(all_df$sign) == c("-1")) {
    scale_values <- c("#F8766D")
    scale_labels <- c("-")
  }


  plot_all <- ggplot2::ggplot(all_df,
                              ggplot2::aes(x = tidytext::reorder_within(x = .data$name,
                                                                        by = -.data$value,
                                                                        within = .data$type),
                                           y = .data$value,
                                           fill = .data$sign)) +
    ggplot2::geom_bar(stat = "identity", colour = "black", alpha = 1) +
    tidytext::scale_x_reordered() +
    ggplot2::facet_wrap(~type, scales = "free_x") +
    cowplot::theme_half_open() +
    ggplot2::labs(y = "Coefficient (absolute) values", x = "Variables or interactions") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::scale_fill_manual(values = scale_values, labels = scale_labels) +
    ggplot2::theme(legend.direction = "horizontal") +
    ggplot2::labs(fill = "Sign")


  return(plot_all)
}
