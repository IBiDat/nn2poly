#' Predict method for \code{nn2poly} objects.
#'
#' Predicted values obtained with a \code{nn2poly} object on given data.
#'
#' @inherit eval_poly
#' @param object Object of class inheriting from 'nn2poly'.
#' @param layers Vector containing the chosen layers from \code{object} to be
#' evaluated. If set to \code{NULL}, all layers are computed. Default is set
#' to \code{NULL}.
#' @param ... 	Further arguments passed to or from other methods.
#'
#' @details
#' Internally uses `eval_poly()` to obtain the predictions. However, this only
#' works with a objects of class \code{nn2poly} while `eval_poly()` can be used
#' with a manually created polynomial in list form.
#'
#' When \code{object} contains all the internal polynomials also, as given by
#' \code{nn2poly(object, keep_layers = TRUE)}, it is important to note that there
#' are two polynomial items per layer (input/output). These polynomial items will
#' also contain several polynomials of the same structure, one per neuron in the
#' layer, stored as matrix rows in \code{$values}. Please see the NN2Poly
#' original paper for more details.
#'
#' Note also that "linear" layers will contain the same input and output results
#' as Taylor expansion is not used and thus the polynomials are also the same.
#' Because of this, in the situation of evaluating multiple layers we provide
#' the final layer with "input" and "output" even if they are the same, for
#' consistency.
#'
#' @seealso [nn2poly()]: function that obtains the \code{nn2poly} polynomial
#' object, [eval_poly()]: function that can evaluate polynomials in general,
#' [stats::predict()]: generic predict function.
#'
#' @return Returns a matrix or list of matrices with the evaluation of each
#' polynomial at each layer as given by the provided \code{object} of class
#' \code{nn2poly}. The format can be as follows, depending on the layers
#' contained in \code{object} and the parameters \code{layers} and \code{monomials} values:
#'
#' - If \code{object} contains the polynomials of the last layer, as given by
#'   \code{nn2poly(object, keep_layers = FALSE)}, then the output is:
#'    - A matrix: if \code{monomials==FALSE}, returns a matrix containing the
#'      evaluation of the polynomials on the given data. The matrix has dimensions
#'      \code{(n_sample, n_polynomials)}, meaning that each column corresponds to the
#'      result of evaluating all the data for a polynomial. If a single polynomial is
#'      provided, the output is a vector instead of a row matrix.
#'    - A 3D array: If \code{monomials==TRUE}, returns a 3D array containing the monomials of
#'      each polynomial evaluated on the given data. The array has dimensions
#'      \code{(n_sample, n_monomial_terms, n_polynomials)}, where element
#'      \code{[i,j,k]} contains the evaluation on observation \code{i} on
#'      monomial \code{j} of polynomial \code{k}, where monomial \code{j} corresponds
#'      to the one on \code{poly$labels[[j]]}.
#'- If \code{object} contains all the internal polynomials, as given by
#' \code{nn2poly(object, keep_layers = TRUE)}, then the output is a list of
#' layers (represented by \code{layer_i}), where each of them is another list with
#' \code{input} and \code{output} elements. Each of those elements contains the
#' corresponding evaluation of the "input" or "output" polynomial at the given layer,
#' as explained in the last layer case, which will be a matrix if \code{monomials==FALSE}
#' and a 3D array if \code{monomials==TRUE}.
#'
#' @examples
#' # Build a NN structure with random weights, with 2 (+ bias) inputs,
#' # 4 (+bias) neurons in the first hidden layer with "tanh" activation
#' # function, 4 (+bias) neurons in the second hidden layer with "softplus",
#' # and 1 "linear" output unit
#'
#' weights_layer_1 <- matrix(rnorm(12), nrow = 3, ncol = 4)
#' weights_layer_2 <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' weights_layer_3 <- matrix(rnorm(5), nrow = 5, ncol = 1)
#'
#' # Set it as a list with activation functions as names
#' nn_object = list("tanh" = weights_layer_1,
#'                  "softplus" = weights_layer_2,
#'                  "linear" = weights_layer_3)
#'
#' # Obtain the polynomial representation (order = 3) of that neural network
#' final_poly <- nn2poly(nn_object, max_order = 3)
#'
#' # Define some new data, it can be vector, matrix or dataframe
#' newdata <- matrix(rnorm(10), ncol = 2, nrow = 5)
#'
#' # Predict using the obtained polynomial
#' predict(object = final_poly, newdata = newdata)
#'
#' # Predict the values of each monomial of the obtained polynomial
#' predict(object = final_poly, newdata = newdata, monomials = TRUE)
#'
#' # Change the last layer to have 3 outputs (as in a multiclass classification)
#' # problem
#' weights_layer_4 <- matrix(rnorm(20), nrow = 5, ncol = 4)
#'
#' # Set it as a list with activation functions as names
#' nn_object = list("tanh" = weights_layer_1,
#'                  "softplus" = weights_layer_2,
#'                  "linear" = weights_layer_4)
#'
#' # Obtain the polynomial representation of that neural network
#' # Polynomial representation of each hidden neuron is given by
#' final_poly <- nn2poly(nn_object, max_order = 3, keep_layers = TRUE)
#'
#' # Define some new data, it can be vector, matrix or dataframe
#' newdata <- matrix(rnorm(10), ncol = 2, nrow = 5)
#'
#' # Predict using the obtained polynomials (for all layers)
#' predict(object = final_poly, newdata = newdata)
#'
#' # Predict using the obtained polynomials (for chosen layers)
#' predict(object = final_poly, newdata = newdata, layers = c(2,3))
#'
#' @export
predict.nn2poly <- function(object,
                            newdata,
                            monomials = FALSE,
                            layers = NULL,
                            ...) {
  if (length(class(object)) > 1)
    return(NextMethod())

  # Check if object is a single polynomial or a list of polynomials.
  # If we get only the output layer, then it has to be a list with 2 elements,
  # values and labels. We check one of them:
  if (!is.null(object$labels)){
    # If we have a final polynomial, directly evaluate the results:
    result <- eval_poly(poly = object, newdata = newdata, monomials = monomials)
  } else {
    # Multiple layer case:

    # If layer = NULL, set all layers to be used
    if (is.null(layers)){
      layers <- 1:length(object)
    }

    # Check if a vector or number is given
    if (!(is.atomic(layers) & is.numeric(layers))){
      stop("Argument layers is neither a numeric vector nor NULL.",
           call. = FALSE
      )
    }
    # Check that selected layers are within object dimension
    # To do so, we need to check if "layer_maxvalue" exists:
    final_layer <- paste0("layer_", max(layers))
    if (is.null(object[[final_layer]])){
      stop("Argument layers contains elements that exceed number of layers in nn2poly object.",
           call. = FALSE
      )
    }

    # Make sure layers are ordered, just for consistent output
    layers <- sort(layers)

    # Compute results for the given layers.
    result <- list()
    for (i in layers){

      layer_name <- paste0("layer_", i)
      result[[layer_name]] <- list()

      result[[layer_name]][["input"]] <-
        eval_poly(poly = object[[layer_name]][["input"]],
                  newdata = newdata,
                  monomials = monomials)

      result[[layer_name]][["output"]] <-
        eval_poly(poly = object[[layer_name]][["output"]],
                  newdata = newdata,
                  monomials = monomials)
    }
  }

  return(result)

}

#' Plot method for \code{nn2poly} objects.
#'
#' A function that takes a polynomial (or several ones) as given by the
#' \pkg{nn2poly} algorithm, and then plots their absolute magnitude as barplots
#' to be able to compare the most important coefficients.
#'
#' @param x A \code{nn2poly} object, as returned by the \pkg{nn2poly} algorithm.
#' @param ... Ignored.
#' @param n An integer denoting the number of coefficients to be plotted,
#' after ordering them by absolute magnitude.
#'
#' @return A plot showing the \code{n} most important coefficients.
#'
#' @details
#' The plot method represents only the polynomials at the final layer, even if
#' `x` is generated using `nn2poly()` with `keep_layers=TRUE`.
#'
#' @examples
#' # --- Single polynomial output ---
#' # Build a NN structure with random weights, with 2 (+ bias) inputs,
#' # 4 (+bias) neurons in the first hidden layer with "tanh" activation
#' # function, 4 (+bias) neurons in the second hidden layer with "softplus",
#' # and 2 "linear" output units
#'
#' weights_layer_1 <- matrix(rnorm(12), nrow = 3, ncol = 4)
#' weights_layer_2 <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' weights_layer_3 <- matrix(rnorm(5), nrow = 5, ncol = 1)
#'
#' # Set it as a list with activation functions as names
#' nn_object = list("tanh" = weights_layer_1,
#'                  "softplus" = weights_layer_2,
#'                  "linear" = weights_layer_3)
#'
#' # Obtain the polynomial representation (order = 3) of that neural network
#' final_poly <- nn2poly(nn_object, max_order = 3)
#'
#' # Plot all the coefficients, one plot per output unit
#' plot(final_poly)
#'
#' # Plot only the 5 most important coeffcients (by absolute magnitude)
#' # one plot per output unit
#' plot(final_poly, n = 5)
#'
#' # --- Multiple output polynomials ---
#' # Build a NN structure with random weights, with 2 (+ bias) inputs,
#' # 4 (+bias) neurons in the first hidden layer with "tanh" activation
#' # function, 4 (+bias) neurons in the second hidden layer with "softplus",
#' # and 2 "linear" output units
#'
#' weights_layer_1 <- matrix(rnorm(12), nrow = 3, ncol = 4)
#' weights_layer_2 <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' weights_layer_3 <- matrix(rnorm(10), nrow = 5, ncol = 2)
#'
#' # Set it as a list with activation functions as names
#' nn_object = list("tanh" = weights_layer_1,
#'                  "softplus" = weights_layer_2,
#'                  "linear" = weights_layer_3)
#'
#' # Obtain the polynomial representation (order = 3) of that neural network
#' final_poly <- nn2poly(nn_object, max_order = 3)
#'
#' # Plot all the coefficients, one plot per output unit
#' plot(final_poly)
#'
#' # Plot only the 5 most important coeffcients (by absolute magnitude)
#' # one plot per output unit
#' plot(final_poly, n = 5)
#'
#' @export
plot.nn2poly <- function(x, ..., n=NULL) {
  if (length(class(x)) > 1)
    return(NextMethod())

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package 'ggplot2' is required for this functionality", call. = FALSE)
  }

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("package 'patchwork' is required for this functionality", call. = FALSE)
  }

  # a special case is needed for the case in which the polynomial was generated
  # with `keep_layers = TRUE`

  if (is.null(x$values)) {
    x <- x[[length(x)]][["output"]]
  }

  # Check if x$values is a vector and transform it into a column matrix
  if (is.vector(x$values)){
    x$values <- matrix(x$values, ncol = 1)
  }

  if (is.null(n)) {
    n <- dim(x$values)[1]
  }

  # Transpose values to be polynomials as rows instead of columns
  # Needed to work as in previous nn2poly output format
  M <- t(x$values)
  all_labels <- x$labels
  n_polys <- nrow(M)

  all_df <- data.frame()

  for (r in 1:n_polys) {
    Mr <- M[r, ]
    aux_total <- sort(abs(Mr), decreasing = TRUE, index.return = TRUE)
    aux_values <- aux_total$x[1:n]
    aux_index <- aux_total$ix[1:n]

    # Obtain labels of chosen coefficients:
    list_labels <- all_labels[aux_index]

    string_labels <- rep("0", n)
    for (i in 1:n) {
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

  # inspired by tidytext::reorder_within
  new_x <- do.call(paste, c(list(all_df$name, sep = "___"), list(all_df$type)))
  reorder_aux <- stats::reorder(new_x, all_df$value, FUN = mean, decreasing = TRUE)

  # inspired by tidytext::scale_x_reordered and tidtytext::reorder_func
  reorder_func <- function(x, sep = "___") {
    reg <- paste0(sep, ".+$")
    gsub(reg, "", x)
  }


  plot_all <- ggplot2::ggplot(all_df,
                              ggplot2::aes(x = reorder_aux,
                                           y = .data$value,
                                           fill = .data$sign)) +
    ggplot2::geom_bar(stat = "identity", colour = "black", alpha = 1) +
    ggplot2::scale_x_discrete(labels = reorder_func)

  if (n_polys >1){
    plot_all <- plot_all + ggplot2::facet_wrap(~type, scales = "free_x")
  }

  plot_all <- plot_all +
    cowplot::theme_half_open() +
    ggplot2::labs(y = "Coefficient (absolute) values", x = "Variables or interactions") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::scale_fill_manual(values = scale_values, labels = scale_labels) +
    ggplot2::theme(legend.direction = "horizontal") +
    ggplot2::labs(fill = "Sign")


  return(plot_all)
}
