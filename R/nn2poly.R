#' @useDynLib nn2poly, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom stats density
utils::globalVariables(c(".data", "super", "self", "private", "ctx"))
NULL

#' Obtain a polynomial representation from a trained neural network
#'
#' Implements the main NN2Poly algorithm to obtain a polynomial representation
#' of a trained neural network using its weights and Taylor expansion of its
#' activation functions.
#'
#' @param object An object for which the computation of the NN2Poly algorithm is
#' desired. Currently supports models from the following deep learning frameworks:
#' - \code{tensorflow}/\code{keras} models built as a sequential model.
#' - \code{torch}/\code{luz} models built as a sequential model.
#'
#' It also supports a named \code{list} as input which allows to introduce by
#' hand a model from any other source. This \code{list} should be of length L
#' (number of hidden layers + 1) containing the weights matrix for each layer.
#' Each element of the list should be names as the activation function used at
#' each layer.
#'
#' At any layer \eqn{l}, the expected shape of such matrices is of the form
#' \eqn{(h_{(l-1)} + 1)*(h_l)}, that is, the number of rows is the number of
#' neurons in the previous layer plus the bias vector, and the number of columns
#' is the number of neurons in the current layer L. Therefore, each column
#' corresponds to the weight vector affecting each neuron in that layer.
#' The bias vector should be in the first row.
#'
#' @param max_order \code{integer} that determines the maximum order
#' that will be forced in the final polynomial, discarding terms of higher order
#' that would naturally arise when considering all Taylor expansions allowed by
#' \code{taylor_orders}.
#'
#' @param keep_layers Boolean that determines if all polynomials computed in
#' the internal layers have to be stored and given in the output (\code{TRUE}),
#' or if only the polynomials from the last layer are needed (\code{FALSE}).
#'
#' @param taylor_orders \code{integer} or \code{vector} of length L that sets the
#' degree at which Taylor expansion is truncated at each layer. If a single
#' value is used, that value is set for each non linear layer and 1 for linear
#  layers. If a vector is used, each value corresponds to the Taylor order used
#' at each layer activation function. Default is set to 8.
#'
#' @param ... Ignored.
#'
#' @param all_partitions Optional argument containing the needed multipartitions
#' as list of lists of lists. If set to \code{NULL}, nn2poly will compute said
#' multipartitions. This step can be computationally expensive when the chosen
#' polynomial order or the dimension are too high. In such cases, it is
#' encouraged that the multipartitions are stored and reused when possible.
#'
#' @return An object of class `nn2poly`.
#' If \code{keep_layers = FALSE} (default case), it returns a list with two
#' items:
#' - An item named `labels` that is a list of integer vectors. Those vectors
#' represent each monomial in the polynomial, where each integer in the vector
#' represents each time one of the original variables appears in that term.
#' As an example, vector c(1,1,2) represents the term \eqn{x_1^2x_2}.
#' - An item named `values` which contains a matrix in which each row contains
#' the coefficients of the polynomial associated with an output neuron. That is,
#' if the neural network has a single output unit, the matrix `values` will have
#' a single row and if it has multiple output units, the matrix `values` will
#' have several rows.
#'
#' If \code{keep_layers = TRUE}, it returns a list of length L that for each
#' layer contains an item as explained before. The last element of the list will
#' be the same element as if \code{keep_layers = FALSE}.
#'
#' The polynomials obtained at the hidden layers are not needed to represent the
#' NN but can be used to explore other insights from the NN.
#'
#' @export
nn2poly <- function(object,
                    max_order = 2,
                    keep_layers = FALSE,
                    taylor_orders = 8,
                    ...,
                    all_partitions = NULL
                    ) {
  UseMethod("nn2poly")
}

#' @export
nn2poly.list <- function(object, ...) {
  result <- nn2poly_algorithm(object, names(object), ...)
  class(result) <- "nn2poly"
  result
}

#' @export
nn2poly.default <- function(object, ...) {
  params <- get_parameters(object)
  object <- params$weights_list
  names(object) <- params$af_string_list

  nn2poly(object, ...)
}

#' S3 method for class 'nn2poly'
#'
#' @param object An object of class inheriting from 'nn2poly'.
#' @param newdata Matrix for which predictions are to be made.
#' @param ... Additional arguments.
#'
#' @return \code{matrix} containing the predictions. There is one prediction for
#' each row in `newdata`.
#'
#' @export
predict.nn2poly <- function(object, newdata, ...) {
  if (length(class(object)) > 1)
    return(NextMethod())
  # this happens after running nn2poly()
  eval_poly(poly = object, x = newdata)
}
