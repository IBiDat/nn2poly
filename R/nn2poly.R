#' @useDynLib nn2poly, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom stats density
utils::globalVariables(c(".data", "super", "self", "private", "ctx"))
NULL

#' Obtain a polynomial representation from a trained neural network
#'
#' Implements the main nn2poly algorithm to obtain a polynomial representation
#' of a trained neural network from its weights and Taylor expansion of its
#' activation functions.
#'
#' @param object An object for which the computation of the nn2poly algorithm is desired.
#' For the default case, it should be a \code{list} of length L ( number of hidden layers + 1)
#' containing the weights matrix for each layer. The name of every element in the list
#' should be the name of the activation function to be used at each layer.
#'
#' The expected shape of such matrices at any layer L is of the form
#' \eqn{(h_{(l-1)} + 1)*(h_l)}, that is, the number of rows is the number of neurons
#' in the previous layer plus the bias vector, and the number of columns is the
#' number of neurons in the current layer L. Therefore, each column
#' corresponds to the weight vector affecting each neuron in that layer.
#' The bias vector should be in the first row.
#' It could also be a `keras.engine.training.Model` model.
#'
#' @param max_order Integer that determines the maximum order
#' that will be forced in the final polynomial, discarding terms of higher order
#' that would naturally arise using all the orders in `taylor_orders`.
#'
#' @param taylor_orders \code{integer} or \code{vector} of length L that sets the
#' degree at which Taylor expansion is truncated at each layer. If a single
#' value is used, that value is set for each non linear layer and 1 for linear
#  layers. If a vector is used, each value corresponds to the Taylor order used
#' at each layer activation function. Default is set to 8.
#'
#' @param keep_layers Boolean that determines if all polynomials computed in
#' the internal layers have to be stored and given in the output (\code{TRUE}),
#' or if only the last layer is needed (\code{FALSE}).
#'
#' @param ... Ignored.
#'
#' @param all_partitions Optional argument containing the needed multipartitions
#' as list of lists of lists. If \code{NULL}, the function computes it first. This
#' step can be computationally expensive and it is encouraged that the
#' multipartitions are stored and reused when possible.
#'
#' @return An object of class `nn2poly`.
#' If \code{keep_layers = FALSE} (default case), it returns a list
#' with an item named `labels` that is a list of integer vectors with each the
#' variables index associated to each polynomial term, and a item named `values`
#' which contains a matrix where each row are the coefficients of the polynomial
#' associated with an output neuron.
#'
#' If \code{keep_layers = TRUE}, it returns a list of length L that for each
#' layer contains an item as explained before. The polynomials obtained at the
#' hidden layers are not needed to represent the NN but can be used to explore
#' how the method works.
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
  eval_poly(x = newdata, poly = object)
}
