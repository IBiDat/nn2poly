#' @useDynLib nn2poly, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom stats density
utils::globalVariables(".data")
NULL

#' nn2poly generic
#'
#' @param object An object for which the computation of the nn2poly algorithm is desired.
#' For the default case, it should be a \code{list} of length L ( number of hidden layers + 1)
#' containing the weights matrix for each layer. The name of every element in the list
#' should be the name of the activation function to be used at each layer.
#'
#' The expected shape of such matrices at any layer L is of the form
#' $(h_(l-1) + 1)*(h_l)$, that is, the number of rows is the number of neurons
#' in the previous layer plus the bias vector, and the number of columns is the
#' number of neurons in the current layer L. Therefore, each column
#' corresponds to the weight vector affecting each neuron in that layer.
#' It could also be a `keras.engine.training.Model` model.
#' @param q_taylor_vector \code{vector} of length L containing the degree
#' (\code{numeric}) up to which Taylor expansion should be performed at each
#' layer.
#' @param all_partitions Optional argument containing the needed multipartitions
#' as list of lists of lists. If \code{NULL}, the function computes it first. This
#' step can be computationally expensive and it is encouraged that the
#' multipartitions are stored and reused when possible.
#' @param store_coeffs Boolean that determines if all polynomials computed in
#' the internal layers have to be stored and given in the output (TRUE), or if
#' only the last layer is needed (FALSE).
#' @param forced_max_Q Optional argument: integer that determines the maximum order
#' that we will force in the final polynomial, discarding terms of higher order
#' that would naturally arise using all the orders in `q_taylor_vector`.
#' @param ... Ignored.
#'
#' @return An object of class `nn2poly`.
#' If \code{store_coeffs = FALSE} (default case), it returns a list
#' with an item named `labels` that is a list of integer vectors with each the
#' variables index associated to each polynomial term, and a item named `values`
#' which contains a matrix where each row are the coefficients of the polynomial
#' associated with an output neuron.
#'
#' If \code{store_coeffs = TRUE}, it returns a list of length L that for each
#' layer contains an item as explained before. The polynomials obtained at the
#' hidden layers are not needed to represent the NN but can be used to explore
#' how the method works.
#' @export
#'
#' @examples
nn2poly <- function(object,
                    q_taylor_vector,
                    all_partitions = NULL,
                    store_coeffs   = FALSE,
                    forced_max_Q   = NULL,
                    ...) {
  UseMethod("nn2poly")
}

#' @export
nn2poly.default <- function(object, # weights_list and af_string_list
                            q_taylor_vector,
                            all_partitions = NULL,
                            store_coeffs   = FALSE,
                            forced_max_Q   = NULL,
                            ...) {

  result <- nn2poly_algorithm(
    weights_list    = object,
    af_string_list  = names(object),
    q_taylor_vector = q_taylor_vector,
    all_partitions  = all_partitions,
    store_coeffs    = store_coeffs,
    forced_max_Q    = forced_max_Q
  )
  class(result) <- "nn2poly"
  result
}

#' @export
nn2poly.keras.engine.training.Model <- function(object,
                                                q_taylor_vector,
                                                all_partitions = NULL,
                                                store_coeffs   = FALSE,
                                                forced_max_Q   = NULL,
                                                ...) {

  model_parameters <- get_model_parameters(object)

  result <- nn2poly_algorithm(
    weights_list    = model_parameters$weights_list,
    af_string_list  = model_parameters$af_string_list,
    q_taylor_vector = q_taylor_vector,
    all_partitions  = all_partitions,
    store_coeffs    = store_coeffs,
    forced_max_Q    = forced_max_Q
  )

  class(result) <- "nn2poly"
  result
}



#' S3 method for class 'nn2poly'
#'
#' @param object An object of class inheriting from 'nn2poly'.
#' @param newdata Matrix for which predictions are to be made.
#' @param ... Additional arguments.
#'
#' @return \code{matrix} containing the predictions. There is one prediction for
#' each row in `newdata`.
#' @export
#'
#' @examples
predict.nn2poly <- function(object, newdata, ...) {
  eval_poly(x = newdata, poly = object)
}


