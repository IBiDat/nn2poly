#' @useDynLib nn2poly, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom stats density
utils::globalVariables(c(".data", "super", "self", "private", "ctx"))
NULL

#' Obtain polynomial representation
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
#' Each element of the list should be named as the activation function used at
#' each layer. Currently supported activation functions are \code{"tanh"},
#' \code{"softplus"}, \code{"sigmoid"} and \code{"linear"}.
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
#' Default set to \code{FALSE}.
#'
#' @param taylor_orders \code{integer} or \code{vector} of length L that sets the
#' degree at which Taylor expansion is truncated at each layer. If a single
#' value is used, that value is set for each non linear layer and 1 for linear
#  layers. If a vector is used, each value corresponds to the Taylor order used
#' at each layer activation function. Default set to \code{8}.
#'
#' @param ... Ignored.
#'
#' @param all_partitions Optional argument containing the needed multipartitions
#' as list of lists of lists. If set to \code{NULL}, nn2poly will compute said
#' multipartitions. This step can be computationally expensive when the chosen
#' polynomial order or the dimension are too high. In such cases, it is
#' encouraged that the multipartitions are stored and reused when possible.
#' Default set to \code{NULL}.
#'
#' @return Returns an object of class `nn2poly`.
#'
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
#' If \code{keep_layers = TRUE}, it returns a list of length the number of
#' layers (represented by \code{layer_i}), where each one is another list with
#' \code{input} and \code{output} elements. Each of those elements contains an
#' item as explained before. The last layer output item will be the same element
#' as if \code{keep_layers = FALSE}.
#'
#' The polynomials obtained at the hidden layers are not needed to represent the
#' NN but can be used to explore other insights from the NN.
#'
#' @seealso Predict method for \code{nn2poly} output [predict.nn2poly()].
#'
#' @examples
#' # Build a NN estructure with random weights, with 2 (+ bias) inputs,
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
#' # Change the last layer to have 3 outputs (as in a multiclass classification)
#' # problem
#' weights_layer_4 <- matrix(rnorm(20), nrow = 5, ncol = 4)
#'
#' # Set it as a list with activation functions as names
#' nn_object = list("tanh" = weights_layer_1,
#'                  "softplus" = weights_layer_2,
#'                  "linear" = weights_layer_4)
#' # Obtain the polynomial representation of that neural network
#' # In this case the output is formed by several polynomials with the same
#' # structure but different coefficient values
#' final_poly <- nn2poly(nn_object, max_order = 3)
#'
#' # Polynomial representation of each hidden neuron is given by
#' final_poly <- nn2poly(nn_object, max_order = 3, keep_layers = TRUE)
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
  result_raw <- nn2poly_algorithm(object, names(object), ...)

  # Check if object is a single polynomial or a list of polynomials.
  # This is controlled by keep_layers.
  bool_layers = is.null(result_raw$labels)

  if(bool_layers){
    n_items <- length(result_raw)
    n_layers <- ceiling(n_items/2)
    result <- list()
    for (i in 1:n_layers){
      layer_name <- paste0("layer_", i)
      result[[layer_name]] <- list()
      result[[layer_name]][["input"]] <- result_raw[[(2*i-1)]]
      if (2*i <= n_items){
        result[[layer_name]][["output"]] <- result_raw[[(2*i)]]
      } else {
        # If there is a linear output, i.e. single polynomial in final layer
        # and odd number of items, then we repeat the input as the output, as
        # the activation functions takes no effect on the polynomial
        result[[layer_name]][["output"]] <- result[[layer_name]][["input"]]
      }
    }

  } else {
    result = result_raw
  }

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

#' Predict method for \code{nn2poly} objects.
#'
#' Predicted values obtained with a nn2poly object on given data.
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
#' \code{nn2poly}.
#'
#' If \code{object} contains the polynomials of the last layer, as given by
#' \code{nn2poly(object, keep_layers = FALSE)}, then the output is a matrix with
#' the evaluation of each data point on each polynomial.
#'
#' If \code{object} contains all the internal polynomials also, as given by
#' \code{nn2poly(object, keep_layers = TRUE)}, then the output is a list of
#' layers (represented by \code{layer_i}), where each one is another list with
#' \code{input} and \code{output} elements, where each one contains a matrix with the
#' evaluation of the "input" or "output" polynomial at the given layer.
#'
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
predict.nn2poly <- function(object, newdata, layers = NULL, ...) {
  if (length(class(object)) > 1)
    return(NextMethod())

  # Check if object is a single polynomial or a list of polynomials.
  # If we get only the output layer, then it has to be a list with 2 elements,
  # values and labels. We check one of them:
  if (!is.null(object$labels)){
    # If we have a final polynomial, directly evaluate the results:
    result <- eval_poly(poly = object, newdata = newdata)
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
      result[[layer_name]][["input"]] <- eval_poly(
        poly = object[[layer_name]][["input"]],
        newdata = newdata)
      result[[layer_name]][["output"]] <- eval_poly(
        poly = object[[layer_name]][["output"]],
        newdata = newdata)
    }
  }

return(result)

}
