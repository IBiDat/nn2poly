#' Computes one or several polynomials to represent a given neural network
#' using the NN2Poly algorithm.
#'
#' Performs the full NN2Poly algorithm that obtains polynomial coefficients
#' for a model that performs closely as a given already trained neural network
#' using its weights and a Taylor approximation of its activation functions.
#'
#' @param weights_list \code{list} of length L ( number of hidden layers + 1)
#' containing the weights matrix for each layer.
#' The expected shape of such matrices at any layer L is of the form
#' $(h_(l-1) + 1)*(h_l)$, that is, the number of rows is the number of neurons
#' in the previous layer plus the bias vector, and the number of columns is the
#' number of neurons in the current layer L. Therefore, each column
#' corresponds to the weight vector affecting each neuron in that layer.
#'
#' @param af_string_list \code{list} of length L containing \code{character}
#' strings with the names of the activation function used at each layer.
#' The last element should be "linear" if the output layer has a single linear
#' output neuron, i.e., when solving a regression problem.
#'
#' @param q_taylor_vector \code{vector} of length L containing the degree
#' (\code{numeric}) up to which Taylor expansion should be performed at each
#' layer.
#'
#' @param all_partitions Optional argument containing the needed multipartitions
#' as list of lists of lists. If missing, the function computes it first. This
#' step can be computationally expensive and it is encouraged that the
#' multipartitions are stored and reused when possible.
#'
#' @return If \code{historical_coeffs = FALSE} (default case), it returns a list
#' with a vector containing the coefficients of a polynomial associated with
#' each output unit, i.e., one vector in a regression NN and C vectors in
#' a classification NN with C possible classes. If
#' \code{historical_coeffs = TRUE}, it returns a list of length L that for each
#' layer contains a list with a vector of the coefficients of a polynomial
#' associated with each unit at that layer. The polynomials obtained at the
#' hidden layers are not needed to represent the NN but can be used to explore
#' how the method works.
#'
#' @export
#'
OLD_VERSION_nn2poly_algorithm <- function(weights_list,
                              af_string_list,
                              q_taylor_vector,
                              all_partitions) {

  # Obtain number of variables (dimension p)
  p <- dim(weights_list[[1]])[1] - 1

  # Obtain number of layers L
  L <- length(af_string_list)

  # Check if the problem is a regression or classification problem
  # If it is regression, the last activation function must be "linear"
  regression <- (af_string_list[[L]] == "linear")

  # The list of lists of coefficients at each layer, will be of size
  # 2(L-1) - 1 if it is a regression problem
  # or 2(L-1) if not:
  if (regression == TRUE) {
    historical_list <- vector(mode = "list", length = 2 * (L - 1) - 1)
  } else {
    historical_list <- vector(mode = "list", length = 2 * (L - 1))
  }


  # Obtain the activation function list as R functions:
  af_function_list <- change_string_to_function(af_string_list = af_string_list)

  # Obtain now all the derivatives up to the desired Taylor degree at each layer
  af_derivatives_list <- obtain_derivatives_list(
    af_function_list = af_function_list,
    q_taylor_vector = q_taylor_vector
  )

  # Obtain the maximum degree of the polynomial that we will obtain with the method:
  q_max <- prod(q_taylor_vector)

  # Check if partitions have not been given as an input
  if (missing(all_partitions)) {
    # Compute multiset partitions if missing, using Python

    # Generate all partitions with Python script:
    all_partitions <- OLD_VERSION_generate_partitions(as.integer(p), as.integer(q_max))
    # Obtain a label for each of the coefficients partitioned in the list:
    all_partitions <- label_partitions_from_python(all_partitions = all_partitions)
    # These are now stored to be used when needed in all the loops.
    print("partitions obtained")
  }


  ############################ WHEN L = 2 ##############################

  ########## First layer, Regression case ##########

  # Obtain the number of nodes after the first hidden layer
  output_dimension <- dim(weights_list[[2]])[2]

  output_indexes <- 1:output_dimension
  coeffs_list_output <- future.apply::future_lapply(output_indexes,
    obtain_PR_coefficients_regression_first_layer,
    weights_list = weights_list,
    g = af_derivatives_list[[1]]
  )

  # Save historical coefficients:
  historical_list[[1]] <- coeffs_list_output

  # We now check if we only need these computations to stop here and return the values:
  if (L == 2) {
    if (regression == TRUE) {
      return(historical_list)
    }
  }

  ##### First layer: Clasification case ######
  # The output dimension remains the same as we have the same nodes
  # as in the regression but need now to include the effect of their activation function

  # Treat now the previous coeffs_list output as input
  coeffs_list_input <- coeffs_list_output

  # Set up layer index:
  layer_index <- 2

  coeffs_list_output <- future.apply::future_lapply(output_indexes,
    OLD_VERSION_obtain_PR_coefficients_classification,
    coeffs_list_input = coeffs_list_input,
    layer_index = layer_index,
    af_derivatives_list = af_derivatives_list,
    q_taylor_vector = q_taylor_vector,
    p = p,
    all_partitions = all_partitions
  )

  # Save historical coefficients:
  historical_list[[2]] <- coeffs_list_output

  if (L == 2) {
    if (regression == FALSE) {
      return(historical_list)
    }
  }

  ############################ LOOP WHEN L > 2 ##############################

  # we have already checked that L>2, because if not, return has already been called.
  # Inside each iteration of the loop we will compute first regression then classification
  # and check if it is the last computation to return the value.

  for (layer_index in 3:L) {

    ########## Regression case ##########

    # Treat now the previous coeff output as input
    coeffs_list_input <- coeffs_list_output

    # Obtain the number of nodes after the hidden layer "layer_index"
    output_dimension <- dim(weights_list[[layer_index]])[2]

    # Parallel lapply
    output_indexes <- 1:output_dimension
    coeffs_list_output <- future.apply::future_lapply(output_indexes,
      obtain_PR_coefficients_regression_deep_layer,
      coeffs_list_input = coeffs_list_input,
      weights_list = weights_list,
      layer_index = layer_index
    )


    # Save historical coefficients:
    historical_list[[2 * (layer_index - 1) - 1]] <- coeffs_list_output

    # Check if this is the last layer and if it is a regression problem
    if (layer_index == L) {
      if (regression == TRUE) {
        return(historical_list)
      }
    }

    ##### First layer: Clasification case ######
    # The output dimension remains the same as we have the same nodes
    # as in the regression but need now to include the effect of their activation function

    # Treat now the previous coeff output as input
    coeffs_list_input <- coeffs_list_output

    # Parallel lapply
    coeffs_list_output <- future.apply::future_lapply(output_indexes,
      OLD_VERSION_obtain_PR_coefficients_classification,
      coeffs_list_input = coeffs_list_input,
      layer_index = layer_index,
      af_derivatives_list = af_derivatives_list,
      q_taylor_vector = q_taylor_vector,
      p = p,
      all_partitions = all_partitions
    )


    # Save historical coefficients:
    historical_list[[2 * (layer_index - 1)]] <- coeffs_list_output

    # Check if this is the last layer and if it is a classification problem
    # Actually this could be ommited as at the end of the loop we would be in this
    # situation if a return has not been called yet.

    if (layer_index == L) {
      if (regression == FALSE) {
        return(historical_list)
      }
    }
  }
}
