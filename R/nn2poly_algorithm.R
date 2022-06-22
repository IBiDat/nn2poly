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
#' @param store_coeffs Boolean that determines if all polynomials computed in
#' the internal layers have to be stored and given in the output (TRUE), or if
#' only the last layer is needed (FALSE).
#'
#' @param forced_max_Q Optional argument: integer that determines the maximum order
#' that we will force in the final polynomial, discarding terms of higher order
#' that would naturally arise using all the orders in `q_taylor_vector`.
#'
#' @return If \code{store_coeffs = FALSE} (default case), it returns a list
#' with a vector containing the coefficients of a polynomial associated with
#' each output unit, i.e., one vector in a regression NN and C vectors in
#' a classification NN with C possible classes. If
#' \code{store_coeffs = TRUE}, it returns a list of length L that for each
#' layer contains a list with a vector of the coefficients of a polynomial
#' associated with each unit at that layer. The polynomials obtained at the
#' hidden layers are not needed to represent the NN but can be used to explore
#' how the method works.
#'
#' @export
#'
nn2poly_algorithm <- function(weights_list,
                              af_string_list,
                              q_taylor_vector,
                              all_partitions,
                              store_coeffs = FALSE,
                              forced_max_Q) {

  # Obtain number of variables (dimension p)
  p <- dim(weights_list[[1]])[1] - 1

  # Obtain number of layers L (hidden + output, input is dentoed by 0)
  L <- length(af_string_list)

  # Initialize current layer in algorithm:
  current_layer <- 1

  # Check if the problem is a regression or classification problem
  # If it is regression, the last activation function must be "linear"
  regression <- (af_string_list[[L]] == "linear")

  # The list with the results of coefficients at each layer,
  # depending on being a regression problem or not
  if (regression == TRUE) {
    results <- vector(mode = "list", length = 2 * L - 1)
  } else {
    results <- vector(mode = "list", length = 2 * L)
  }

  # Obtain all the derivatives up to the desired Taylor degree at each layer
  af_derivatives_list <- obtain_derivatives_list(
    af_string_list = af_string_list,
    q_taylor_vector = q_taylor_vector
  )

  # Obtain the maximum degree of the final polynomial:
  if(missing(forced_max_Q)){
    q_max <- prod(q_taylor_vector)
  } else {
    q_max <- min(prod(q_taylor_vector),forced_max_Q)
  }


  # Check if partitions have not been given as an input
  if (missing(all_partitions)) {
    # all_partitions will be a list with 2 elements:
    # the "total partitions" obtained with Knuth's algo
    # and then the actual coefficient's "labels" for which the partitions are
    # obtained
    partitions <- generate_partitions(as.integer(p), as.integer(q_max))

    labels <- vector(mode = "list", length = length(partitions))
    # Obtain labels:
    for (i in 1:length(partitions)){
      # Here it is used that the first partition of the multiset is always
      # the multiset itself. This could be generalized in case we change the
      # generation order. #REVISETHISLATER
      labels[[i]] <- partitions[[i]][[1]][[1]]
    }

    all_partitions <- list("labels" = labels, "partitions" = partitions)

    print("partitions obtained")
  }

  ################# current_layer = 1, linear #################

  # Starting point for the algorithm: Set weights as coefficients
  # of an order 1 polynomial.
  # Extract the weights:
  W1 <- weights_list[[1]]
  h1 <- dim(W1)[2]

  # The labels for each coefficient vector at the same layer and linear or
  # same layer and non linear case will have be the same, so they can be
  # stored only once as the first element of the coeffs_list_output,
  # so we define length h+1
  coeffs_list_output <- vector(mode = "list", length = h1+1)

  # generate and store the labels (as a list of integer vectors)
  # In this case the integer vectors are of length 1.
  labels_output<- vector(mode = "list", length = p+1)
  for (i in 0:p){
    labels_output[[i+1]] <- c(i)
  }
  coeffs_list_output[[1]] <- labels_output

  for (i in 1:h1){
    # For each neuron in the first hidden layer, when computing th synaptic
    # potentials (u_j), each column of the weight matrix represents the
    # coefficients of an order 1 polynomial for that neuron potential.
    # The first element will be the bias, and the rest the coefficient
    # associated with each variable from x_1 to x_p.
    coeffs_list_output[[i+1]] <- W1[,i]
  }

  results[[1]] <- coeffs_list_output

  # Stop if last layer and regression
  if (current_layer == L) {
    if (regression == TRUE) {
      return(results)
    }
  }

  ################# Loop over all layers #################

  # Note that the loop will iterate the current layer from 1 to L
  # and compute the linear and then non linear situation.
  # The linear case at layer 1 has been computed outside so we skip it

  for (current_layer in 1:L) {

    ########## Linear case ##########
    if (current_layer != 1){

      # Treat the previous coefficients output as input
      coeffs_list_input <- coeffs_list_output

      # Note that the polynomial in this case does not increase its order
      # from the one in the non linear previous layer, so the labels
      # will be the same:
      labels_output <- coeffs_list_input[[1]]

      # Parallel apply
      only_coeffs_output <- future.apply::future_apply(
        weights_list[[current_layer]], 2, alg_linear,
        coeffs_list_input = coeffs_list_input[-1],
        simplify = FALSE
      )

      # Join the coefficients with the labels as first list element:
      coeffs_list_output <- c(list(labels_output), only_coeffs_output)

      # Save results from this layer:
      results[[2 * (current_layer) - 1]] <- coeffs_list_output

      # Stop if last layer and regression
      if (current_layer == L) {
        if (regression == TRUE) {
          return(results)
        }
      }

    }


    ########## Non linear case ##########
    # The output dimension remains the same as in the previous linear case,
    # because the same number of neurons is considered

    # Treat the previous coeff output as input
    coeffs_list_input <- coeffs_list_output

    # In the non linear case the polynomial order increases (unless forced_max_Q is
    # reached), so the new labels need to be computed. However, the previous
    # ones can be reused.
    # The new labels will be for monomials of orders between the total order
    # of the previous polynomial and the total order of the new polynomial:
    if (current_layer==1){
      previous_total_order <- 1
    } else {
      # Get the total order used in the previous iteration
      previous_total_order <- new_total_order
    }

    # Compute the new total order with the product of q_taylor_vector.
    # If a forced_max_Q value is used, its taken as the minimum between both.
    if (missing(forced_max_Q)){
      new_total_order <- previous_total_order*q_taylor_vector[current_layer]
    } else {
      new_total_order <- min(previous_total_order*q_taylor_vector[current_layer],forced_max_Q)
    }

    # If the order has increased, created new needed labels.
    # If not, forced_max_Q has been reached and no new labels are needed.
    if (previous_total_order != new_total_order){
      # Loop over each of the new orders up to the maximum one
      for (order in (previous_total_order+1):new_total_order){
        combinations_indexes <- combinations_with_repetition(p, order)

        # Number of different combinations
        n_combinations <- nrow(combinations_indexes)

        # Intialize list with new labels for the given order
        new_labels <- vector(mode = "list", length = n_combinations)

        for (i in 1:n_combinations){
          new_labels[[i]] <- combinations_indexes[i, ]
        }

        # update labels with the new ones
        labels_output <- c(labels_output, new_labels)

      }
    }




    # Parallel lapply
    # The output index is already computed in the linear case
    # but not for l=1 #REVISETHISLATER
    # Obtain the number of nodes after the current layer
    output_dimension <- dim(weights_list[[current_layer]])[2]

    # Parallel lapply
    output_indexes <- 1:output_dimension
    only_coeffs_output <- future.apply::future_lapply(
      coeffs_list_input[-1], alg_non_linear,
      labels_input = coeffs_list_input[[1]],
      labels_output = labels_output,
      q_taylor_vector = q_taylor_vector,
      current_layer = current_layer,
      g = af_derivatives_list[[current_layer]],
      labels = all_partitions$labels,
      partitions = all_partitions$partitions
    )
    # print("salimos del future")

    # Join the coefficients with the labels as first list element:
    coeffs_list_output <- c(list(labels_output), only_coeffs_output)

    # Save results from this layer:
    results[[2 * (current_layer)]] <- coeffs_list_output

    # Check if this is the last layer and if it is a classification problem
    if (current_layer == L) {
      if (regression == FALSE) {
        return(results)
      }
    }
  }
}












