#' NN2Poly algorithm full algorithm.
#'
#' Internal function that performs the full NN2Poly algorithm given a list of
#' weights and a list of activation functions (af),
#' used inside nn2poly S3 method.
#'
#' @inheritParams nn2poly
#'
#' @param weights_list \code{list} of length L (number of hidden layers + 1)
#' containing the weights matrix for each layer. See nn2poly object argument
#' documentation.
#'
#' @param af_string_list \code{list} of length L containing \code{character}
#' strings with the names of the activation function used at each layer as the
#' names of the list expected by nn2poly.
#'
#' @return A list as expected in \code{nn2poly} output but without the nn2poly
#' class.
#'
#' @noRd
nn2poly_algorithm <- function(weights_list,
                              af_string_list,
                              max_order = 2,
                              keep_layers = FALSE,
                              taylor_orders = 8,
                              ...,
                              all_partitions = NULL
                              ) {

  if (!check_weights_dimensions(weights_list)) {
    stop("The list of weights has incorrect dimensions.
         Please, check the  right dimmensions in the documentation.",
         call. = FALSE
    )
  }

  # Obtain number of variables (dimension p)
  p <- dim(weights_list[[1]])[1] - 1

  # Obtain number of layers L (L-1 hidden + 1 output, input is denoted by 0)
  L <- length(af_string_list)

  # Initialize current layer in algorithm:
  current_layer <- 1

  # Check if the last layer is linear
  last_linear <- (af_string_list[[L]] == "linear")

  # The list with the results of coefficients at each layer,
  # depending on the last layer being linear or not
  if (last_linear) {
    results <- vector(mode = "list", length = 2 * L - 1)
  } else {
    results <- vector(mode = "list", length = 2 * L)
  }

  # Create a default taylor_orders if it is not given by the user
  taylor_orders <- obtain_taylor_vector(
    taylor_orders = taylor_orders,
    af_string_list = af_string_list
    )

  # Obtain all the derivatives up to the desired Taylor degree at each layer
  af_derivatives_list <- obtain_derivatives_list(
    af_string_list = af_string_list,
    taylor_orders = taylor_orders
    )

  # Obtain the maximum degree of the final polynomial:
  q_max <- obtain_final_poly_order(max_order, taylor_orders)

  # Check if partitions have not been given as an input
  if (is.null(all_partitions)) {
    all_partitions <- obtain_partitions_with_labels(p, q_max)
  }

  ################# current_layer = 1, linear #################

  # Starting point for the algorithm: Set weights as coefficients
  # of an order 1 polynomial.

  # The labels for each coefficient vector at the same layer and linear or
  # same layer and non linear case will have be the same, so they can be
  # stored only once as the element `labels` of the coeffs_list_output,
  # so we define length h+1
  coeffs_list_output <- vector(mode = "list", length = 0)

  # generate and store the labels (as a list of integer vectors)
  # In this case the integer vectors are of length 1.
  labels_output <- vector(mode = "list", length = p+1)
  for (i in 0:p) {
    labels_output[[i+1]] <- i
  }
  coeffs_list_output$labels <- labels_output

  # For each neuron in the first hidden layer, when computing the activation
  # potentials (u_j), each column of the weight matrix represents the
  # coefficients of an order 1 polynomial for that neuron potential.
  # The first element will be the bias, and the rest the coefficient
  # associated with each variable from x_1 to x_p.
  coeffs_list_output$values <- t(weights_list[[1]])

  # Store the results
  results[[1]] <- coeffs_list_output

  # Stop if last layer and the last layer is linear
  if (current_layer == L && last_linear) {
    return(coeffs_list_output)
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
      # will be the same and are already stored in $labels output.
      # Only the matrix of $values will change its number of rows


      ####### New  version alg linear START  ----------------------------------
      # apply the linear algorithm
      values <- coeffs_list_input$values
      coeffs_list_output$values <- t(weights_list[[current_layer]]) %*%
        rbind(c(1,rep(0,ncol(values)-1)),values)

      ####### New  version alg linear END  ----------------------------------

      # Save results from this layer:
      results[[2 * (current_layer) - 1]] <- coeffs_list_output

      # Stop if last layer and the last layer is linear
      if (current_layer == L && last_linear) {
        if (keep_layers) {
          return(results)
        } else {
          return(coeffs_list_output)
        }
      }

    }


    ########## Non linear case ##########
    # The output dimension remains the same as in the previous linear case,
    # because the same number of neurons is considered

    # Treat the previous coeff output as input
    coeffs_list_input <- coeffs_list_output

    # In the non linear case the polynomial order increases (unless max_order is
    # reached), so the new labels need to be computed. However, the previous
    # ones can be reused.
    # The new labels will be for monomials of orders between the total order
    # of the previous polynomial and the total order of the new polynomial:
    if (current_layer == 1) {
      previous_total_order <- 1
    } else {
      # Get the total order used in the previous iteration
      previous_total_order <- new_total_order
    }

    # Compute the new total order with the product of taylor_orders.
    # If a max_order value is used, its taken as the minimum between both.
    if (is.null(max_order)) {
      new_total_order <- previous_total_order * taylor_orders[current_layer]
    } else {
      new_total_order <- min(previous_total_order * taylor_orders[current_layer],
                             max_order)
    }

    # If the order has increased, create new needed labels.
    # If not, max_order has been reached and no new labels are needed.
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
        coeffs_list_output$labels <- c(coeffs_list_output$labels, new_labels)

      }
    }

    # Parallel lapply
    # The output index is already computed in the linear case
    # but not for l=1 #REVISETHISLATER
    values <- alg_non_linear(
      coeffs_list_input$values,
      labels_input = coeffs_list_input$labels,
      labels_output = coeffs_list_output$labels,
      taylor_orders = taylor_orders,
      current_layer = current_layer,
      g = af_derivatives_list[[current_layer]],
      partitions_labels = all_partitions$labels,
      partitions = all_partitions$partitions
    )

    # Join the coefficients with the labels as first list element:
    coeffs_list_output$values <- values

    # Save results from this layer:
    results[[2 * (current_layer)]] <- coeffs_list_output

    # Check if this is the last layer and if the last layer is not linear
    if (current_layer == L && !last_linear) {
      if (keep_layers) {
        return(results)
      } else {
        return(coeffs_list_output)
      }
    }

  }
}

#' This functions will generate the partitions obtained with Knuth's algorithm,
#' compute their labels and store both things in a list of length 2.
#'
#'
#' @param p Number of variables.
#'
#' @param q_max Maximum degree of the final polynomial.
#'
#' @return List of length 2 where the first element is a list with the labels
#' and the second element is a list with the partitions.
#'
#' @noRd
obtain_partitions_with_labels <- function(p, q_max) {
  # This function will return a list with 2 elements:
  #
  # * The partitions obtained with Knuth's algorithm
  # * The actual coefficient's "labels" for which the partitions are obtained
  #

  if (missing(p) && missing(q_max)) {
    stop("Missing both arguments p and q_max.", call. = FALSE)
  } else if (missing(p)) {
      stop("Missing argument p.", call. = FALSE)
  } else if (missing(q_max)) {
      stop("Missing argument q_max.", call. = FALSE)
  }


  partitions <- generate_partitions(as.integer(p), as.integer(q_max))

  labels <- vector(mode = "list", length = length(partitions))
  # Obtain labels:
  for (i in 1:length(partitions)){
    # Here it is used that the first partition of the multiset is always
    # the multiset itself. This could be generalized in case we change the
    # generation order. #REVISETHISLATER
    labels[[i]] <- partitions[[i]][[1]][[1]]
  }

  return(list("labels" = labels, "partitions" = partitions))
}

#' Computes the maximum polynomial order allowed by max_order and
#' Taylor orders at each layer
#'
#' Internal function used in nn2poly_algorithm.
#' Computes the final polynomial order by checking if max_order is an integer
#' and that the product of the Taylor order values allows for it. If it is
#' not reached, a warning is provided.
#'
#' @inheritParams nn2poly
#' @return An integer with the final polynomial order
#'
#' @noRd
obtain_final_poly_order <- function(max_order, taylor_orders){
  if(is.numeric(max_order) & (max_order %% 1)==0){
    # This condition allows for integers and also
    # integers written as numeric
    max_order <- as.integer(max_order)
    poly_order <- min(prod(taylor_orders),max_order)

    # Warning if max_order has not been reached. (Very rare situation)
    if (poly_order < max_order){
      warning("Argument `max_order` has not been reached due to chosen taylor_orders")
    }
  } else {
    stop("Argument `max_order` is not an integer value", call. = FALSE)
  }

  return(poly_order)
}

#' Obtain a vector containing the Taylor order to be applied at each layer
#'
#' Internal function used in nn2poly_algorithm.
#' It allows the user to specify a single numeric value, in which case that
#' value will be employed at all no linear layers and 1 at linear layers. The
#' user can also provide their own vector, where in that case the dimensions
#' are checked so they match the number of layers provided by af_string_list.
#'
#' @inheritParams nn2poly
#'
#' @return An integer vector
#'
#' @noRd
obtain_taylor_vector <- function(taylor_orders, af_string_list){
  # Get the number of layers
  L <- length(af_string_list)

  # First check if taylor_orders are integers or numeric with no decimal part:
  if(!(is.numeric(taylor_orders) & all((taylor_orders %% 1)==0))){
    stop("Argument `taylor_orders` is non numeric", call. = FALSE)
  } else if(length(taylor_orders)==1) {
    # Single value case, set 1 in linear, taylor_orders in other AF
    taylor_orders <- ifelse(af_string_list=="linear", 1, taylor_orders)
  } else {
    # Vector provided by user, check if dimensions match
    if(!(length(taylor_orders)==length(af_string_list))){
      stop("Argument `taylor_orders` length does not match provided number of layers",
           call. = FALSE)
    }
  }
  return(taylor_orders)
}

#' Obtain needed derivatives up to the chosen order (q Taylor)
#'
#' This function is internally used in nn2poly_algorithm to obtain the
#' derivatives of the given activation function at 0 up to the desired order $q$
#' for each layer.
#'
#' @inheritParams nn2poly
#' @inheritParams nn2poly_algorithm
#'
#' @return list of vectors with the derivatives
#'
#' @noRd
obtain_derivatives_list <- function(af_string_list, taylor_orders) {

  n <- length(af_string_list)
  af_function_list <- string_to_function(af_string_list)
  af_derivatives_list <- vector(mode = "list", length = n)

  for (i in 1:n) {
    # Obtain the vector with the derivatives of the activation function up to the given degree:
    # centered at 0
    # and use rev to reverse and match our notation.
    af_derivatives_list[[i]] <- rev(pracma::taylor(af_function_list[[i]], 0, taylor_orders[i]))

    # here we have a problem: if the last term of the taylor expansion is 0,
    # the previous method deletes that entry and then the dimensions willm not macth later
    # therefore, we add 0's if needed:
    diff_len <- (taylor_orders[i] + 1) - length(af_derivatives_list[[i]])
    if (diff_len > 0) {
      af_derivatives_list[[i]] <- c(af_derivatives_list[[i]], rep(0, diff_len))
    }
  }

  return(af_derivatives_list)
}

#' Checks that the weights dimensions are correct.
#'
#' This means that each matrix has the same number of rows as the number
#' of columns in the previous matrix + 1. This is because the number of
#' output neurons in the previous layer is the same as the number of inputs
#' to the current layer + the bias.
#'
#' @inheritParams nn2poly_algorithm
#'
#' @return `TRUE` if the dimensiones are correct, `FALSE` if not.
#'
#' @noRd
check_weights_dimensions <- function(weights_list) {
  for (matrix_index in 2:length(weights_list)) {
    nrows_current <- nrow(weights_list[[matrix_index]])
    ncols_prev    <- ncol(weights_list[[matrix_index - 1]])

    if (nrows_current != ncols_prev + 1)
      return(FALSE)
  }
  return(TRUE)
}
