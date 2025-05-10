#' Polynomial evaluation
#'
#' Evaluates one or several polynomials on the given data.
#'
#' @details Note that this function is unstable and subject to change. Therefore it is
#' not exported but this documentations is left available so users can use it if
#' needed to simulate data by using \code{nn2poly:::eval_poly()}.
#'
#' @param poly List containing 2 items: \code{labels} and \code{values}.
#' - \code{labels}: List of integer vectors with same length (or number of cols)
#' as \code{values}, where each integer vector denotes the combination of
#' variables associated to the coefficient value stored at the same position in
#' \code{values}. That is, the monomials in the polynomial. Note that the
#' variables are numbered from 1 to p, with the intercept is represented by 0.
#' - \code{values}: Matrix (can also be a vector if single polynomial), where
#' each column represents a polynomial, with same number of rows as the length
#' of \code{labels}, containing at each row the value of the coefficient
#' of the monomial given by the equivalent label in that same position.
#'
#' Example: If \code{labels} contains the integer vector \code{c(1,1,3)} at position
#' 5, then the value stored in \code{values} at row 5 is the coefficient
#' associated with the term \eqn{x_1^2*x_3}.
#'
#' @param newdata Input data as matrix, vector or dataframe.
#' Number of columns (or elements in vector) should be the number of variables
#' in the polynomial (dimension p). Response variable to be predicted should
#' not be included.
#'
#' @param monomials Boolean determining if the returned item should contain the
#' evaluations of all the monomials of the provided polynomials
#' (\code{monomials==TRUE}), or if the final polynomial evaluation should be
#' computed, i.e., adding up all the monomials (\code{monomials==FALSE}).
#' Defaults to \code{FALSE}.
#'
#' @return If \code{monomials==FALSE}, returns a matrix containing the
#' evaluation of the polynomials on the given data. The matrix has dimensions
#' \code{(n_sample, n_polynomials)}, meaning that each column corresponds to the
#' result of evaluating all the data for a polynomial. If a single polynomial is
#' provided, the output is a vector instead of a row matrix.
#'
#' If \code{monomials==TRUE}, returns a 3D array containing the monomials of
#' each polynomial evaluated on the given data. The array has dimensions
#' \code{(n_sample, n_monomial_terms, n_polynomials)}, where element
#' \code{[i,j,k]} contains the evaluation on observation \code{i} on
#' monomial \code{j} of polynomial \code{k}, where monomial \code{j} corresponds
#' to the one on \code{poly$labels[[j]]}.
#'
#' @seealso \code{eval_poly()} is also used in [predict.nn2poly()].
#'
eval_poly <- function(poly, newdata, monomials = FALSE) {

  newdata <- preprocess_newdata(newdata)

  # Check if there are labels with bigger numbers than the number of columns
  if (length(poly$labels) > 0) { # Only check if there are labels
    # Find the maximum variable index referenced in the polynomial labels
    # Exclude 0 (intercept) from consideration as a variable index
    all_vars_in_labels <- unlist(lapply(poly$labels, function(lab) lab[lab > 0]))
    if (length(all_vars_in_labels) > 0) {
      max_var_index_poly <- max(all_vars_in_labels, na.rm = TRUE)

      # Check against number of columns in newdata
      if (max_var_index_poly > 0 && ncol(newdata) < max_var_index_poly) {
        stop(paste0("Polynomial requires at least ", max_var_index_poly,
                    " variable(s), but newdata only has ", ncol(newdata), " column(s)."),
             call. = FALSE)
      }
    }
  }

  aux <- preprocess_poly(poly)
  poly <- aux$poly
  original_intercept_pos_for_reordering <- aux$intercept_position

  # Check if the *current first term* (after potential reordering by preprocess_poly) is the intercept
  first_term_is_intercept <- FALSE
  if (length(poly$labels) > 0 && length(poly$labels[[1]]) == 1 && poly$labels[[1]][1] == 0) {
    first_term_is_intercept <- TRUE
  }

  n_sample <- nrow(newdata)
  n_polynomials <- ncol(poly$values)
  n_monomial_terms <- length(poly$labels)

  # We will first compute all the needed 3D monomial arrays
  # At the end, they will be summed to form the final polynomial prediction if
  # needed.

  response <- array(0,c(n_sample, n_monomial_terms, n_polynomials))

  for (k in 1:n_polynomials){

    # Select the desired polynomial values (column of poly$values)
    values_k <- poly$values[,k]

    if (first_term_is_intercept){
      # Initialize the vector with the intercept value repeated as needed.
      response[,1,k] <- rep(values_k[1], nrow(newdata))
      start_loop <- 2
    } else {
      # Intercept (label = 0) should always be the first element of labels at this
      # point of the function (labels reordered previously in preprocess_poly).
      # initialize the vector with 0s repeated as needed.
      start_loop <- 1
    }

    # Loop over all terms (labels) except the intercept
    if (start_loop <= length(poly$labels)) {
      for (j in start_loop:length(poly$labels)) {

        label_j <- poly$labels[[j]]
        coefficient_val <- values_k[j]

        var_prod <- multiply_variables(label_j, newdata)

        # Here instead of adding response over the loop as in the normal
        # eval_poly, store it in the appropriate position.
        response[,j,k] = coefficient_val * var_prod
      }
    }

    # In case the intercept has been moved, we reorder it to its original
    # position so it preserves the original notation of the user.
    response[,,k] <- reorder_intercept_in_monomials(response[,,k],
                                                    original_intercept_pos_for_reordering,
                                                    n_sample)
  }

  # With all monomials computed, we can now add them to obtain the final
  # polynomial prediction if needed, and simplify the output format when
  # having a single polynomial.

  if (monomials == FALSE){
    # The full polynomial prediction is needed.
    # It can be computed by adding the monomials for each polynomial, which is
    # adding by rows on each matrix response[,,k]
    aux_response <- NULL
    for (k in 1:n_polynomials){
      aux_response <- cbind(aux_response,
                            rowSums(matrix(
                              response[,,k],
                              nrow = n_sample,
                              ncol = n_monomial_terms))
                            )
    }

    # Set the final response to be the obtained matrix
    response <- aux_response

    # If there is a single polynomial, turn matrix into vector
    if (n_polynomials==1){
      response <- as.vector(response)
    }

  }

  return(response)
}

# Aux functions to help with internal eval_poly and eval_monomials -----

#' Preprocesses different newdata inputs to match eval_poly needs
#'
#' @param newdata matrix, dataframe, vector, or any other possible input, where
#' rows represent observations and columns the variables.
#'
#' @return An unnamed matrix, even if its single vector.
#'
#' @noRd
preprocess_newdata <- function(newdata){

  # Remove names and transform into matrix (variables as columns)
  newdata <- unname(as.matrix(newdata))

  # If newdata is a single vector, transpose to have it as row vector:
  if(ncol(newdata)==1){
    newdata = t(newdata)
  }

  return(newdata)
}


#' Preprocesses poly input to match eval_poly needs
#'
#' This should not be needed if the input polynomial to eval_poly is always
#' built using nn2poly(), but this preprocessing steps allow us to use it with
#' manually built polynomials under different conditions.
#'
#' @param poly A polynomial as given by eval_poly, with $labels and $values.
#'
#' @return An element containing:
#' - A new polynomial in the same form, but with values as a matrix
#' and labels with the intercept ordered to be the first element.
#' - The original intercept position, which will a positive integer if it has
#' been moved, and NULL if not.
#'
#' @noRd
preprocess_poly <- function(poly){
  if (!is.matrix(poly$values)){
    poly$values <- as.matrix(poly$values)
  }
  intercept_position_original <- NULL # To return the original position

  # Find if intercept c(0) exists
  idx_intercept_in_list <- which(sapply(poly$labels, function(x) length(x)==1 && x[1]==0))

  if (length(idx_intercept_in_list) > 0) { # Intercept c(0) exists
    intercept_position_original <- idx_intercept_in_list[1] # Take the first one if multiple

    if (intercept_position_original != 1) { # If it's not already first
      # Store the intercept label and value
      intercept_label_vec <- poly$labels[[intercept_position_original]] # Should be c(0)
      intercept_value_row <- poly$values[intercept_position_original, , drop = FALSE]

      # Remove it from original position
      poly$labels <- poly$labels[-intercept_position_original]
      poly$values <- poly$values[-intercept_position_original, , drop = FALSE]

      # Prepend it
      poly$labels <- c(list(intercept_label_vec), poly$labels) # Correctly prepend list element
      poly$values <- rbind(intercept_value_row, poly$values)
    }
  }
  # At this point, if intercept c(0) existed, it's now the first element.
  # If it didn't exist, poly is unchanged.
  # The variable to return indicating original pos (for reorder_intercept_in_monomials)
  # should reflect the original position, not just if it was moved.

  output <- list()
  # For reorder_intercept_in_monomials, we need to know if an intercept *was* present and its *original* slot
  # The current `intercept_position` in `eval_poly` seems to be used to determine if an intercept *is now first*.
  # Let's stick to your variable names. `intercept_position` will be used by `eval_poly` to check if the first term is an intercept.
  # And by `reorder_intercept_in_monomials` to know where it *originally* was.

  current_first_is_intercept <- length(poly$labels) > 0 && length(poly$labels[[1]]) == 1 && poly$labels[[1]][1] == 0

  output$intercept_position <- if(length(idx_intercept_in_list) > 0) idx_intercept_in_list[1] else NULL
  output$poly <- poly
  return(output)
}

#' Reorder intercept in monomials matrix
#'
#' Returns the matrix with the monomials evaluation to comply with the original
#' order in poly$labels when the intercept has been moved to the first element.
#'
#' @param monomials_matrix The monomials matrix computed for a single polynomial
#' on all the given data.
#' @param intercept_position The original position of the intercept. Set to NULL
#' if not present.
#' @param n_sample number of samples or observations in newdata.
#'
#' @return The same monomials matrix but with he intercept values at the
#' original position in poly$labels instead of the first one (if it was changed)
#'
#' @noRd
reorder_intercept_in_monomials <- function(monomials_matrix,
                                           intercept_position,
                                           n_sample){


  # Intercept has only been moved if its position is not NULL
  # Also, reordering only needed if it is different from 1.
  if(!is.null(intercept_position) && !(intercept_position==1)){

    # Force monomials_matrix to be a matrix, which will be not if
    # we have a single observation.
    M<- matrix(monomials_matrix, nrow = n_sample)

    M_prev <- matrix(M[,2:(intercept_position)], nrow = n_sample)
    M_intercept <- matrix(M[,1], nrow = n_sample)
    M_post <- matrix(M[,(intercept_position+1):ncol(M)], nrow = n_sample)


    monomials_matrix <- cbind(M_prev,
                              M_intercept,
                              M_post)
  }

  return(monomials_matrix)
}


#' Multiply variables given by label (a monomial representation)
#'
#' Multiplies the needed variables with their newdata values, but does not
#' include the monomial coefficient.
#'
#'
#' @param label label representing the variables interacting in the monomial
#' @param newdata data on which the monomial will be evaluated.
#'
#' @return A number with the products of the variables.
#'
#' @noRd

multiply_variables <- function(label, newdata){
  # Get the needed values with repetition determined by the label
  values_rep <- newdata[,label]

  # Transform into matrix form to be able to use rowProds.
  # This is needed for the case of single labels or single variables,
  # that sadly return a vector instead of the desired matrix when selecting
  # the needed variables in the previous step.
  # The matrix needs to keep the rows as the observations.
  M <- matrix(values_rep, nrow = nrow(newdata))

  # Perform the product of al values involved in the monomial determined by the
  # label
  var_prod <- matrixStats::rowProds(M)

  return(var_prod)
}
