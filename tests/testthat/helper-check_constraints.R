
check_weight_constraints <- function(weights, maxnorm) {
  #### Compute the norm for the full matrix ####
  n_weights <- length(weights)
  # Initialize an empty list of length n_weights, we will add the elements with a name
  weights_norms <- vector(mode = "list", length = n_weights)

  # Loop over each full matrix in the list
  for (i in 1:n_weights) {

    # Obtain number of columns for the given matrix
    # (with this notation the weight vectors that we want to compute the norm
    # are the columns)
    n_cols <- ncol(weights[[i]])

    # Initialize vector that will contain the norms of each column
    weights_norms[[i]] <- vector(mode = "numeric", length = n_cols)

    # Loop over each column
    for (j in 1:n_cols) {
      if (maxnorm[[1]] == "l1_norm") {
        norm <- pracma::Norm(weights[[i]][, j], 1)
        weights_norms[[i]][j] <- norm
      } else if (maxnorm[[1]] == "l2_norm") {
        norm <- pracma::Norm(weights[[i]][, j], 2)
        weights_norms[[i]][j] <- norm
      } else if (maxnorm[[1]] == "unit") {
        norm <- pracma::Norm(weights[[i]][, j], 2)
        weights_norms[[i]][j] <- norm
      } else {
        print("Imprecise norm. Computing the l1 norm...")
        norm <- pracma::Norm(weights[[i]][, j], 1)
        weights_norms[[i]][j] <- norm
      }

      # Name the list item
      names(weights_norms)[i] <- paste("Full W to layer", i)
    }
  }


  output <- weights_norms

  return(output)
}
