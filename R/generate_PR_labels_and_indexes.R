#' Labels generation for
#'
#' Generates the labels and indexes used to determine each term of a polynomial
#' regression for a given number of variables p and order t.
#'
#' @param p number of variables (integer)
#' @param t order of the polynomial (integer)
#'
#' @return list with indexes and labels
#'
generate_PR_labels_and_indexes <- function(p, t) {
  # Obtain all possible combinations with repetition and non ordered:
  combinations_indexes <- combinations(p, t, repeats.allowed = TRUE)
  # Number of different combinations
  n_combinations <- nrow(combinations_indexes)

  # Vector to store the labels
  labels_betas_t <- rep("label", n_combinations)

  for (i in 1:n_combinations) {
    # Create the label as a string of the form "l_1 l_2 ... l_t"
    labels_betas_t[i] <- paste(as.character(combinations_indexes[i, ]), collapse = ",")
  }

  # Output including the combinations as vectors and the labels
  output <- vector(mode = "list", length = 2)
  output[[1]] <- combinations_indexes
  output[[2]] <- labels_betas_t
  names(output) <- c("combinations_indexes", "labels")
  return(output)
}
