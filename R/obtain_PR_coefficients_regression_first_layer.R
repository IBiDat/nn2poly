#' Title
#'
#' @param weights_list List containing a matrix of weights and bias for each
#' layer
#' @param g The derivatives of the activation function in the first layer
#' g vector of length q+1 such that g=(g(0),g'(0),g''(0),...,g^{(q)}(0))
#' TODO: CHANGE THIS TO MATCH THE NOTATION SUED IN OTHER FUNCTIONSx
#' @param output_index An index denoting which of the neurons (or outputs) in
#' the layer we are computing.
#'
#' @return
#' @noRd
#'


obtain_PR_coefficients_regression_first_layer <- function(weights_list,
                                                          g,
                                                          output_index) {
  # weights should be a list obtained from "obtainWeights"
  # Each element of the list is a  matrix of shape
  # (input dimension + 1)*(output dimension).
  # In this SIngle Layer function we only need the first 2 weight matrices.
  # w matrix of size h_1*p+1, such that the elements are w_ji
  # v vector of length h_1+1 such that the elements are v_j
  # Therefore v should be only a vector  from the matrix v,
  # denoting which one by the "output index", veing indeed v_kj where k = output index

  # g vector of length q+1 such that g=(g(0),g'(0),g''(0),...,g^{(q)}(0))

  # Extract the weights:
  w <- weights_list[[1]]
  v <- weights_list[[2]][, output_index]

  # NEED TO REFACTOR ALL OF THIS TO BE CONSISTENT WITH THE NOTATION
  # BUT NOW WE WILL KEEP USING w TRANSPOSED:
  w <- t(w)

  # To follow our original notation we need to set up the values for q,p and h_1.
  # note that this is because the vector starts at 0 in our notation

  q <- length(g) - 1
  h_1 <- dim(w)[1]
  p <- dim(w)[2] - 1

  # We define the vector that will contain all the coefficents (betas) and their associated indexes (labels)

  betas <- c(0)
  labels <- c("0")

  # Now we apply the formulas described previously.

  # beta 0 (special case)

  betas[1] <- v[1] # first get v[1]=v_0 in our notation

  # Now obtain the summation:
  for (j in 1:h_1) {
    aux <- 0 # this auxiliar variable will store the inner summation
    for (n in 0:q) {
      aux <- aux + g[n + 1] * w[j, 1]^n # we have to use g[n+1] to obtain g^(n)/n!, because the function taylor already includes the term 1/n!
    }
    betas[1] <- betas[1] + v[j + 1] * aux # we have to use v[j+1] to obtain v_j
  }


  # The rest of the betas:

  # For each t from 1 to q, where t is the number of subindexes in the coefficent:

  for (t in 1:q) {
    labels_and_indexes <- generate_PR_labels_and_indexes(p, t)

    indexes <- labels_and_indexes[[1]]
    labels_betas_t <- labels_and_indexes[[2]]

    n_combinations <- nrow(indexes)

    # Now we create temporal betas and labels for the given t that we will include in the final result vector
    betas_t <- rep(0, n_combinations)

    # Loop over all the number of possible combinations
    for (combination_index in 1:n_combinations) {

      # Obtain a vector containing the indexes, l_1,l_2,...,l_t in our notation:
      l_values <- indexes[combination_index, ]

      # Now we can obtain the vector m=(m_1,...,m_p). The value for m_0 changes with n and will be added later in the summation
      m <- rep(0, p)
      for (i in 1:p) {
        m[i] <- sum(l_values == i)
      }

      # Finally we can apply the general formula:
      for (j in 1:h_1) {
        aux <- 0
        for (n in t:q) {
          m_n <- c(n - t, m)
          aux <- aux + g[n + 1] * factorial(n) / prod(factorial(m_n)) * prod(w[j, ]^m_n)
        }
        betas_t[combination_index] <- betas_t[combination_index] + v[j + 1] * aux
      }
    }

    # Include all the values and the labels for each t in the final vector
    betas <- c(betas, betas_t)
    labels <- c(labels, labels_betas_t)
  }

  # Finally set the betas vector as a row matrix and use the labels as the column names
  betas <- t(as.matrix(betas))
  colnames(betas) <- labels
  return(betas)
}
