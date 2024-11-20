#' Monomial evaluation
#'
#' Evaluates each monomial of one or several polynomials on the given data.
#' Serves as local interpretation by measuring the contribution of each monomial
#' to the final prediction.
#'
#' Note that this function is unstable and subject to change. Therefore it is
#' not exported but this documentations is left available so users can use it if
#' needed to simulate data by using \code{nn2poly:::eval_local_poly()}
#'
#' @inheritParams eval_poly
#'
#' @seealso \code{eval_monomials()} is also used in [predict.nn2poly()].
#'
eval_monomials <- function(poly, newdata) {

  newdata <- preprocess_newdata(newdata)

  aux <- preprocess_poly(poly)
  poly <- aux$poly
  intercept_position <- aux$intercept_position

  # Initialize matrix which will contain results for each desired polynomial,
  # with rows equal to the rows of `poly$values`, that is, the number of
  # polynomials and columns equal to the number of observations evaluated.
  n_sample <- nrow(newdata)
  n_polynomials <- ncol(poly$values)
  n_terms <- length(poly$labels)
  response <- array(0,c(n_sample, n_terms, n_polynomials))

  for (k in 1:n_polynomials){

    # Select the desired polynomial values (column of poly$values)
    values_k <- poly$values[,k]

    # If poly has no intercept if intercept_position is NULL
    if (is.null(intercept_position)){
      # Intercept (label = 0) should always be the first element of labels at this
      # point of the function (labels reordered previously in preprocess_poly).
      # initialize the vector with 0s repeated as needed.
      start_loop <- 1
    } else {
      # Initialize the vector with the intercept value repeated as needed.
      response[,1,k] <- rep(values_k[1], nrow(newdata))
      start_loop <- 2
    }

    # Loop over all terms (labels) except the intercept
    for (j in start_loop:length(values_k)) {

      label_j <- poly$labels[[j]]

      var_prod <- multiply_variables(label_j, newdata)


      # Here instead of adding repsonse over the loop as in the normal
      # eval_poly, store it in the appropiate position.
      response[,j,k] = values_k[j] * var_prod


    }
    # Once the monomials matrix for polynomial k is ready,
    # we should reorder it in case the intercept has been moved,
    # so it preserves the original notation.
    response[,,k] <- reorder_intercept_in_monomials(response[,,k],
                                                    intercept_position,
                                                    n_sample)
  }

  return(response)
}
