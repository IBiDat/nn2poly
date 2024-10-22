#' Local polynomial evaluation
#'
#' Evaluates each monomial of one or several polynomials on the given data.
#'
#' Note that this function is unstable and subject to change. Therefore it is
#' not exported but this documentations is left available so users can use it if
#' needed to simulate data by using \code{nn2poly:::eval_local_poly()}
#'
#' @inheritParams eval_poly
#'
#' @seealso \code{eval_local_poly()} is also used in [predict.nn2poly()].
#'
eval_poly_local <- function(poly, newdata) {

  # MODIFICATION THAT SHOULD BE FIXED IN THE PACKAGE:
  # We need to transpose the poly values to match previous behavior
  poly$values <- t(poly$values)

  # Remove names and transform into matrix (variables as columns)
  newdata <- unname(as.matrix(newdata))

  # If x is a single vector, transpose to have it as row vector:
  if(ncol(newdata)==1){
    newdata = t(newdata)
  }

  # If values is a single vector, transform into matrix
  if (!is.matrix(poly$values)){
    poly$values <- t(as.matrix(poly$values))
  }

  # If there is intercept and it is not the first element, reorder the
  # polynomial labels and values
  if (c(0) %in% poly$labels){
    intercept_position <- which(sapply(poly$labels, function(x) c(0) %in% x))
    # if (intercept_position != 1){
    #
    #   # Divide again in single observation or matrix form:
    #
    #
    #   # Store the value
    #   intercept_value <- poly$values[,intercept_position]
    #
    #   # Remove label and value
    #   poly$labels <- poly$labels[-intercept_position]
    #   poly$values <- poly$values[,-intercept_position, drop = FALSE]
    #
    #   # Add label and value back at start of list
    #   poly$labels <- append(poly$labels, c(0), after=0)
    #   poly$values <- unname(cbind(intercept_value, poly$values))
    # }
  }


  ##### Start of local eval changes ######


  # Initialize matrix which will contain results for each desired polynomial,
  # with rows equal to the rows of `poly$values`, that is, the number of
  # polynomials and columns equal to the number of observations evaluated.
  n_sample <- nrow(newdata)
  n_polynomials <- nrow(poly$values)
  n_terms <- length(poly$labels)
  response <- array(0,c(n_sample, n_terms, n_polynomials))

  for (k in 1:n_polynomials){

    # Select the desired polynomial values (row of poly$values)
    values_k <- poly$values[k,]

    # Intercept (label = 0) should always be the first element of labels at this
    # point of the function (labels reordered previously)
    if (poly$labels[[1]] == c(0)){
      response[,1,k] <- rep(values_k[1], nrow(newdata))
      start_loop <- 2
    } else {
      # If polynomial has no intercept
      start_loop <- 1
    }

    # Loop over all terms (labels) except the intercept
    for (j in start_loop:length(values_k)) {

      label_j <- poly$labels[[j]]

      # Need to differentiate between 1 single label or more to use rowProds
      if(length(label_j) == 1){
        # When single variable, it is included in 1:p, that are also the
        # number of columns in x
        var_prod <- newdata[,label_j]
      } else {
        # Here we have a term of order 2 or higher.

        # Special case if x is a single observation.
        # Selecting the vars in x returns a column instead of row in this case
        if(nrow(newdata)==1){
          var_prod <- matrixStats::colProds(as.matrix(newdata[,label_j]))
        } else {
          # Obtain the product of each variable as many times as label_i indicates
          var_prod <- matrixStats::rowProds(newdata[,label_j])
        }

      }


      # Here instead of adding repsonse over the loop as in the normal
      # eval_poly, store it in the appropiate position.
      response[,j,k] = values_k[j] * var_prod
    }
  }

  return(response)
}



if (bool_intercept == TRUE && k == intercept_position){

}
