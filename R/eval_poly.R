#' Evaluates one or several polynomials over one or more data points in the
#' desired variables.
#'
#' @param x Input data as matrix, vector or dataframe.
#' The number of columns should be the number of variables in the polynomial
#' (the dimension p). Response variable to be predicted should not be included.
#'
#' @param poly List containing 2 items: \code{labels} and \code{values}.
#' - \code{labels} List of integer vectors with same length (or number of rows)
#' as \code{values}, where each integer vector denotes the combination of
#' variables associated to the coefficient value stored at the same position in
#' \code{values}. Note that the variables are numbered from 1 to p.
#' - \code{values} Matrix (or also a vector if single polynomial), where each
#' row represents a polynomial, with same number of columns as the length of
#' \code{labels}, containing at each column the value of the coefficient
#' given by the equivalent label in that same position.
#'
#' Example: If \code{labels} contains the integer vector c(1,1,3) at position
#' 5, then the value stored in \code{values} at position 5 is the coefficient
#' associated with the term x_1^2*x_3.
#'
#' @return Matrix containing the evaluation of the polynomials. Each row
#' corresponds to each polynomial used and each column to each observation,
#' meaning that each row vector corresponds to the results of evaluating all the
#' given data for each polynomial.
#'
#' @examples
#' # Create the polynomial 1 + (-1)·x_1 + 1·x_2 + 0.5·(x_1)^2 as a list
#' poly <- list()
#' poly$values <- c(1,-1,1,0.5)
#' poly$labels <- list(c(0),c(1),c(2),c(1,1))
#' # Create two observations, (x_1,x_2) = (1,2) and (x_1,x_2) = (3,1)
#' x <- rbind(c(1,2), c(3,1))
#' # Evaluate the polynomial on both observations
#' eval_poly(x,poly)
#'
#' @export
#'

eval_poly <- function(x, poly) {

  # Remove names and transform into matrix (variables as columns)
  x <- unname(as.matrix(x))

  # If x is a single vector, transpose to have it as row vector:
  if(ncol(x)==1){
    x = t(x)
  }

  # If values is a single vector, transform into matrix
  if (!is.matrix(poly$values)){
    poly$values <- t(as.matrix(poly$values))
  }

  # If there is intercept and it is not the first element, reorder the
  # polynomial labels and values
  if (c(0) %in% poly$labels){
    intercept_position <- which(sapply(poly$labels, function(x) c(0) %in% x))
    if (intercept_position != 1){

      # Divide again in single observation or matrix form:


      # Store the value
      intercept_value <- poly$values[,intercept_position]

      # Remove label and value
      poly$labels <- poly$labels[-intercept_position]
      poly$values <- poly$values[,-intercept_position, drop = FALSE]

      # Add label and value back at start of list
      poly$labels <- append(poly$labels, c(0), after=0)
      poly$values <- unname(cbind(intercept_value, poly$values))
    }
  }



  # Initialize matrix which will contain results for each desired polynomial,
  # with rows equal to the rows of `poly$values`, that is, the number of
  # polynomials and columns equal to the number of observations evaluated.
  n_polynomials <- nrow(poly$values)
  response <- matrix(0, nrow = n_polynomials, ncol = nrow(x))
  for (j in 1:n_polynomials){

    # Select the desired polynomial values (row of poly$values)
    values_j <- poly$values[j,]

    # Intercept (label = 0) should always be the first element of labels at this
    # point of the function (labels reordered previously)
    if (poly$labels[[1]] == c(0)){
      response_j <- rep(values_j[1], nrow(x))
      start_loop <- 2
    } else {
      response_j <- rep(0, nrow(x))
      start_loop <- 1
    }

    # Loop over all terms (labels) except the intercept
    for (i in start_loop:length(values_j)) {

      label_i <- poly$labels[[i]]

      # Need to differentiate between 1 single label or more to use rowProds
      if(length(label_i) == 1){
        # When single variable, it is included in 1:p, that are also the
        # number of columns in x
        var_prod <- x[,label_i]
      } else {
        # Special case if x is a single observation.
        # Selecting the vars in x returns a column instead of row in this case
        if(nrow(x)==1){
          var_prod <- matrixStats::colProds(as.matrix(x[,label_i]))
        } else {
          # Obtain the product of each variable as many times as label_i indicates
          var_prod <- matrixStats::rowProds(x[,label_i])
        }

      }


      # We add to the response the product of those variables
      # with their associated coefficient value
      response_j <- response_j + values_j[i] * var_prod
    }
    response[j,] <- response_j
  }

  return(response)
}
