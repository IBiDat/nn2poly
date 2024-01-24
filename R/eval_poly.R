#' Polynomial evaluation
#'
#' Evaluates one or several polynomials on the given data.
#'
#' Note that this function is unstable and subject to change. Therefore it is
#' not exported but this documentations is left available so users can use it if
#' needed to simulate data by using \code{nn2poly:::eval_poly()}
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
#' Example: If \code{labels} contains the integer vector c(1,1,3) at position
#' 5, then the value stored in \code{values} at row 5 is the coefficient
#' associated with the term x_1^2*x_3.
#'
#' @param newdata Input data as matrix, vector or dataframe.
#' Number of columns (or elements in vector) should be the number of variables
#' in the polynomial (dimension p). Response variable to be predicted should
#' not be included.
#'
#' @return Returns a matrix containing the evaluation of the polynomials.
#' Each column corresponds to each polynomial used and each row to each
#' observation, meaning that each column vector corresponds to the results of
#' evaluating all the given data for each polynomial.
#'
#' @seealso \code{eval_poly()} is also used in [predict.nn2poly()].
#'
eval_poly <- function(poly, newdata) {

  # Remove names and transform into matrix (variables as columns)
  newdata <- unname(as.matrix(newdata))

  # If newdata is a single vector, transpose to have it as row vector:
  if(ncol(newdata)==1){
    newdata = t(newdata)
  }

  # If values is a single vector, transform into matrix
  if (!is.matrix(poly$values)){
    poly$values <- as.matrix(poly$values)
  }

  # Detect if the polynomial has intercept or not, needed in later steps
  bool_intercept <- FALSE
  if (c(0) %in% poly$labels) bool_intercept <- TRUE


  # If there is intercept and it is not the first element, reorder the
  # polynomial labels and values
  if (bool_intercept){
    intercept_position <- which(sapply(poly$labels, function(x) c(0) %in% x))
    if (intercept_position != 1){

      # Store the value
      intercept_value <- poly$values[intercept_position,]

      # Remove label and value
      poly$labels <- poly$labels[-intercept_position]
      poly$values <- poly$values[-intercept_position,, drop = FALSE]

      # Add label and value back at start of list
      poly$labels <- append(poly$labels, c(0), after=0)
      poly$values <- unname(rbind(intercept_value, poly$values))
    }
  }



  # Initialize matrix which will contain results for each desired polynomial,
  # with columns equal to the columns of `poly$values`, that is, the number of
  # polynomials and rows equal to the number of observations evaluated.
  n_polynomials <- ncol(poly$values)
  response <- matrix(0, nrow = nrow(newdata), ncol = n_polynomials)
  for (j in 1:n_polynomials){

    # Select the desired polynomial values (row of poly$values)
    values_j <- poly$values[,j]

    # Intercept (label = 0) should always be the first element of labels at this
    # point of the function (labels reordered previously)
    if (bool_intercept){
      response_j <- rep(values_j[1], nrow(newdata))
      start_loop <- 2
    } else {
      response_j <- rep(0, nrow(newdata))
      start_loop <- 1
    }

    # Loop over all terms (labels) except the intercept
    for (i in start_loop:length(values_j)) {

      label_i <- poly$labels[[i]]

      # Need to differentiate between 1 single label or more to use rowProds
      if(length(label_i) == 1){
        # When single variable, it is included in 1:p, that are also the
        # number of columns in newdata
        var_prod <- newdata[,label_i]
      } else {
        # Special case if newdata is a single observation.
        # Selecting the vars in newdata returns a column instead of row in this case
        if(nrow(newdata)==1){
          var_prod <- matrixStats::colProds(as.matrix(newdata[,label_i]))
        } else {
          # Obtain the product of each variable as many times as label_i indicates
          var_prod <- matrixStats::rowProds(newdata[,label_i])
        }

      }


      # We add to the response the product of those variables
      # with their associated coefficient value
      response_j <- response_j + values_j[i] * var_prod
    }
    response[,j] <- response_j
  }

  # Check if it is a single polynomial and transform to vector:
  if (dim(response)[2]==1){
    response <- as.vector(response)
  }

  return(response)
}
