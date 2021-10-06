#' Evaluates the response value of a polynomual regression in the explicit case
#' where p=2 and a surface is egnerated as a matrix.
#'
#' @param x1 variable x1
#' @param x2 variable x2
#' @param betas PR coefficients
#'
#' @return matrix response?
#' @export
#'

evaluate_PR_surface <- function(x1, x2, betas) {
  # performs the result of the polynomial regression expresion given the betas and their labels.

  # join x1 and x2
  x <- c(x1, x2)

  response <- betas[1] # this gets the intercept beta_0
  for (i in 2:length(betas)) {
    # here the label is transformed into a vector of the needed length with the index of each variable
    variable_indexes <- as.integer(unlist(strsplit(colnames(betas)[i], ",")))

    # Intialize the product as 1 and loop over all the indexes l_j to obtain the product of al the needed variables x
    product <- 1
    for (j in 1:length(variable_indexes)) {
      product <- product * x[variable_indexes[j]]
    }
    # We add to the response the product of those variables with their associated beta
    response <- response + betas[i] * product
  }
  return(response)
}
