#' Evaluates the response value of a polynomual regression
#'
#' @param x the input variables
#' @param betas the PR coefficients
#'
#' @return a vector with the response Y?
#' @export
#'

evaluate_PR <- function(x, betas) {
  # performs the result of the polynomial regression expresion given the betas and their labels.
  x <- unname(as.matrix(x)) # removes the colnames and rownames of the input variables when using a dataframe.

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
