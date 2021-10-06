#' Label partitions opbtained in python according to the polynomial regression
#' terms.
#'
#' @param all_partitions nested lists obtained with python.
#'
#' @return The same nested lists with the corresponding names.
#'
label_partitions_from_python <- function(all_partitions) {
  n <- length(all_partitions)

  for (i in 1:n) {
    coeff <- all_partitions[[i]][[1]][[1]]
    label <- stringr::str_c(as.character(coeff), collapse = ",")
    names(all_partitions)[i] <- label
  }

  return(all_partitions)
}
