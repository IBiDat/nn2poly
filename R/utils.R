#' Auxiliar function used to transform some of the used activation functions
#' in the neural network from their string name into an R function.
#'
#' @param af_string_list List of strings containing the predefined possible
#' activation functions, i.e., "softplus", "tanh", "sigmoid" or "linear".
#'
#' @return List of R functions associated with the string names provided.
#'
#' @noRd
string_to_function <- function(af_string_list) {
  lapply(af_string_list, function(i) {
    switch(
      i,
      "softplus" = function(x) log(1 + exp(x)),
      "tanh"     = function(x) tanh(x),
      "sigmoid"  = function(x) 1 / (1 + exp(-x)),
      "linear"   = function(x) x,
      stop("Function '", i, "' not supported", call.=FALSE)
    )
  })
}

# Auxiliar function used in keras constraints and callbacks as Python is needed.
py_load_class <- function(name) {
  file <- system.file(paste0("python/", name, ".py"), package="nn2poly")
  reticulate::py_run_file(file)[name]
}
