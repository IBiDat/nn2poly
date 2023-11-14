#' Luz Model composed of a linear stack of layers
#'
#' @param ... Sequence of modules to be added.
#'
#' @return A `nn_sequential` module.
#'
#' @export
luz_model_sequential <- function(...) {
  if (!requireNamespace("torch", quietly = TRUE))
    stop("package 'torch' is required for this functionality", call.=FALSE)

  torch::nn_module(
    classname = "nn_sequential",
    args = list(...),
    initialize = function() {
      for (i in seq_along(self$args))
        self$add_module(name = i - 1, module = self$args[[i]])
    },
    forward = function(input) {
      for (module in private$modules_)
        input <- module(input)
      input
    }
  )
}

luz_model_sequential_check <- function(object) {
  if (any(grepl("_generator", class(object))))
    object <- object()

  if (!inherits(object, "nn_sequential"))
    stop("only sequential models are supported, see '?luz_model_sequential'", call.=FALSE)
}
