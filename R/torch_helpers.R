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

#' torch constraint generator
#'
#' @param ord Order of norm (default: 1).
#'
#' @return Constraint function.
#'
#' @noRd
torch_constraint <- function(ord = 1) function(object) {
  if (!inherits(object, "nn_linear"))
    return(object)

  wb <- torch::torch_tensor(
    rbind(
      t(as.matrix(object[["bias"]])),
      t(as.matrix(object[["weight"]]))
    ),
    requires_grad = TRUE
  )

  norms <- torch::linalg_vector_norm(wb, dim = 1, ord = ord)
  desired <- norms$clip(0,1)
  result <- wb * (desired / (torch::torch_tensor(1e-7) + norms))

  torch::with_no_grad({
    object[["bias"]][] <- torch::torch_tensor(
      t(as.matrix(result)[1,]),
      requires_grad = TRUE
    )

    object[["weight"]][] <- torch::torch_tensor(
      t(as.matrix(result)[-1,]),
      requires_grad = TRUE
    )
  })

  object
}
