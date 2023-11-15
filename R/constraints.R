#' Add constraints to a neural network
#'
#' This function sets up a neural network object with the constraints required
#' by the \code{\link{nn2poly}} algorithm.
#'
#' @param object A neural network object.
#' @param type Constraint type. Currently, `l1_norm` and `l2_norm` are supported.
#' @param ... Additional arguments (unused).
#'
#' @return A `nn2poly` neural network object.
#'
#' @export
add_constraints <- function(object, type = c("l1_norm", "l2_norm"), ...) {
  UseMethod("add_constraints")
}

.add_constraints <- function(object, type = c("l1_norm", "l2_norm"), ...) {
  attr(object, "constraint") <- match.arg(type)
  class(object) <- c("nn2poly", class(object))
  object
}

#' @export
add_constraints.keras.engine.training.Model <- function(object, ...) {
  .add_constraints(object, ...)
}

#' @export
add_constraints.luz_module_generator <- function(object, ...) {
  luz_model_sequential_check(object)
  .add_constraints(object, ...)
}
