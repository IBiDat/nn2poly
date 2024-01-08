#' Add constraints to a neural network
#'
#' This function sets up a neural network object with the constraints required
#' by the \code{\link{nn2poly}} algorithm. Currently supported neural network
#' frameworks are \code{keras/tensorflow} and \code{luz/torch}.
#'
#' @param object A neural network object in sequential form from one of the
#' supported frameworks.
#' @param type Constraint type. Currently, `l1_norm` and `l2_norm` are supported.
#' @param ... Additional arguments (unused).
#'
#' @details
#' Constraints are added to the model object using callbacks in their specific
#' framework. These callbacks are used during training when calling fit on the
#' model. Specifically we are using callbacks that are applied at the end of
#' each train batch.
#'
#' Models in \code{luz/torch} need to use the \code{\link{luz_model_sequential}}
#' helper in order to have a sequential model in the appropriate form.
#'
#' @return A `nn2poly` neural network object.
#'
#' @seealso [luz_model_seuqential()]
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
