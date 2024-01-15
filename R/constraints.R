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
#' @seealso [luz_model_sequential()]
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("keras", quietly=TRUE)) {
#'   # ---- Example with a keras/tensorflow network ----
#'   # Build a small nn:
#'   nn <- keras::keras_model_sequential()
#'   nn <- keras::layer_dense(nn, units = 10, activation = "tanh", input_shape = 2)
#'   nn <- keras::layer_dense(nn, units = 1, activation = "linear")
#'
#'   # Add constraints
#'   nn_constrained <- add_constraints(nn, constraint_type = "l1_norm")
#'
#'   # Check that class of the constrained nn is "nn2poly"
#'   class(nn_constrained)[1]
#' }
#'
#' if (requireNamespace("luz", quietly=TRUE)) {
#'   # ---- Example with a luz/torch network ----
#'
#'   # Build a small nn
#'   nn <- luz_model_sequential(
#'     torch::nn_linear(2,10),
#'     torch::nn_tanh(),
#'     torch::nn_linear(10,1)
#'   )
#'
#'   # With luz/torch we need to setup the nn before adding the constraints
#'   nn <- luz::setup(module = nn,
#'     loss = torch::nn_mse_loss(),
#'     optimizer = torch::optim_adam,
#'   )
#'
#'   # Add constraints
#'   nn <- add_constraints(nn)
#'
#'   # Check that class of the constrained nn is "nn2poly"
#'   class(nn)[1]
#' }
#' }
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
