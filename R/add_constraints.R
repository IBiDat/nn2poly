# Functions needed to add the desired constraints by nn2poly to a given keras
# model.

# These constraints are expected to affect at the same time bias
# and kernel weights, something that tensorflow does not support by default.
# The idea is to bound the norm of the weight vector affecting arriving to each
# neuron, including the bias of that neuron. The supported norms are L1 and L2,
# but L1 is preferred, with 1 as it maximum norm bound.


#' Add custom constraints to the weights of a keras neural network.
#'
#' This function is designed to transform a given keras NN (feed forward, dense),
#' into a NN with the same structure, parameters and weights but with our custom
#' layers that impose L1 and L2 norm constraints
#'
#' @param model A keras model to which the restrictions on the weights are to be applied.
#' @param type The type of constraint you want to apply on the model. Currently,
#' 'l1_norm' and 'l2_norm' can be applied.
#' @param ... Additional arguments (unused).
#'
#' @return A keras model with the custom constraints applied.
#'
#' @export
add_constraints <- function(model,
                            type = c("l1_norm", "l2_norm"),
                            ...) {
  UseMethod("add_constraints")
}

#' @export
add_constraints.keras.engine.training.Model <- function(model,
                                                        type = c("l1_norm", "l2_norm"),
                                                        ...) {

  attr(model, "constraint") <- match.arg(type)
  class(model) <- c("nn2poly", class(model))
  model
}

#' @export
add_constraints.luz_module_generator <- function(model,
                                                 type = c("l1_norm", "l2_norm"),
                                                 ...) {
  luz_model_sequential_check(model)
  attr(model, "constraint") <- match.arg(type)
  class(model) <- c("nn2poly", class(model))
  model
}
