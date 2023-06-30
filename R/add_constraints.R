# Functions needed to add the desired constraints by nn2poly to a given keras
# model.

# These constraints are expected to affect at the same time bias
# and kernel weights, something that tensorflow does not support by default.
# The idea is to bound the norm of the weight vector affecting arriving to each
# neuron, including the bias of that neuron. The supported norms are L1 and L2,
# but L1 is preferred, with 1 as it maximum norm bound.
# Here, the


#' Custom keras L1 constraint.
#'
#' This function implements an L1 constraint to the weights of a NN in keras.
#' It follows a similar structure as the already implemented L2 norm in keras.
#' Note that currently it only supports a maximum norm bound equal to 1.
#'
#' Internal function, not exported.
#'
#' @param w weights to be passed to the constraint. In usual keras layers this
#' would be the kernel weights or the bias weights, but this also works with
#' the custom layers implemented here, where bias and weights will be joined
#' together.
#'
constraint_l1_norm <- function(w) {
  norms   = keras::k_sum(keras::k_abs(w), axis = 1, keepdims = TRUE)
  desired = keras::k_clip(norms, 0, 1)
  final   = w * (desired/(keras::k_epsilon() + norms))
}





#' Add custom constraints to the weights of a keras neural network.
#'
#' This function is designed to transform a given keras NN (feed forward, dense),
#' into a NN with the same structure, parameters and weights but with our custom
#' layers that impose L1 and L2 norm constraints
#'
#' @param model A keras model to which the restrictions on the weights are to be applied.
#' @param constraint_type The type of constraint you want to apply on the model. Currently,
#' 'l1_norm' and 'l2_norm' can be applied.
#'
#' @return A keras model with the custom constraints applied.
#' @export
add_constraints <- function(model, constraint_type = "l1_norm") {
  if (!requireNamespace("keras", quietly = TRUE)) {
    stop("package 'keras' is required for this functionality", call.=FALSE)
  }

  # Custom keras layers using L1 and L2 norms. These are needed to impose
  # constraints simultaneously to the weights and bias.
  # It might be possible to reduce this code by defining both layers at the same time.
  Layer_Combined_L1 = R6::R6Class(
    classname = "Layer_Combined_L1",
    inherit = keras::keras$layers$Layer,
    public = list(
      initialize = function(units, activation) {
        super$initialize()
        self$units = units
        self$activation = activation
      },
      build = function(input_shape) {
        self$combined_w_b = self$add_weight(
          shape = keras::shape(utils::tail(input_shape, 1) + tensorflow::tf$ones(keras::shape(1, 1), dtype = "int32"), self$units),
          initializer = "random_normal",
          trainable = TRUE,
          # Custom defined constraint with L1 norm
          constraint = constraint_l1_norm
        )
      },
      call = function(inputs) {
        b = self$combined_w_b[1, , drop = TRUE]
        w = self$combined_w_b[2:NULL, ]
        tensorflow::tf$matmul(inputs, w) + b
      }
    )
  )

  Layer_Combined_L2 = R6::R6Class(
    classname = "Layer_Combined_L2",
    inherit = keras::keras$layers$Layer,
    public = list(
      initialize = function(units, activation) {
        super$initialize()
        self$units = units
        self$activation = activation
      },
      build = function(input_shape) {
        self$combined_w_b = self$add_weight(
          shape = keras::shape(utils::tail(input_shape, 1) + tensorflow::tf$ones(keras::shape(1, 1), dtype = "int32"), self$units),
          initializer = "random_normal",
          trainable = TRUE,
          # maxnorm uses the L2 norm with a given max value
          constraint = constraint_maxnorm(max_value = 1, axis = 0)
        )
      },
      call = function(inputs) {
        b = self$combined_w_b[1, , drop = TRUE]
        w = self$combined_w_b[2:NULL, ]
        tensorflow::tf$matmul(inputs, w) + b
      }
    )
  )

  # Wrapper needed to use the R6 custom classes
  layer_combined_L1 <- keras::create_layer_wrapper(Layer_Combined_L1)
  layer_combined_L2 <- keras::create_layer_wrapper(Layer_Combined_L2)


  params      <- get_model_parameters(model)
  nlayers     <- length(params$weights_list)
  new_layers  <- vector(mode = "list", length = nlayers)

  # choose the custom layer to use
  custom_layer <- switch(constraint_type,
                         "l1_norm" = layer_combined_L1,
                         "l2_norm" = layer_combined_L2,
                         layer_combined_L1)

  # add the constraints to the new layers, except the last one
  for (layer in 1:(nlayers - 1)) {
    new_layers[[layer]] <- custom_layer(
      units = params$n_neurons[[layer]],
      activation = params$af_string_list[[layer]]
    )
  }

  # add the last layer to the layers list
  new_layers[[nlayers]] <- keras::layer_dense(
    units = params$n_neurons[[nlayers]],
    activation = params$af_string_list[[nlayers]]
  )

  # return the new model with the custom constraints
  keras::keras_model_sequential(
    input_shape = params$p,
    layers = new_layers
  )

}

