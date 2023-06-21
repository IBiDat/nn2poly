#' Add custom constraints to the weights of a keras neural network.
#'
#' @param model A keras model to which the restrictions on the weights are to be applied.
#' @param constraint_type The type of constraint you want to apply on the model. Currently,
#' 'l1_norm' and 'l2_norm' can be applied.
#'
#' @return A keras model with the custom cosntraints applied.
#' @export
#'
#' @examples
add_constraints <- function(model, constraint_type = "l1_norm") {
  if (!requireNamespace("keras", quietly = TRUE)) {
    stop("package 'keras' is required for this functionality", call.=FALSE)
  }


  constraint_l1_norm <- function(w) {
    norms   = keras::k_sum(keras::k_abs(w), axis = 1, keepdims = TRUE)
    desired = keras::k_clip(norms, 0, 1)
    final   = w * (desired/(keras::k_epsilon() + norms))
  }

  # crear nueva función para repetir menos código

  Layer_Combined_L1 = R6::R6Class(
    classname = "Layer_Combined_L1",
    inherit = keras$layers$Layer,
    public = list(
      initialize = function(units, activation) {
        super$initialize()
        self$units = units
        self$activation = activation
      },
      build = function(input_shape) {
        self$combined_w_b = self$add_weight(
          shape = shape(tail(input_shape, 1) + tf$ones(shape(1, 1), dtype = "int32"), self$units),
          initializer = "random_normal",
          trainable = TRUE,
          # Custom defined constraint with L1 norm
          constraint = constraint_l1_norm
        )
      },
      call = function(inputs) {
        b = self$combined_w_b[1, , drop = TRUE]
        w = self$combined_w_b[2:NULL, ]
        tf$matmul(inputs, w) + b
      }
    )
  )

  Layer_Combined_L2 = R6::R6Class(
    classname = "Layer_Combined_L2",
    inherit = keras$layers$Layer,
    public = list(
      initialize = function(units, activation) {
        super$initialize()
        self$units = units
        self$activation = activation
      },
      build = function(input_shape) {
        self$combined_w_b = self$add_weight(
          shape = shape(tail(input_shape, 1) + tf$ones(shape(1, 1), dtype = "int32"), self$units),
          initializer = "random_normal",
          trainable = TRUE,
          # maxnorm uses the L2 norm with a given max value
          constraint = constraint_maxnorm(max_value = 1, axis = 0)
        )
      },
      call = function(inputs) {
        b = self$combined_w_b[1, , drop = TRUE]
        w = self$combined_w_b[2:NULL, ]
        tf$matmul(inputs, w) + b
      }
    )
  )

  layer_combined_L1 <- keras::create_layer_wrapper(Layer_Combined_L1)
  layer_combined_L2 <- keras::create_layer_wrapper(Layer_Combined_L2)


  params      <- get_model_parameters(model)
  nlayers     <- length(params[[1]])
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
    input_shape = model$layers[[1]]$input_shape[[2]],
    layers = new_layers
  )

}

