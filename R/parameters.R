#' Get the parameters of the model required by the nn2poly algorithm
#'
#' @return \code{list} of length 4 with the following items:
#' - \code{weights_list}: list of weights and biases. Each element of the list
#' is a matrix where the first row is the bias and the remaining rows are the
#' weights.
#' - \code{af_string_list}: the list of activation functions as strings
#' - \code{n_neurons}: the number of neurons at each layer.
#' - \code{p}: the dimension of the problem, i.e., number of predictor variables.
#'
#' @noRd
get_parameters <- function(model) {
  UseMethod("get_parameters")
}

get_parameters.keras.engine.training.Model <- function(model) {
  nlayers <- length(model$layers)

  p <- model$layers[[1]]$input_shape[[2]]

  l_params <- list()
  layer_index <- 1
  list_index  <- 1
  while(layer_index <= nlayers) {
    params <- list()

    params[["n_neurons"]] <- model$layers[[layer_index]]$get_config()$units
    params[["weights"]]   <- model$layers[[layer_index]]$get_weights()[[1]]

    neurons_previous_layer <- if (layer_index == 1) p else l_params[[list_index-1]][["n_neurons"]]

    if (nrow(params[["weights"]]) == (neurons_previous_layer + 1)) {
      params[["wb"]] <- params[["weights"]]

      # Check if the layer is one of our custom layers
      if (class(model$layers[[layer_index]])[[1]] == "R6type.Layer_Combined_L1" ||
          class(model$layers[[layer_index]])[[1]] == "R6type.Layer_Combined_L2") {

        params[["activation"]] <- model$layers[[layer_index]]$get_config()$activation
        layer_index <- layer_index + 1

      } else {
        # We assume that the next layer is the activation
        params[["activation"]] <- model$layers[[layer_index + 1]]$get_config()$activation
        layer_index <- layer_index + 2
      }
    } else {
      params[["bias"]]       <- model$layers[[layer_index]]$get_weights()[[2]]
      params[["wb"]]         <- rbind(params[["bias"]],params[["weights"]])
      params[["activation"]] <- model$layers[[layer_index]]$get_config()$activation
      layer_index <- layer_index + 1
    }
    l_params[[list_index]] <- params
    list_index <- list_index + 1
  }

  weights_list    <- lapply(l_params, function(model) model[["wb"]])
  af_string_list  <- lapply(l_params, function(model) model[["activation"]])
  n_neurons       <- lapply(l_params, function(model) model[["n_neurons"]])

  list(weights_list   = weights_list,
       af_string_list = af_string_list,
       n_neurons      = n_neurons,
       p              = p)
}

get_parameters.nn_module <- function(model) {
  if (inherits(model, "nn_module_generator")) {
    model <- model()
  }

  layers_class      <- lapply(model$children,
                              function(layer) class(layer)[[1]])
  layers_inunits    <- lapply(model$children,
                              function(layer) layer$in_features)
  layers_outunits   <- lapply(model$children,
                              function(layer) layer$out_feature)
  layers_islinear   <- sapply(model$children,
                              function(layer) inherits(layer, "nn_linear"))
  # The shape of layer$weight is (out_features, in_features) therefore we
  # must transpose it to make it of shape (in_features, out_features)
  linlayers_weights <- lapply(model$children[layers_islinear],
                              function(layer) t(as.matrix(layer$weight)))
  linlayers_bias    <- lapply(model$children[layers_islinear],
                              function(layer) t(as.matrix(layer$bias)))
  linlayers_wb      <- lapply(model$children[layers_islinear],
                              function(layer) rbind(t(as.matrix(layer$bias)),t(as.matrix(layer$weight))))
  p                 <- layers_inunits[[1]]

  forward_parsed <- torch_forward_parser(model)
  functions_order <- forward_parsed$functions_order
  functions_order_class <- forward_parsed$functions_order_class

  last_linear <- functions_order_class[length(functions_order_class)] == "nn_linear"

  af_string_list <- list()

  for (i in 1:length(functions_order)) {
    if (functions_order_class[[i]] == "nn_linear") {
      # If last iteration and linear, add linear as af
      if (i == length(functions_order)) {
        af_string_list <- append(af_string_list, "linear")
      }
      next
    }
    af_string_list <- append(af_string_list, functions_order[[i]])
  }

    list(weights_list   = linlayers_wb,
       af_string_list = af_string_list,
       n_neurons      = layers_outunits[layers_islinear],
       p              = p)

}
