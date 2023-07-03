#' Auxiliary function. It obtains some parameters of a keras model.
#'
#' @param model Keras nn model.
#'
#' @return \code{list} of length 3 with the following items:
#' - \code{weights_list}  list of weights.
#' - \code{af_string_list}: the list of activation functions as strings
#' - \code{n_nurons}: the number of neurons at each layer.
#' - \code{p}: the dimension of the problem, i.e., number of predictor variables.
#'
get_model_parameters <- function(model) { # test needed for this function

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

      # check if the layer is one of our custom layers
      if (class(model$layers[[layer_index]])[[1]] == "R6type.Layer_Combined_L1" ||
          class(model$layers[[layer_index]])[[1]] == "R6type.Layer_Combined_L2") {
        params[["activation"]] <- model$layers[[layer_index]]$get_config()$activation
        layer_index <- layer_index + 1
      } else {
        # we assume that the next layer is the activation
        params[["activation"]] <- model$layers[[layer_index + 1]]$get_config()$activation
        layer_index <- layer_index + 2
      }
    } else {
      params[["bias"]] <- model$layers[[layer_index]]$get_weights()[[2]]
      params[["wb"]] <- rbind(params[["bias"]],params[["weights"]])
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
       p = p)
}
