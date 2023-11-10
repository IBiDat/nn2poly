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
get_parameters <- function(object) {
  UseMethod("get_parameters")
}

get_parameters.keras.engine.training.Model <- function(object) {
  nlayers <- length(object$layers)

  p <- object$layers[[1]]$input_shape[[2]]

  l_params <- vector(mode = "list", length = nlayers - 1)
  list_index  <- 1

  for (layer in object$layers) {
    params <- vector(mode = "list", length = 3)
    names(params) <- c("n_neurons", "wb", "activation")

    params[["n_neurons"]] <- layer$get_config()$units
    params[["wb"]] <- rbind(
      layer$get_weights()[[2]],
      layer$get_weights()[[1]]
    )
    params[["activation"]] <- layer$get_config()$activation

    l_params[[list_index]] <- params
    list_index <- list_index + 1
  }

  weights_list    <- lapply(l_params, "[[", "wb")
  af_string_list  <- lapply(l_params, "[[", "activation")
  n_neurons       <- lapply(l_params, "[[", "n_neurons")

  list(weights_list   = weights_list,
       af_string_list = af_string_list,
       n_neurons      = n_neurons,
       p              = p)
}

get_parameters.nn_module <- function(object) {
  luz_model_sequential_check(object)

  if (inherits(object, "nn_module_generator"))
    object <- object()

  is_linear <- sapply(object$children, inherits, "nn_linear")

  # The shape of layer$weight is (out_features, in_features) therefore we
  # must transpose it to make it of shape (in_features, out_features)
  weights_list <- lapply(object$children[is_linear],
                         function(layer) rbind(t(as.matrix(layer$bias)),
                                               t(as.matrix(layer$weight))))

  af_string_list <- sapply(object$children[which(is_linear) + 1],
                           function(layer) sub("nn_", "", class(layer)[1]))
  af_string_list <- as.list(sub("NULL", "linear", af_string_list))

  n_neurons <- sapply(object$children[is_linear], "[[", "out_feature")
  p <- object$children[[1]]$in_features

  list(weights_list   = unname(weights_list),
       af_string_list = unname(af_string_list),
       n_neurons      = unname(n_neurons),
       p              = p)
}

get_parameters.luz_module_fitted <- function(object) {
  get_parameters(object$model)
}
