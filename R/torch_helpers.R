#' torch constraint generator
#'
#' @param ord Order of norm (default: 1).
#'
#' @return Constraint function.
#'
#' @noRd
torch_constraint <- function(ord = 1) function(object) {
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

#' Parse the forward function of a torch model.
#'
#' This function assumes that the forward function has been written using the pipe syntax.
#'
#' @param model A torch neural network.
#'
#' @return A \code{list} with two elements where the first element is a vector with
#' the name of the functions of the forward function in order and the second element
#' is the class of those functions in order.
#'
#' @noRd
torch_forward_parser <- function(model) {
  layers_class      <- lapply(model$children,
                              function(layer) class(layer)[[1]])
  # Parsing the forward function to obtain af_string_list
  forward <- deparse(model$forward)
  forward <- trimws(forward)
  forward <- paste(forward[-c(1,2,length(forward))], collapse = "")
  forward_components <- trimws(strsplit(forward, split = "%>%")[[1]])[-1]
  forward_componentes_splited <- strsplit(forward_components, split = "\\$")
  functions_order <- sapply(forward_componentes_splited,
                            function(x) strsplit(x[[2]], split = "\\(")[[1]][1])

  functions_order_class <- c()
  for (i in 1:length(functions_order)) {
    class_index <-which(functions_order[[i]] == names(layers_class))[[1]]
    functions_order_class <- c(functions_order_class,
                               layers_class[[class_index]])
  }

  list(
    functions_order       = functions_order,
    functions_order_class = functions_order_class
  )

}

#' Get the layers to constraint in a torch model.
#'
#' @param model A torch neural network.
#'
#' @return Vector with the names of the layers to constrain.
#'
#' @noRd
layers_to_constrain <- function(model) {
  forward_parsed <- torch_forward_parser(model)
  functions_order <- forward_parsed$functions_order
  functions_order_class <- forward_parsed$functions_order_class

  # Get the name of the layers to be constrained i.e. the linear layers that
  # are not the output layer.

  to_constrain <- functions_order[which(functions_order_class[-length(functions_order_class)] == "nn_linear")]
  to_constrain
}
