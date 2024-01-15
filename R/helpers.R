#' Build a \code{luz} model composed of a linear stack of layers
#'
#' Helper function to build \code{luz} models as a sequential model, by feeding
#' it a stack of \code{luz} layers.
#'
#' @param ... Sequence of modules to be added.
#'
#' @return A `nn_sequential` module.
#'
#' @details This step is needed so we can get the activation functions and
#' layers and neurons architecture easily with \code{nn2poly:::get_parameters()}.
#' Furthermore, this step is also needed to be able to impose the needed
#' constraints when using the \code{luz/torch} framework.
#'
#' @seealso [add_constraints()]
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("luz", quietly=TRUE)) {
#' # Create a NN using luz/torch as a sequential model
#' # with 3 fully connected linear layers,
#' # the first one with input = 5 variables,
#' # 100 neurons and tanh activation function, the second
#' # one with 50 neurons and softplus activation function
#' # and the last one with 1 linear output.
#' nn <- luz_model_sequential(
#'   torch::nn_linear(5,100),
#'   torch::nn_tanh(),
#'   torch::nn_linear(100,50),
#'   torch::nn_softplus(),
#'   torch::nn_linear(50,1)
#' )
#'
#' nn
#'
#' # Check that the nn is of class nn_squential
#' class(nn)
#' }
#' }
#'
#' @export
luz_model_sequential <- function(...) {
  if (!requireNamespace("torch", quietly = TRUE))
    stop("package 'torch' is required for this functionality", call.=FALSE)

  torch::nn_module(
    classname = "nn_sequential",
    args = list(...),
    initialize = function() {
      for (i in seq_along(self$args))
        self$add_module(name = i - 1, module = self$args[[i]])
    },
    forward = function(input) {
      for (module in private$modules_)
        input <- module(input)
      input
    }
  )
}

luz_model_sequential_check <- function(object) {
  if (any(grepl("_generator", class(object))))
    object <- object()

  if (!inherits(object, "nn_sequential"))
    stop("only sequential models are supported, see '?luz_model_sequential'", call.=FALSE)
}
