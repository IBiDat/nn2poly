#' @importFrom generics fit
#' @export
generics::fit

#' @export
fit.nn2poly <- function(object, ...) {
  callback <- build_callback(object, attr(object, "constraint"))
  NextMethod(callbacks = append(as.list(list(...)[["callbacks"]]), callback))
}
