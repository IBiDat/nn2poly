#' keras constraint generator
#'
#' @param ord Order of norm (default: 1).
#'
#' @return Constraint function.
#'
#' @noRd
keras_constraint <- function(ord = 1) function(object) {
  wb <- do.call(rbind, rev(object$weights))

  result <- switch(
    ord, {
      norms   <- keras::k_sum(keras::k_abs(wb), axis = 1, keepdims = TRUE)
      desired <- keras::k_clip(norms, 0, 1)
      wb * (desired / (keras::k_epsilon() + norms))
    },
    keras::constraint_maxnorm(max_value = 1, axis = 0)(wb)
  )

  object$set_weights(keras::keras_array(list(result[2:NULL, ], result[1, ])))

  object
}
