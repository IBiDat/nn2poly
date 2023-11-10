#' keras weights and bias constrainer
#'
#' @param layer Layer of the keras model.
#' @param type Constraint type.
#'
#' @return Keras array with constrained weight and bias
#'
#' @noRd
keras_constraint <- function(layer,
                             type = c("l1_norm", "l2_norm")) {
  type <- match.arg(type)

  w <- layer$get_weights()[[1]]
  b <- layer$get_weights()[[2]]
  wb <- rbind(b, w)

  if (type == "l1_norm") {
    norms   <- keras::k_sum(keras::k_abs(wb), axis = 1, keepdims = TRUE)
    desired <- keras::k_clip(norms, 0, 1)
    final   <- wb * (desired / (keras::k_epsilon() + norms))
  } else if (type == "l2_norm") {
    final <- keras::constraint_maxnorm(max_value = 1, axis = 0)(wb)
  }

  keras::keras_array(list(final[2:NULL, ], final[1, ]))

}
