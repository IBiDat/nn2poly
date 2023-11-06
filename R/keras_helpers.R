keras_constraint <- function(type = c("l1_norm",
                                      "l2_norm")) {
  type <- match.arg(type)

  constraint <- if (type == "l1_norm") {
    function(wb) {
      norms   <- keras::k_sum(keras::k_abs(wb), axis = 1, keepdims = TRUE)
      desired <- keras::k_clip(norms, 0, 1)
      final   <- wb * (desired/(keras::k_epsilon() + norms))

      final
    }
  } else if (type == "l2_norm") {
    keras::constraint_maxnorm(max_value = 1, axis = 0)
  }

  constraint
}
