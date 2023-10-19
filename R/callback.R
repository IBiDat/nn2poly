#' Build the appropriate callback based on the model class
#' @noRd
build_callback <- function(object,
                           type = c("l1_norm", "l2_norm")) {
  UseMethod("build_callback")
}

norm_order <- function(type = c("l1_norm", "l2_norm")) {
  as.numeric(substr(match.arg(type), 2, 2))
}

build_callback.luz_module_generator <- function(object,
                                                type = c("l1_norm", "l2_norm")) {
  ctx <- NULL
  constraint <- torch_constraint(norm_order(type))

  luz_callback <- luz::luz_callback(
    name = paste0(type, "_callback"),

    initialize = function(constraint) {
      self$constraint <- constraint
      self$to_constrain <- NULL
    },

    on_train_batch_end = function() {
      if (is.null(self$to_constrain))
        self$to_constrain <- layers_to_constrain(ctx$model)
      for (layer_name in self$to_constrain) {
        ctx$model$modules[[layer_name]]$apply(self$constraint)
      }
    }
  )

  luz_callback(constraint)
}
