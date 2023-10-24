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
  luz_callback <- luz::luz_callback(
    name = paste0(type, "_callback"),

    initialize = function() {
      self$constraint <- torch_constraint(norm_order(type))
    },

    on_train_batch_end = function() {
      for (layer in utils::head(ctx$model$children, -1))
        layer$apply(self$constraint)
    }
  )

  luz_callback()
}
