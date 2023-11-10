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

build_callback.keras.engine.training.Model <- function(object,
                                                       type = c("l1_norm", "l2_norm")) {
  keras::`%py_class%`(
    keras_callback(keras::keras$callbacks$Callback), {
      on_train_batch_end <- function(batch, logs = NULL) {
        for (layer in utils::head(self$model$layers, -1))
          layer$set_weights(keras_constraint(layer = layer, type = type))
      }
    }
  )
  keras_callback()

}

