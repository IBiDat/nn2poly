#' Predict method for \code{nn2poly} objects.
#'
#' Predicted values obtained with a \code{nn2poly} object on given data.
#'
#' @inherit eval_poly
#' @param object Object of class inheriting from 'nn2poly'.
#' @param layers Vector containing the chosen layers from \code{object} to be
#' evaluated. If set to \code{NULL}, all layers are computed. Default is set
#' to \code{NULL}.
#' @param ... 	Further arguments passed to or from other methods.
#'
#' @details
#' Internally uses `eval_poly()` to obtain the predictions. However, this only
#' works with a objects of class \code{nn2poly} while `eval_poly()` can be used
#' with a manually created polynomial in list form.
#'
#' When \code{object} contains all the internal polynomials also, as given by
#' \code{nn2poly(object, keep_layers = TRUE)}, it is important to note that there
#' are two polynomial items per layer (input/output). These polynomial items will
#' also contain several polynomials of the same structure, one per neuron in the
#' layer, stored as matrix rows in \code{$values}. Please see the NN2Poly
#' original paper for more details.
#'
#' Note also that "linear" layers will contain the same input and output results
#' as Taylor expansion is not used and thus the polynomials are also the same.
#' Because of this, in the situation of evaluating multiple layers we provide
#' the final layer with "input" and "output" even if they are the same, for
#' consistency.
#'
#' @seealso [nn2poly()]: function that obtains the \code{nn2poly} polynomial
#' object, [eval_poly()]: function that can evaluate polynomials in general,
#' [stats::predict()]: generic predict function.
#'
#' @return Returns a matrix or list of matrices with the evaluation of each
#' polynomial at each layer as given by the provided \code{object} of class
#' \code{nn2poly}. The format can be as follows, depending on the layers
#' contained in \code{object} and the parameters \code{layers} and \code{monomials} values:
#'
#' - If \code{object} contains the polynomials of the last layer, as given by
#'   \code{nn2poly(object, keep_layers = FALSE)}, then the output is:
#'    - A matrix: if \code{monomials==FALSE}, returns a matrix containing the
#'      evaluation of the polynomials on the given data. The matrix has dimensions
#'      \code{(n_sample, n_polynomials)}, meaning that each column corresponds to the
#'      result of evaluating all the data for a polynomial. If a single polynomial is
#'      provided, the output is a vector instead of a row matrix.
#'    - A 3D array: If \code{monomials==TRUE}, returns a 3D array containing the monomials of
#'      each polynomial evaluated on the given data. The array has dimensions
#'      \code{(n_sample, n_monomial_terms, n_polynomials)}, where element
#'      \code{[i,j,k]} contains the evaluation on observation \code{i} on
#'      monomial \code{j} of polynomial \code{k}, where monomial \code{j} corresponds
#'      to the one on \code{poly$labels[[j]]}.
#'- If \code{object} contains all the internal polynomials, as given by
#' \code{nn2poly(object, keep_layers = TRUE)}, then the output is a list of
#' layers (represented by \code{layer_i}), where each of them is another list with
#' \code{input} and \code{output} elements. Each of those elements contains the
#' corresponding evaluation of the "input" or "output" polynomial at the given layer,
#' as explained in the last layer case, which will be a matrix if \code{monomials==FALSE}
#' and a 3D array if \code{monomials==TRUE}.
#'
#' @examples
#' # Build a NN structure with random weights, with 2 (+ bias) inputs,
#' # 4 (+bias) neurons in the first hidden layer with "tanh" activation
#' # function, 4 (+bias) neurons in the second hidden layer with "softplus",
#' # and 1 "linear" output unit
#'
#' weights_layer_1 <- matrix(rnorm(12), nrow = 3, ncol = 4)
#' weights_layer_2 <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' weights_layer_3 <- matrix(rnorm(5), nrow = 5, ncol = 1)
#'
#' # Set it as a list with activation functions as names
#' nn_object = list("tanh" = weights_layer_1,
#'                  "softplus" = weights_layer_2,
#'                  "linear" = weights_layer_3)
#'
#' # Obtain the polynomial representation (order = 3) of that neural network
#' final_poly <- nn2poly(nn_object, max_order = 3)
#'
#' # Define some new data, it can be vector, matrix or dataframe
#' newdata <- matrix(rnorm(10), ncol = 2, nrow = 5)
#'
#' # Predict using the obtained polynomial
#' predict(object = final_poly, newdata = newdata)
#'
#' # Predict the values of each monomial of the obtained polynomial
#' predict(object = final_poly, newdata = newdata, monomials = TRUE)
#'
#' # Change the last layer to have 3 outputs (as in a multiclass classification)
#' # problem
#' weights_layer_4 <- matrix(rnorm(20), nrow = 5, ncol = 4)
#'
#' # Set it as a list with activation functions as names
#' nn_object = list("tanh" = weights_layer_1,
#'                  "softplus" = weights_layer_2,
#'                  "linear" = weights_layer_4)
#'
#' # Obtain the polynomial representation of that neural network
#' # Polynomial representation of each hidden neuron is given by
#' final_poly <- nn2poly(nn_object, max_order = 3, keep_layers = TRUE)
#'
#' # Define some new data, it can be vector, matrix or dataframe
#' newdata <- matrix(rnorm(10), ncol = 2, nrow = 5)
#'
#' # Predict using the obtained polynomials (for all layers)
#' predict(object = final_poly, newdata = newdata)
#'
#' # Predict using the obtained polynomials (for chosen layers)
#' predict(object = final_poly, newdata = newdata, layers = c(2,3))
#'
#' @export
predict.nn2poly <- function(object,
                            newdata,
                            monomials = FALSE,
                            layers = NULL,
                            ...) {
  if (length(class(object)) > 1)
    return(NextMethod())

  # Check if object is a single polynomial or a list of polynomials.
  # If we get only the output layer, then it has to be a list with 2 elements,
  # values and labels. We check one of them:
  if (!is.null(object$labels)){
    # If we have a final polynomial, directly evaluate the results:
    result <- eval_poly(poly = object, newdata = newdata, monomials = monomials)
  } else {
    # Multiple layer case:

    # If layer = NULL, set all layers to be used
    if (is.null(layers)){
      layers <- 1:length(object)
    }

    # Check if a vector or number is given
    if (!(is.atomic(layers) & is.numeric(layers))){
      stop("Argument layers is neither a numeric vector nor NULL.",
           call. = FALSE
      )
    }
    # Check that selected layers are within object dimension
    # To do so, we need to check if "layer_maxvalue" exists:
    final_layer <- paste0("layer_", max(layers))
    if (is.null(object[[final_layer]])){
      stop("Argument layers contains elements that exceed number of layers in nn2poly object.",
           call. = FALSE
      )
    }

    # Make sure layers are ordered, just for consistent output
    layers <- sort(layers)

    # Compute results for the given layers.
    result <- list()
    for (i in layers){

      layer_name <- paste0("layer_", i)
      result[[layer_name]] <- list()

      result[[layer_name]][["input"]] <-
        eval_poly(poly = object[[layer_name]][["input"]],
                  newdata = newdata,
                  monomials = monomials)

      result[[layer_name]][["output"]] <-
        eval_poly(poly = object[[layer_name]][["output"]],
                  newdata = newdata,
                  monomials = monomials)
    }
  }

  return(result)

}

.nn2poly_extract_final_layer_index <- function(x) {
  if (!is.list(x) || is.null(names(x)))
    return(NULL)

  layer_names <- names(x)
  layer_hits <- grep("^layer_[0-9]+$", layer_names)
  if (length(layer_hits) == 0)
    return(NULL)

  layer_ids <- suppressWarnings(as.integer(sub("^layer_", "", layer_names[layer_hits])))
  layer_ids <- layer_ids[!is.na(layer_ids)]
  if (length(layer_ids) == 0)
    return(NULL)

  max(layer_ids)
}

.nn2poly_extract_final_poly <- function(x) {
  if (is.list(x) && !is.null(x$labels) && !is.null(x$values))
    return(x)

  last_layer_index <- .nn2poly_extract_final_layer_index(x)
  if (!is.null(last_layer_index)) {
    last_layer_name <- paste0("layer_", last_layer_index)
    last_layer <- x[[last_layer_name]]
    if (is.list(last_layer) && !is.null(last_layer$output) &&
        !is.null(last_layer$output$labels) && !is.null(last_layer$output$values)) {
      return(last_layer$output)
    }
  }

  stop("Input 'x' is not a recognized nn2poly polynomial object.", call. = FALSE)
}

.nn2poly_normalize_poly <- function(poly_obj) {
  if (!is.list(poly_obj) || is.null(poly_obj$labels) || is.null(poly_obj$values))
    stop("Internal error: invalid polynomial object.", call. = FALSE)

  if (is.vector(poly_obj$values))
    poly_obj$values <- matrix(poly_obj$values, ncol = 1)

  if (!is.matrix(poly_obj$values))
    stop("Internal error: polynomial values must be a matrix.", call. = FALSE)

  if (nrow(poly_obj$values) != length(poly_obj$labels))
    stop("Polynomial is inconsistent: number of labels does not match number of coefficient rows.", call. = FALSE)

  poly_obj
}

.nn2poly_validate_whole_number <- function(x,
                                           arg,
                                           allow_null = FALSE,
                                           min = NULL,
                                           max = NULL) {
  if (is.null(x)) {
    if (allow_null)
      return(NULL)
    stop("'", arg, "' must not be NULL.", call. = FALSE)
  }

  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      abs(x - round(x)) > sqrt(.Machine$double.eps)) {
    stop("'", arg, "' must be a single whole number.", call. = FALSE)
  }

  x <- as.integer(round(x))
  if (!is.null(min) && x < min)
    stop("'", arg, "' must be >= ", min, ".", call. = FALSE)
  if (!is.null(max) && x > max)
    stop("'", arg, "' must be <= ", max, ".", call. = FALSE)

  x
}

.nn2poly_validate_optional_whole_number <- function(x,
                                                    arg,
                                                    min = NULL,
                                                    max = NULL) {
  .nn2poly_validate_whole_number(
    x = x,
    arg = arg,
    allow_null = TRUE,
    min = min,
    max = max
  )
}

.nn2poly_validate_character_scalar <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
    stop("'", arg, "' must be a single non-empty string.", call. = FALSE)
  x
}

.nn2poly_extract_final_monomials <- function(newdata_monomials) {
  if (is.array(newdata_monomials))
    return(newdata_monomials)

  if (is.list(newdata_monomials)) {
    last_layer_index <- .nn2poly_extract_final_layer_index(newdata_monomials)
    if (!is.null(last_layer_index)) {
      last_layer_name <- paste0("layer_", last_layer_index)
      last_layer <- newdata_monomials[[last_layer_name]]
      if (is.list(last_layer) && !is.null(last_layer$output))
        return(last_layer$output)
    }
  }

  stop("'newdata_monomials' must be an array, or the output of predict(..., monomials=TRUE) for keep_layers=TRUE objects.", call. = FALSE)
}

.nn2poly_slice_monomials_for_obs <- function(newdata_monomials,
                                            observation_index,
                                            poly_output_index) {
  pred_dims <- dim(newdata_monomials)
  if (is.null(pred_dims) || length(pred_dims) < 2 || length(pred_dims) > 3)
    stop("'newdata_monomials' must be a 2D or 3D array.", call. = FALSE)

  num_obs_pred <- pred_dims[1]
  num_poly_outputs_pred <- if (length(pred_dims) == 3) pred_dims[3] else 1

  observation_index <- .nn2poly_validate_whole_number(
    observation_index,
    "observation_index",
    min = 1,
    max = num_obs_pred
  )
  poly_output_index <- .nn2poly_validate_whole_number(
    poly_output_index,
    "poly_output_index",
    min = 1,
    max = num_poly_outputs_pred
  )

  if (num_poly_outputs_pred == 1 && length(pred_dims) == 2) {
    return(newdata_monomials[observation_index, ])
  }
  newdata_monomials[observation_index, , poly_output_index]
}

.nn2poly_slice_monomials_for_output <- function(newdata_monomials,
                                                poly_output_index) {
  pred_dims <- dim(newdata_monomials)
  if (is.null(pred_dims) || length(pred_dims) < 2 || length(pred_dims) > 3)
    stop("'newdata_monomials' must be a 2D or 3D array.", call. = FALSE)

  if (length(pred_dims) == 2) {
    .nn2poly_validate_whole_number(poly_output_index, "poly_output_index", min = 1, max = 1)
    return(as.matrix(newdata_monomials))
  }

  poly_output_index <- .nn2poly_validate_whole_number(
    poly_output_index,
    "poly_output_index",
    min = 1,
    max = pred_dims[3]
  )

  matrix(
    newdata_monomials[, , poly_output_index],
    nrow = pred_dims[1],
    ncol = pred_dims[2]
  )
}

.nn2poly_resolve_feature_index <- function(color_by_feature,
                                           original_feature_data,
                                           variable_names = NULL) {
  if (is.null(color_by_feature))
    return(NULL)

  if (is.numeric(color_by_feature) && length(color_by_feature) == 1) {
    return(.nn2poly_validate_whole_number(
      color_by_feature,
      "color_by_feature",
      min = 1,
      max = ncol(original_feature_data)
    ))
  }

  if (is.character(color_by_feature) && length(color_by_feature) == 1) {
    color_by_feature <- .nn2poly_validate_character_scalar(color_by_feature, "color_by_feature")
    if (!is.null(colnames(original_feature_data))) {
      hit <- match(color_by_feature, colnames(original_feature_data))
      if (!is.na(hit))
        return(hit)
    }
    if (!is.null(variable_names)) {
      hit <- match(color_by_feature, variable_names)
      if (!is.na(hit))
        return(hit)
    }
    stop("'color_by_feature' did not match any column name in 'original_feature_data' or entry in 'variable_names'.", call. = FALSE)
  }

  stop("'color_by_feature' must be a single numeric index or a single character name.", call. = FALSE)
}

#' Plot method for \code{nn2poly} objects.
#'
#' Provides various plots for \code{nn2poly} objects.
#'
#' @param x A \code{nn2poly} object.
#' @param type A string selecting the plot type. Main supported types are:
#' - `"bar"`: coefficient magnitude bar plot.
#' - `"heatmap"`: second-order coefficient heatmap (diagonal = squared terms).
#' - `"local_contributions"`: per-feature local attributions for one observation
#'   by redistributing each term contribution across its variables (by multiplicity).
#' - `"waterfall"`: waterfall of per-term contributions for one observation.
#' - `"beeswarm"`: summary of per-term contributions
#'   across observations, colored by one selected original feature.
#' - `"interaction_surface"`: two-feature response surface for one output polynomial.
#' - `"interaction_network"`: network view of pairwise or higher-order interactions.
#' @param ... Additional arguments passed to specific plot types.
#'
#' @param n For `type = "bar"`, the number of top coefficients to plot.
#'
#' @param newdata_monomials For `type = "local_contributions"`, `"waterfall"`,
#'   `"beeswarm"` or `metric_network = "mean_monomial_abs"`,
#'   the output of `predict(x, newdata, monomials = TRUE)`. These values are
#'   per-term contributions (coefficient multiplied by variable product). This should be
#'   for a single observation for "local_contributions" (or specify `observation_index`)
#'   and for multiple observations for "beeswarm".
#' @param observation_index For `type = "local_contributions"`, the row index
#'   from `newdata_monomials` to plot (default: 1).
#' @param poly_output_index For local plots and multi-output polynomials,
#'   if `x` produces multiple polynomial outputs, which one to plot (default: 1).
#' @param variable_names Optional character vector of original feature names
#'   to make labels more readable in plots where variables are displayed.
#' @param max_order_to_display For `type = "local_contributions"`, the maximum
#'   term order to show in the stacked bars (default: 3).
#' @param waterfall_n For `type = "waterfall"`, the number of top non-intercept
#'   terms (by absolute contribution) to show. Remaining terms are aggregated
#'   into an `(Other)` bucket.
#'
#' @param original_feature_data For `type = "beeswarm"` or `"interaction_surface"`,
#'   a matrix or data frame of original predictor values. It is used for beeswarm
#'   coloring and to define the grid/ranges/reference values of response surfaces.
#' @param color_by_feature For `type = "beeswarm"`, which original feature to
#'   use for selected-feature coloring. Can be a numeric column index or a character name
#'   matching `colnames(original_feature_data)` or `variable_names`.
#' @param top_n_terms For `type = "beeswarm"`, an optional integer to display only
#'   the top N most important terms (based on mean absolute term contribution).
#' @param min_order For `type = "bar"`, the minimum order of terms to include.
#'   0 (default) includes intercept (order 0 for this purpose) and all terms.
#'   1 excludes intercept, showing terms of polynomial degree 1+.
#'   2 excludes intercept and linear terms, showing terms of polynomial degree 2+.
#' @param feature_pair For `type = "interaction_surface"`, a numeric index vector
#'   or character vector of length 2 specifying the two features to vary.
#' @param grid_resolution For `type = "interaction_surface"`, the number of points
#'   per dimension for the grid.
#' @param interaction_order_network For `type = "interaction_network"`, the number
#'   of unique variables that must appear in a term for it to be included.
#' @param metric_network For `type = "interaction_network"`, the metric for edge
#'   weights ("coefficient_abs" or "mean_monomial_abs").
#' @param top_n_interactions For `type = "interaction_network"`, number of top
#'   (projected) pairwise interactions to display by weight.
#' @param layout_network For `type = "interaction_network"`, either `"circle"`
#'   or `"line"`.
#'
#' @return A ggplot object.
#' @export
plot.nn2poly <- function(x, type = "bar", ...,
                         # Args for bar plot
                         n = NULL,
                         min_order = 0, # New arg for bar plot
                         # Args for local_contributions & beeswarm plots
                         newdata_monomials = NULL,
                         poly_output_index = 1,
                         variable_names = NULL,
                         # Args for local_contributions plot
                         observation_index = 1,
                         max_order_to_display = 3,
                         # Args for waterfall plot
                         waterfall_n = 15,
                         # Args for beeswarm plot
                         original_feature_data = NULL, # Still needed for beeswarm
                         color_by_feature = 1,
                         top_n_terms = NULL, # For beeswarm y-axis
                         # Args for interaction_surface plot
                         feature_pair = NULL,
                         grid_resolution = 20,
                         # Args for interaction_network plot
                         interaction_order_network = 2,
                         metric_network = "coefficient_abs", # Defaulted to coefficient_abs
                         top_n_interactions = NULL,
                         layout_network = "circle"
) {

  if (length(class(x)) > 1) {
    return(NextMethod())
  }
  type <- .nn2poly_validate_character_scalar(type, "type")
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this functionality.", call. = FALSE)
  }

  plot_object <- NULL
  poly_obj <- NULL
  available_types <- c("bar", "heatmap", "local_contributions", "waterfall",
                       "beeswarm", "interaction_surface", "interaction_network")

  if (type %in% available_types) {
    poly_obj <- .nn2poly_normalize_poly(.nn2poly_extract_final_poly(x))
  }

  # --- Common pre-processing for coefficient-based plots ("bar", "heatmap") ---
  if (type %in% c("bar", "heatmap")) {
    if (type == "bar") {
      plot_object <- plot_bar(poly_obj, n = n, variable_names = variable_names, min_order = min_order)
    } else if (type == "heatmap") {
      plot_object <- plot_heatmap(poly_obj, variable_names = variable_names)
    }
  }
  # --- Local Contributions Plot ---
  else if (type == "local_contributions") {
    if (is.null(newdata_monomials)) stop("For 'local_contributions', 'newdata_monomials' must be provided.", call. = FALSE)
    newdata_monomials_arr <- .nn2poly_extract_final_monomials(newdata_monomials)
    if (dim(newdata_monomials_arr)[2] != length(poly_obj$labels))
      stop("Term count mismatch between 'newdata_monomials' and the nn2poly object labels.", call. = FALSE)

    monomial_values_slice <- .nn2poly_slice_monomials_for_obs(
      newdata_monomials_arr,
      observation_index = observation_index,
      poly_output_index = poly_output_index
    )
    plot_object <- plot_local_contributions_internal(
      poly_obj = poly_obj, monomial_values_for_obs = monomial_values_slice,
      variable_names = variable_names, max_order_to_display = max_order_to_display
    )
  }
  # --- Waterfall Plot ---
  else if (type == "waterfall") {
    if (is.null(newdata_monomials)) stop("For 'waterfall', 'newdata_monomials' must be provided.", call. = FALSE)

    newdata_monomials_arr <- .nn2poly_extract_final_monomials(newdata_monomials)
    if (dim(newdata_monomials_arr)[2] != length(poly_obj$labels))
      stop("Term count mismatch between 'newdata_monomials' and the nn2poly object labels.", call. = FALSE)

    monomial_values_slice <- .nn2poly_slice_monomials_for_obs(
      newdata_monomials_arr,
      observation_index = observation_index,
      poly_output_index = poly_output_index
    )

    plot_object <- plot_waterfall_internal(
      poly_obj = poly_obj,
      monomial_values_for_obs = monomial_values_slice,
      variable_names = variable_names,
      waterfall_n = waterfall_n
    )
  }
  # --- Beeswarm Plot ---
  else if (type == "beeswarm") {
    if (is.null(newdata_monomials)) stop("For 'beeswarm', 'newdata_monomials' must be provided.", call. = FALSE)
    if (is.null(original_feature_data)) stop("For 'beeswarm', 'original_feature_data' must be provided for coloring.", call. = FALSE)
    top_n_terms <- .nn2poly_validate_optional_whole_number(top_n_terms, "top_n_terms", min = 1)
    newdata_monomials_arr <- .nn2poly_extract_final_monomials(newdata_monomials)
    pred_dims <- dim(newdata_monomials_arr)
    if (length(pred_dims) < 2 || length(pred_dims) > 3) stop("'newdata_monomials' must be a 2D or 3D array.", call. = FALSE)
    num_obs_pred <- pred_dims[1]; num_terms_pred <- pred_dims[2]; num_poly_outputs_pred <- if (length(pred_dims) == 3) pred_dims[3] else 1

    # Ensure original_feature_data is a matrix for consistent indexing
    if (!is.matrix(original_feature_data)) {
      original_feature_data_mat <- as.matrix(original_feature_data)
      if(!is.numeric(original_feature_data_mat) && !is.logical(original_feature_data_mat)) { # Allow logicals to become 0/1
        stop("'original_feature_data' cannot be coerced to a numeric/logical matrix.", call. = FALSE)
      }
    } else {
      original_feature_data_mat <- original_feature_data
    }

    if (nrow(original_feature_data_mat) != num_obs_pred) stop("Row count mismatch: 'original_feature_data' and 'newdata_monomials'.", call. = FALSE)
    poly_output_index <- .nn2poly_validate_whole_number(
      poly_output_index,
      "poly_output_index",
      min = 1,
      max = num_poly_outputs_pred
    )
    if (num_terms_pred != length(poly_obj$labels)) stop("Term count mismatch between 'newdata_monomials' and the nn2poly object labels.", call. = FALSE)

    color_by_feature_idx <- .nn2poly_resolve_feature_index(
      color_by_feature = color_by_feature,
      original_feature_data = original_feature_data_mat,
      variable_names = variable_names
    )
    if (is.null(color_by_feature_idx))
      color_by_feature_idx <- 1L

    monomial_values_for_plot <- .nn2poly_slice_monomials_for_output(
      newdata_monomials_arr,
      poly_output_index = poly_output_index
    )

    plot_object <- plot_beeswarm_internal(
      poly_obj = poly_obj, all_monomial_values_for_output = monomial_values_for_plot,
      original_feature_data = original_feature_data_mat, # ensure matrix form is passed
      variable_names = variable_names, color_by_feature = color_by_feature_idx, top_n_terms = top_n_terms
    )
  }

  # --- Interaction Surface Plot ---
  else if (type == "interaction_surface") {
    if (is.null(feature_pair)) stop("For 'interaction_surface', 'feature_pair' must be provided.", call. = FALSE)
    if (is.null(original_feature_data)) stop("For 'interaction_surface', 'original_feature_data' must be provided.", call. = FALSE) # original_feature_data for surface
    if (is.null(poly_obj$labels) || is.null(poly_obj$values)) stop("Input 'x' is not a valid nn2poly object for 'interaction_surface'.", call. = FALSE)

    current_original_feature_data <- original_feature_data # Use the one passed for this plot type
    plot_object <- plot_interaction_surface_internal(
      poly_obj = poly_obj, feature_pair = feature_pair,
      original_feature_data = current_original_feature_data,
      grid_resolution = grid_resolution, variable_names = variable_names,
      poly_output_index = poly_output_index
    )
  }
  # --- Interaction Network Plot ---
  else if (type == "interaction_network") {
    if (is.null(poly_obj$labels) || is.null(poly_obj$values)) stop("Input 'x' is not a valid nn2poly object for 'interaction_network'.", call. = FALSE)

    current_newdata_monomials <- newdata_monomials
    metric_network <- .nn2poly_validate_character_scalar(metric_network, "metric_network")
    metric_network <- match.arg(metric_network, c("coefficient_abs", "mean_monomial_abs"))
    if (metric_network == "mean_monomial_abs" && is.null(current_newdata_monomials)) {
      stop("If metric_network is 'mean_monomial_abs', 'newdata_monomials' must be provided for 'interaction_network'.", call. = FALSE)
    }

    if (!is.null(current_newdata_monomials))
      current_newdata_monomials <- .nn2poly_extract_final_monomials(current_newdata_monomials)

    plot_object <- plot_interaction_network_internal(
      poly_obj = poly_obj, interaction_order = interaction_order_network,
      metric = metric_network, newdata_monomials = current_newdata_monomials,
      top_n_interactions = top_n_interactions, variable_names = variable_names,
      poly_output_index = poly_output_index, layout = layout_network
    )
  } else {
    stop(paste0("Unknown plot type: '", type, "'. Available types are 'bar', 'heatmap', 'local_contributions', 'waterfall', 'beeswarm', 'interaction_surface', 'interaction_network'."), call. = FALSE)
  }
  return(plot_object)
}


#' Format a term label for display
#'
#' @param term_label_vec A numeric vector representing the term, e.g., c(1), c(1,2), c(1,1).
#' @param variable_names Optional character vector of feature names.
#' @param use_product_format_for_named Logical. If TRUE and variable_names are provided,
#'   use product format (e.g., "NameA*NameB"). Otherwise (or if variable_names is NULL),
#'   uses comma-separated numeric indices (e.g., "1,2").
#' @param use_product_format_for_numeric Logical. If TRUE and variable_names are NULL,
#'   use product format for numeric indices (e.g., "1*2"). Defaults to FALSE (uses "1,2").
#' @return A string representation of the term.
#' @noRd
format_term_label_display <- function(term_label_vec,
                                      variable_names = NULL,
                                      use_product_format_for_named = FALSE,
                                      use_product_format_for_numeric = FALSE) {
  if (length(term_label_vec) == 1 && term_label_vec[1] == 0) {
    return("0")
  }
  if (is.null(term_label_vec) || length(term_label_vec) == 0) return("NA_term")

  parts <- character(length(term_label_vec))
  use_product_format_this_term <- FALSE

  if (!is.null(variable_names)) {
    for (i in seq_along(term_label_vec)) {
      idx <- term_label_vec[i]
      if (idx > 0 && idx <= length(variable_names)) {
        parts[i] <- variable_names[idx]
      } else if (idx > 0) { # Index out of bounds for names, or 0
        parts[i] <- as.character(idx) # Fallback to numeric
      } else {
        parts[i] <- as.character(idx) # e.g. if a 0 slips through
      }
    }
    if (use_product_format_for_named) {
      use_product_format_this_term <- TRUE
    }
  } else { # No variable_names provided
    parts <- as.character(term_label_vec)
    if (use_product_format_for_numeric) {
      use_product_format_this_term <- TRUE
    }
  }

  if (use_product_format_this_term) {
    # For product format, it's common to sort parts for canonical representation, e.g., x1*x2 not x2*x1
    # However, c(1,1,2) should be x1*x1*x2, not x1*x2*x1. So simple sort is not enough.
    # For now, just join them as they appear in the label.
    # A more sophisticated approach might count and use powers, e.g., x1^2*x2
    return(paste(parts, collapse = "*"))
  } else {
    return(paste(parts, collapse = ","))
  }
}

#' Internal function for plot.nn2poly bar type.
#'
#' @param poly_obj A nn2poly object (or the filtered subset for plotting).
#' @param n Number of top coefficients to plot per polynomial output.
#' @param variable_names Optional character vector of feature names.
#' @param min_order Minimum order of terms to include (0=all including intercept, 1=degree 1+, etc.).
#' @return A ggplot object.
#' @noRd
plot_bar <- function(poly_obj, n = NULL, variable_names = NULL, min_order = 0) {
  n <- .nn2poly_validate_optional_whole_number(n, "n", min = 0)
  min_order <- .nn2poly_validate_whole_number(min_order, "min_order", min = 0)

  # --- 1. Filter terms based on min_order ---
  labels_to_keep <- list()
  original_indices_to_keep <- integer(0)

  # Determine if poly_obj$values is a matrix (multiple outputs) or vector (single output)
  # poly_obj$values should be terms x polynomial_outputs
  is_multi_output <- !is.null(dim(poly_obj$values)) && ncol(poly_obj$values) > 1
  num_outputs_in_obj <- if (is.null(dim(poly_obj$values))) 1 else ncol(poly_obj$values)

  for (i in seq_along(poly_obj$labels)) {
    term_lab <- poly_obj$labels[[i]]

    current_term_effective_order <- 0 # Default for intercept
    if (length(term_lab) == 1 && term_lab[1] == 0) {
      current_term_effective_order <- 0 # Intercept is order 0
    } else if (length(term_lab) > 0 && all(term_lab > 0)) { # All positive indices (variables)
      current_term_effective_order <- length(term_lab) # Polynomial degree of the term
    } else {
      # Mixed term or unexpected label structure, assign high order to filter out unless min_order is very low
      # Or, decide how to handle terms like c(0,1) if they were possible.
      # For now, assume labels are either c(0) or vectors of positive integers.
      # If not, this might need adjustment based on what format_term_label_display expects.
      current_term_effective_order <- length(term_lab) # Fallback, usually non-zero if not intercept
    }

    if (current_term_effective_order >= min_order) {
      labels_to_keep[[length(labels_to_keep) + 1]] <- term_lab
      original_indices_to_keep <- c(original_indices_to_keep, i)
    }
  }

  if (length(labels_to_keep) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste0("No terms found with order >= ", min_order, ".")) +
             ggplot2::theme_minimal(base_size = 10) +
             ggplot2::ggtitle(paste0("Coefficients (Order >= ", min_order, ")")))
  }

  poly_obj_filtered <- list()
  poly_obj_filtered$labels <- labels_to_keep
  if (is_multi_output) {
    poly_obj_filtered$values <- poly_obj$values[original_indices_to_keep, , drop = FALSE]
  } else { # Single output case (poly_obj$values might have been a vector or 1-col matrix originally)
    values_subset <- if(is.null(dim(poly_obj$values))) poly_obj$values[original_indices_to_keep] else poly_obj$values[original_indices_to_keep, 1]
    poly_obj_filtered$values <- matrix(values_subset, ncol = 1) # Ensure it's a matrix
  }

  # --- 2. Prepare data frame for plotting ---
  # If no number of top coefficients (n) is provided, use all filtered coefficients.
  if (is.null(n) || n > nrow(poly_obj_filtered$values)) {
    n <- nrow(poly_obj_filtered$values)
  }
  if (n == 0) { # Case where n becomes 0 after filtering if nrow(poly_obj_filtered$values) was 0
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste0("No terms to plot after filtering (n=0).")) +
             ggplot2::theme_minimal(base_size = 10) +
             ggplot2::ggtitle(paste0("Coefficients (Order >= ", min_order, ")")))
  }


  # poly_obj_filtered$values is now terms x polynomial_outputs
  coefficients_matrix <- poly_obj_filtered$values
  all_filtered_labels <- poly_obj_filtered$labels
  n_polys <- ncol(coefficients_matrix) # Number of polynomial outputs to plot

  all_df_list <- vector("list", n_polys)

  for (r in 1:n_polys) {
    poly_coeffs_for_output_r <- coefficients_matrix[, r]

    # Sort by absolute magnitude to get top n coefficients for this polynomial output.
    # Ties are resolved by the original term order for reproducibility.
    order_indices <- order(-abs(poly_coeffs_for_output_r), seq_along(poly_coeffs_for_output_r))

    top_n_actual_indices <- order_indices[1:n] # Indices within poly_coeffs_for_output_r

    top_n_values_abs <- abs(poly_coeffs_for_output_r[top_n_actual_indices])
    top_n_signs <- sign(poly_coeffs_for_output_r[top_n_actual_indices])

    # Handle exact zero coefficients' sign (map to positive)
    top_n_signs[top_n_signs == 0] <- 1

    list_labels_for_top_n <- all_filtered_labels[top_n_actual_indices]

    string_labels_for_top_n <- sapply(list_labels_for_top_n, function(lab) {
      format_term_label_display(lab, variable_names,
                                use_product_format_for_named = FALSE,
                                use_product_format_for_numeric = FALSE)
    })

    df_poly_r <- data.frame(
      term_name = string_labels_for_top_n,
      sign = as.factor(top_n_signs),
      value = top_n_values_abs,
      poly_output_id = r, # To identify polynomial if faceting
      stringsAsFactors = FALSE
    )

    order_for_axis <- order(-df_poly_r$value, seq_len(nrow(df_poly_r)))
    df_poly_r$term_display_order <- factor(
      paste(df_poly_r$term_name, df_poly_r$poly_output_id, sep = "___"),
      levels = paste(
        df_poly_r$term_name[order_for_axis],
        df_poly_r$poly_output_id[order_for_axis],
        sep = "___"
      )
    )

    all_df_list[[r]] <- df_poly_r
  }

  all_plot_df <- do.call(rbind, all_df_list)

  if (nrow(all_plot_df) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No coefficients to plot after processing.") +
             ggplot2::theme_minimal(base_size = 10) +
             ggplot2::ggtitle(paste0("Coefficients (Order >= ", min_order, ")")))
  }

  # Ensure poly_output_id is a factor for faceting title with meaningful labels
  all_plot_df$poly_facet_label <- factor(paste("Output Poly.", all_plot_df$poly_output_id))

  # --- 3. Generate Plot ---
  plot_title_text <- if (min_order == 0) "Most Important Coefficients (All Terms)"
  else if (min_order == 1) "Most Important Coefficients (Order >= 1)"
  else paste0("Most Important Interaction Coefficients (Order >= ", min_order, ")")

  p <- ggplot2::ggplot(all_plot_df,
                       ggplot2::aes(x = .data$term_display_order, # Use the reordered factor
                                     y = .data$value,
                                     fill = .data$sign)) +
    ggplot2::geom_bar(stat = "identity", colour = "black", alpha = 1, width=0.7) +
    ggplot2::scale_x_discrete(labels = function(x) sub("___[^_]+$", "", x)) +

    ggplot2::scale_fill_manual(
      name = "Sign",
      values = c("1" = "#00BA38", "-1" = "#F8766D"), # Your green/red colors
      labels = c("1" = "+", "-1" = "-"),
      drop = FALSE # Show all legend items even if one sign not present
    ) +
    ggplot2::labs(title = plot_title_text,
                  x = "Polynomial Term",
                  y = "Coefficient (absolute value)")


    p <- p + ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size=10),
        axis.text.y = ggplot2::element_text(size=10),
        legend.position = "top",
        legend.direction = "horizontal",
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
        strip.background = ggplot2::element_rect(fill="grey90", linetype="blank"),
        strip.text = ggplot2::element_text(face="bold")
      )


  if (n_polys > 1) {
    # For coord_flip, scales = "free_y" means different terms can appear per facet
    # scales = "free_x" means y-axis (now value) can have different scales
    p <- p + ggplot2::facet_wrap(~poly_facet_label, scales = "free_x")
  }

  return(p)
}


#' Internal function for plot.nn2poly heatmap type.
#'
#' @inheritParams plot.nn2poly
#'
#' @return A ggplot object.
#' @noRd
plot_heatmap <- function(poly_obj, variable_names = NULL) { # Changed x to poly_obj

  coefficients_matrix <- poly_obj$values # Matrix: terms x n_polynomial_outputs
  all_labels_list <- poly_obj$labels   # List of label vectors for each term

  # Filter for second-order term labels (length == 2, and positive variable indices)
  is_second_order <- sapply(all_labels_list, function(lab) {
    !is.null(lab) && length(lab) == 2 && all(lab > 0) && is.numeric(lab)
  })

  if (!any(is_second_order)) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No second-order terms with positive variable indices found.") +
             ggplot2::theme_minimal(base_size = 10) +
             ggplot2::ggtitle("Second-Order Coefficients Heatmap"))
  }

  second_order_labels_subset <- all_labels_list[is_second_order]
  second_order_values_matrix <- coefficients_matrix[is_second_order, , drop = FALSE]

  # Determine var_names for axes
  present_vars <- unique(unlist(second_order_labels_subset))
  if (length(present_vars) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x=0.5, y=0.5, label="No variables found in second-order terms.") +
             ggplot2::theme_minimal(base_size = 10) +
             ggplot2::ggtitle("Second-Order Coefficients Heatmap"))
  }
  max_var_idx <- max(present_vars)

  if(max_var_idx == 0){ # Should not happen if all(lab > 0) filter is effective
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x=0.5, y=0.5, label="Max variable index is 0.") +
             ggplot2::theme_minimal(base_size = 10) +
             ggplot2::ggtitle("Second-Order Coefficients Heatmap"))
  }

  axis_var_labels <- character(max_var_idx)
  if (!is.null(variable_names)) {
    for (i in 1:max_var_idx) {
      if (i <= length(variable_names)) {
        axis_var_labels[i] <- variable_names[i]
      } else {
        axis_var_labels[i] <- as.character(i) # Fallback
      }
    }
  } else {
    axis_var_labels <- as.character(1:max_var_idx) # Default: "1", "2", ...
  }

  n_polys <- ncol(second_order_values_matrix)

  all_heatmap_df_list <- vector("list", n_polys)

  for (p_idx in 1:n_polys) {
    current_poly_coeffs <- second_order_values_matrix[, p_idx]

    heatmap_matrix <- matrix(0, nrow = max_var_idx, ncol = max_var_idx,
                             dimnames = list(axis_var_labels, axis_var_labels))

    for (i in seq_along(second_order_labels_subset)) {
      label <- second_order_labels_subset[[i]] # label is c(var1_idx, var2_idx)
      value <- current_poly_coeffs[i]

      # Ensure indices are within bounds (should be, by construction of max_var_idx)
      if (all(label <= max_var_idx)) {
        if (label[1] == label[2]) { # Squared term: x_i^2
          heatmap_matrix[label[1], label[1]] <- value
        } else { # Interaction term: x_i*x_j
          heatmap_matrix[label[1], label[2]] <- value
          heatmap_matrix[label[2], label[1]] <- value # Symmetric
        }
      }
    }

    heatmap_df_poly <- as.data.frame(as.table(heatmap_matrix))
    colnames(heatmap_df_poly) <- c("Var1", "Var2", "Value")
    # Ensure Var1 and Var2 are factors with levels in the correct order
    heatmap_df_poly$Var1 <- factor(heatmap_df_poly$Var1, levels = axis_var_labels)
    heatmap_df_poly$Var2 <- factor(heatmap_df_poly$Var2, levels = rev(axis_var_labels))

    heatmap_df_poly$poly_index <- p_idx
    all_heatmap_df_list[[p_idx]] <- heatmap_df_poly
  }

  combined_heatmap_df <- do.call(rbind, all_heatmap_df_list)
  combined_heatmap_df$poly_facet_label <- factor(paste("Output Polynomial", combined_heatmap_df$poly_index))

  # Generate the heatmap plot
  final_heatmap_plot <- ggplot2::ggplot(combined_heatmap_df, ggplot2::aes(x = .data$Var1, y = .data$Var2, fill = .data$Value)) +
    ggplot2::geom_tile(color = "white", na.rm = TRUE) + # na.rm in case some interactions are missing
    ggplot2::scale_fill_gradient2(low = "#F8766D", mid = "white", high = "#00BA38", # User's colors
                                  midpoint = 0, name = "Coefficient", na.value = "grey90") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::labs(x = "Variable", y = "Variable") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(),
      axis.text.y = ggplot2::element_text(),
      legend.position = "top", legend.direction = "horizontal",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
      strip.background = ggplot2::element_rect(fill="grey90", linetype="blank"),
      strip.text = ggplot2::element_text(face="bold")
    ) +
    ggplot2::coord_fixed() # Ensures tiles are square

  if (n_polys > 1) {
    final_heatmap_plot <- final_heatmap_plot + ggplot2::facet_wrap(~poly_facet_label)
  } else {
    final_heatmap_plot <- final_heatmap_plot + ggplot2::ggtitle("Second-Order Coefficients Heatmap")
  }

  return(final_heatmap_plot)
}

# Helper to get max variable index (p) from a list of labels
get_max_var_index_from_labels <- function(labels_list) {
  all_vars <- unlist(labels_list)
  all_vars <- all_vars[all_vars > 0] # Exclude 0 (intercept) and any non-positive indices
  if (length(all_vars) == 0) return(0)
  return(max(all_vars, na.rm = TRUE))
}

#' Internal function for plot.nn2poly local_contributions type.
#' @noRd
plot_local_contributions_internal <- function(poly_obj,
                                              monomial_values_for_obs, # Vector for one obs, one output
                                              variable_names = NULL,
                                              max_order_to_display = 3) {

  max_order_to_display <- .nn2poly_validate_whole_number(
    max_order_to_display,
    "max_order_to_display",
    min = 1
  )

  # --- Input Validation ---
  if (!is.list(poly_obj) || is.null(poly_obj$labels)) {
    stop("'poly_obj' must be a valid nn2poly object with labels.")
  }
  if (!is.numeric(monomial_values_for_obs) || !is.vector(monomial_values_for_obs)) {
    stop("'monomial_values_for_obs' must be a numeric vector.")
  }
  if (length(poly_obj$labels) != length(monomial_values_for_obs)) {
    stop("Length of 'poly_obj$labels' and 'monomial_values_for_obs' must match.")
  }

  # --- Calculate Contributions ---
  contributions_list <- list()
  for (i in seq_along(poly_obj$labels)) {
    term_label <- poly_obj$labels[[i]]       # e.g., c(1), c(1,2), c(1,1,2)
    term_value_for_obs <- monomial_values_for_obs[i]

    # Skip if this term contribution is effectively zero
    if (abs(term_value_for_obs) < .Machine$double.eps^0.75) { # More robust check for zero
      next
    }

    # Skip intercept term (label c(0)) from feature attribution
    if (length(term_label) == 1 && term_label[1] == 0) {
      next
    }

    current_term_order <- length(term_label) # Order of the term

    # Skip if term order is 0 (shouldn't happen if intercept is skipped) or exceeds display limit
    if (current_term_order == 0 || current_term_order > max_order_to_display) {
      next
    }

    # Filter for actual variable indices (positive integers) within the term label
    vars_in_term_label <- term_label[term_label > 0]

    if (length(vars_in_term_label) == 0) { # No actual variables in this term
      next
    }

    # Total number of variable occurrences in the term (e.g., for c(1,1,2), this is 3)
    total_var_occurrences_in_term <- length(vars_in_term_label)

    # Count occurrences of each unique variable in this specific term's label
    # For c(1,1,2): table gives "1" -> 2, "2" -> 1
    var_counts_in_term <- table(vars_in_term_label)

    # Distribute the term's value proportionally to variable counts
    for (var_idx_char in names(var_counts_in_term)) {
      var_idx <- as.integer(var_idx_char)
      count_this_var <- var_counts_in_term[[var_idx_char]] # How many times this var_idx appears in vars_in_term_label

      # Proportion for this variable in this term
      proportion_for_this_var <- count_this_var / total_var_occurrences_in_term

      attributed_value <- term_value_for_obs * proportion_for_this_var

      contributions_list[[length(contributions_list) + 1]] <-
        data.frame(
          variable_idx = var_idx,
          term_order_num = current_term_order, # Overall order of the term
          contribution = attributed_value,
          stringsAsFactors = FALSE
        )
    }
  }

  if (length(contributions_list) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No feature contributions to display for this observation (or selected orders).") +
             ggplot2::theme_minimal(base_size = 10) + ggplot2::ggtitle("Local Feature Contributions"))
  }

  plot_df <- do.call(rbind, contributions_list)

  # Aggregate contributions: sum up all contributions for each variable_idx and term_order_num
  # E.g., if var 1 gets contribution from term c(1) [1st order] and from term c(1,1,2) [3rd order]
  plot_df_agg <- stats::aggregate(contribution ~ variable_idx + term_order_num, data = plot_df, FUN = sum)

  # Filter out effectively zero aggregated contributions
  plot_df_agg <- plot_df_agg[abs(plot_df_agg$contribution) > .Machine$double.eps^0.75, ]

  if (nrow(plot_df_agg) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = "All aggregated feature contributions are negligible.") +
             ggplot2::theme_minimal(base_size = 10) + ggplot2::ggtitle("Local Feature Contributions"))
  }

  # --- Prepare for Plotting ---
  max_present_order <- min(max(plot_df_agg$term_order_num, na.rm = TRUE), max_order_to_display)

  if(is.infinite(max_present_order) || max_present_order < 1) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No contributions to display for the selected orders after aggregation.") +
             ggplot2::theme_minimal(base_size = 10) + ggplot2::ggtitle("Local Feature Contributions"))
  }

  order_suffix <- function(k) {
    if (k %% 10 == 1 && k %% 100 != 11) return("st")
    if (k %% 10 == 2 && k %% 100 != 12) return("nd")
    if (k %% 10 == 3 && k %% 100 != 13) return("rd")
    return("th")
  }
  order_labels_vec <- sapply(1:max_present_order, function(o) paste0(o, order_suffix(o), " order"))

  plot_df_agg$term_order_str <- factor(
    plot_df_agg$term_order_num,
    levels = 1:max_present_order,
    labels = order_labels_vec[1:max_present_order] # Ensure correct length of labels
  )

  # Variable names for x-axis
  p_model <- get_max_var_index_from_labels(poly_obj$labels)
  p_data <- if(nrow(plot_df_agg) > 0) max(plot_df_agg$variable_idx, na.rm = TRUE) else 0
  p <- max(c(0, p_model, p_data), na.rm = TRUE)
  if (p == 0 && nrow(plot_df_agg) > 0) { p <- max(plot_df_agg$variable_idx, na.rm = TRUE) }
  if (p == 0) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x=0.5, y=0.5, label="No variables found in contributions.") +
             ggplot2::theme_minimal(base_size = 10) + ggplot2::ggtitle("Local Feature Contributions"))
  }

  current_axis_var_labels <- character(p) # Full list of potential labels
  if (!is.null(variable_names)) {
    for (i in 1:p) {
      if (i <= length(variable_names)) {
        current_axis_var_labels[i] <- variable_names[i]
      } else {
        current_axis_var_labels[i] <- as.character(i) # Fallback
      }
    }
  } else {
    current_axis_var_labels <- as.character(1:p) # Default: "1", "2", ...
  }

  unique_var_indices_in_data <- sort(unique(plot_df_agg$variable_idx))
  plot_df_agg$variable_label <- factor(plot_df_agg$variable_idx,
                                       levels = unique_var_indices_in_data,
                                       labels = current_axis_var_labels[unique_var_indices_in_data])

  # --- Generate Plot ---
  final_plot <- ggplot2::ggplot(plot_df_agg,
                                ggplot2::aes(x = .data$variable_label,
                                             y = .data$contribution,
                                             fill = .data$term_order_str)) +
    ggplot2::geom_col(position = "stack", width = 0.7, na.rm = TRUE) + # na.rm for safety
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::scale_fill_brewer(palette = "Set1", name = "Term Order", drop = FALSE) +
    ggplot2::labs(title = "Local Feature Contributions",
                  x = "Feature",
                  y = "Contribution to Prediction") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "top", legend.direction = "horizontal",
                   plot.title = ggplot2::element_text(hjust = 0.5, size = 14))

  return(final_plot)
}

plot_waterfall_internal <- function(poly_obj,
                                   monomial_values_for_obs,
                                   variable_names = NULL,
                                   waterfall_n = 15) {
  waterfall_n <- .nn2poly_validate_optional_whole_number(
    waterfall_n,
    "waterfall_n",
    min = 0
  )

  if (!is.list(poly_obj) || is.null(poly_obj$labels))
    stop("'poly_obj' must be a valid nn2poly object with labels.", call. = FALSE)

  if (!is.numeric(monomial_values_for_obs) || !is.vector(monomial_values_for_obs))
    stop("'monomial_values_for_obs' must be a numeric vector.", call. = FALSE)

  if (length(poly_obj$labels) != length(monomial_values_for_obs))
    stop("Length of 'poly_obj$labels' and 'monomial_values_for_obs' must match.", call. = FALSE)

  intercept_idx <- which(sapply(poly_obj$labels, function(lab) length(lab) == 1 && lab[1] == 0))
  intercept_value <- 0
  if (length(intercept_idx) > 0)
    intercept_value <- monomial_values_for_obs[intercept_idx[1]]

  # Build a per-term table
  term_df <- data.frame(
    term_index = seq_along(poly_obj$labels),
    contribution = as.numeric(monomial_values_for_obs),
    stringsAsFactors = FALSE
  )

  term_df$is_intercept <- FALSE
  if (length(intercept_idx) > 0)
    term_df$is_intercept[intercept_idx[1]] <- TRUE

  term_labels <- lapply(poly_obj$labels, function(lab) lab)
  term_df$term_label_str <- vapply(
    term_labels,
    function(lab) {
      if (length(lab) == 1 && lab[1] == 0)
        return("(Intercept)")
      format_term_label_display(
        lab,
        variable_names = variable_names,
        use_product_format_for_named = TRUE,
        use_product_format_for_numeric = TRUE
      )
    },
    character(1)
  )

  # Order terms (excluding intercept) by absolute contribution
  other_terms <- term_df[!term_df$is_intercept, , drop = FALSE]
  other_terms <- other_terms[order(abs(other_terms$contribution), decreasing = TRUE), , drop = FALSE]

  if (is.null(waterfall_n)) {
    waterfall_n <- nrow(other_terms)
  }

  kept_terms <- utils::head(other_terms, waterfall_n)
  dropped_terms <- if (nrow(other_terms) > nrow(kept_terms)) other_terms[(nrow(kept_terms) + 1):nrow(other_terms), , drop = FALSE] else NULL

  steps <- list(
    data.frame(term_label_str = "(Intercept)", contribution = intercept_value, stringsAsFactors = FALSE)
  )

  if (nrow(kept_terms) > 0)
    steps[[length(steps) + 1]] <- kept_terms[, c("term_label_str", "contribution"), drop = FALSE]

  if (!is.null(dropped_terms) && nrow(dropped_terms) > 0) {
    steps[[length(steps) + 1]] <- data.frame(
      term_label_str = "(Other)",
      contribution = sum(dropped_terms$contribution),
      stringsAsFactors = FALSE
    )
  }

  plot_df <- do.call(rbind, steps)

  plot_df$step_id <- seq_len(nrow(plot_df))
  plot_df$start <- c(0, utils::head(cumsum(plot_df$contribution), -1))
  plot_df$end <- cumsum(plot_df$contribution)
  plot_df$ymin <- pmin(plot_df$start, plot_df$end)
  plot_df$ymax <- pmax(plot_df$start, plot_df$end)
  plot_df$sign <- factor(sign(plot_df$contribution), levels = c(-1, 0, 1))

  plot_df$prev_end <- c(NA_real_, utils::head(plot_df$end, -1))

  pred_value <- utils::tail(plot_df$end, 1)
  pred_row <- data.frame(step_id = max(plot_df$step_id) + 1L, y = pred_value)

  p <- ggplot2::ggplot(plot_df) +
    ggplot2::geom_hline(yintercept = 0, color = "grey60") +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$step_id - 0.4,
        xmax = .data$step_id + 0.4,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$sign
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = plot_df[!is.na(plot_df$prev_end), , drop = FALSE],
      ggplot2::aes(
        x = .data$step_id - 1 + 0.4,
        xend = .data$step_id - 0.4,
        y = .data$prev_end,
        yend = .data$prev_end
      ),
      inherit.aes = FALSE,
      color = "grey50"
    ) +
    ggplot2::geom_point(
      data = pred_row,
      ggplot2::aes(x = .data$step_id, y = .data$y),
      inherit.aes = FALSE,
      size = 2.2,
      color = "black"
    ) +
    ggplot2::scale_fill_manual(
      values = c("-1" = "#F8766D", "0" = "grey80", "1" = "#00BA38"),
      breaks = c("-1", "1", "0"),
      labels = c("-" , "+", "0"),
      name = "Sign"
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(plot_df$step_id, pred_row$step_id),
      labels = c(plot_df$term_label_str, "Prediction"),
      expand = ggplot2::expansion(add = c(0.2, 0.6))
    ) +
    ggplot2::labs(
      title = "Waterfall: term contributions",
      x = NULL,
      y = "Model output"
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "top",
      legend.direction = "horizontal",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14)
    )

  p
}

#' Internal function for plot.nn2poly beeswarm type.
#' @noRd
plot_beeswarm_internal <- function(poly_obj,
                                   all_monomial_values_for_output, # 2D matrix: observations x terms
                                   original_feature_data,        # Full matrix/data frame of original predictors
                                   variable_names = NULL,
                                   color_by_feature = 1,
                                   top_n_terms = NULL) {
  top_n_terms <- .nn2poly_validate_optional_whole_number(
    top_n_terms,
    "top_n_terms",
    min = 1
  )

  if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
    stop("Package 'ggbeeswarm' is required for the 'beeswarm' plot type.\n",
         "Please install it using: install.packages('ggbeeswarm')", call. = FALSE)
  }

  # --- Input Validation & Basic Setup ---
  if (!is.list(poly_obj) || is.null(poly_obj$labels)) {
    stop("'poly_obj' must be a valid nn2poly object with labels.")
  }
  if (!is.matrix(all_monomial_values_for_output) || ncol(all_monomial_values_for_output) != length(poly_obj$labels)) {
    stop("'all_monomial_values_for_output' must be a matrix with columns matching poly_obj$labels.")
  }
  num_obs_mono <- nrow(all_monomial_values_for_output)

  # Ensure original_feature_data is a matrix
  if (!is.matrix(original_feature_data)) {
    original_feature_data <- as.matrix(original_feature_data)
    if (!is.numeric(original_feature_data) && !is.logical(original_feature_data)) {
      stop("'original_feature_data' could not be coerced to a numeric/logical matrix.")
    }
  }
  num_obs_orig <- nrow(original_feature_data)
  num_features_orig <- ncol(original_feature_data)

  if (num_obs_mono != num_obs_orig) {
    stop("Number of observations in 'all_monomial_values_for_output' (", num_obs_mono,
         ") must match 'original_feature_data' (", num_obs_orig, ").")
  }

  term_indices_to_plot <- 1:length(poly_obj$labels)
  term_labels_raw <- poly_obj$labels

  # --- Exclude Intercept ---
  intercept_idx <- which(sapply(term_labels_raw, function(lab) length(lab) == 1 && lab[1] == 0))
  if (length(intercept_idx) > 0) {
    term_indices_to_plot <- setdiff(term_indices_to_plot, intercept_idx)
    if (length(term_indices_to_plot) == 0) {
      return(ggplot2::ggplot() +
               ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Only an intercept term found. No beeswarm plot to generate.") +
               ggplot2::theme_minimal(base_size = 10) + ggplot2::ggtitle("Term Contribution Beeswarm Plot"))
    }
  }
  current_labels_raw_no_intercept <- term_labels_raw[term_indices_to_plot]
  current_monomial_values_no_intercept <- all_monomial_values_for_output[, term_indices_to_plot, drop = FALSE]

  if (ncol(current_monomial_values_no_intercept) == 0) {
    return(ggplot2::ggplot() + ggplot2::annotate("text",x=0.5,y=0.5,label="No non-intercept terms to plot.") +
             ggplot2::theme_minimal(base_size = 10) + ggplot2::ggtitle("Term Contribution Beeswarm Plot"))
  }

  # --- Term Importance and Selection ---
  mean_abs_term_contribution_vals_all <- colMeans(abs(current_monomial_values_no_intercept), na.rm = TRUE)

  if (!is.null(top_n_terms) && top_n_terms > 0 && top_n_terms < ncol(current_monomial_values_no_intercept)) {
    term_order_indices <- order(mean_abs_term_contribution_vals_all, decreasing = TRUE)
    selected_indices_in_no_intercept <- term_order_indices[1:top_n_terms]

    final_labels_to_plot <- current_labels_raw_no_intercept[selected_indices_in_no_intercept]
    final_monomial_values_to_plot <- current_monomial_values_no_intercept[, selected_indices_in_no_intercept, drop = FALSE]
    mean_abs_for_selected_terms <- mean_abs_term_contribution_vals_all[selected_indices_in_no_intercept]
  } else {
    final_labels_to_plot <- current_labels_raw_no_intercept
    final_monomial_values_to_plot <- current_monomial_values_no_intercept
    mean_abs_for_selected_terms <- mean_abs_term_contribution_vals_all
  }

  y_axis_order_indices <- order(mean_abs_for_selected_terms, decreasing = FALSE)

  # --- Prepare Data for Plotting (Long Format) ---
  plot_df_list <- list()
  term_strings_for_plot <- character(length(final_labels_to_plot))
  color_by_feature <- .nn2poly_validate_whole_number(
    color_by_feature,
    "color_by_feature",
    min = 1,
    max = num_features_orig
  )

  coloring_vector <- as.numeric(original_feature_data[, color_by_feature])

  for (j_idx in seq_along(final_labels_to_plot)) {
    term_lab_vector <- final_labels_to_plot[[j_idx]] # e.g. c(1), c(1,2)

    term_str <- format_term_label_display(
      term_lab_vector,
      variable_names = variable_names,
      use_product_format_for_named = TRUE,
      use_product_format_for_numeric = TRUE
    )
    term_strings_for_plot[j_idx] <- term_str

    plot_df_list[[j_idx]] <- data.frame(
      term_label_str = term_str,
      term_contribution = final_monomial_values_to_plot[, j_idx],
      coloring_value = coloring_vector,
      stringsAsFactors = FALSE
    )
  }

  plot_df_long <- do.call(rbind, plot_df_list)

  if (nrow(plot_df_long) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data to plot after filtering terms.") +
             ggplot2::theme_minimal(base_size = 10) + ggplot2::ggtitle("Term Contribution Beeswarm Plot"))
  }

  plot_df_long$term_label_str <- factor(plot_df_long$term_label_str,
                                        levels = term_strings_for_plot[y_axis_order_indices])

  # Legend title for coloring
  color_legend_title <- if (!is.null(variable_names) && color_by_feature <= length(variable_names)) {
    variable_names[color_by_feature]
  } else if (!is.null(colnames(original_feature_data)) && color_by_feature <= length(colnames(original_feature_data))) {
    colnames(original_feature_data)[color_by_feature]
  } else {
    paste0("Feature ", color_by_feature)
  }


  # --- Generate Plot ---
  beeswarm_plot <- ggplot2::ggplot(plot_df_long,
                                   ggplot2::aes(x = .data$term_contribution,
                                                y = .data$term_label_str,
                                                colour = .data$coloring_value)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    ggbeeswarm::geom_quasirandom(alpha = 1, size = 1.5, shape = 16, groupOnX = FALSE, na.rm = TRUE) +
    ggplot2::scale_colour_gradient2(low = "#F8766D", mid = "gray80", high = "#00BA38", midpoint = 0, name = color_legend_title, na.value = "grey70") +
    ggplot2::labs(title = "Term Contribution Beeswarm Plot",
                  x = "Term Contribution",
                  y = "Polynomial Term") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "right",
                   plot.title = ggplot2::element_text(hjust = 0.5, size = 14))

  return(beeswarm_plot)
}



#' Internal function for plot.nn2poly interaction_surface type
#' @noRd
plot_interaction_surface_internal <- function(poly_obj,
                                              feature_pair, # Vector of two indices or names
                                              original_feature_data, # For ranges and means
                                              grid_resolution = 20,
                                              variable_names = NULL,
                                              poly_output_index = 1) {
  grid_resolution <- .nn2poly_validate_whole_number(
    grid_resolution,
    "grid_resolution",
    min = 2
  )
  poly_output_index <- .nn2poly_validate_whole_number(
    poly_output_index,
    "poly_output_index",
    min = 1,
    max = ncol(poly_obj$values)
  )

  if (!is.matrix(original_feature_data))
    original_feature_data <- as.matrix(original_feature_data)

  if (!is.numeric(original_feature_data) && !is.logical(original_feature_data))
    stop("'original_feature_data' must be numeric or logical.", call. = FALSE)

  storage.mode(original_feature_data) <- "double"
  num_total_features <- ncol(original_feature_data)
  if (is.null(num_total_features) || num_total_features < 2)
    stop("'original_feature_data' must have at least two columns.", call. = FALSE)

  feat_idx1 <- NULL; feat_idx2 <- NULL
  feat_name1 <- ""; feat_name2 <- ""

  if (is.character(feature_pair) && length(feature_pair) == 2) {
    if (!is.null(variable_names)) {
      if (length(variable_names) < num_total_features)
        stop("'variable_names' must contain at least one name per feature.", call. = FALSE)
      current_var_names <- variable_names[seq_len(num_total_features)]
    } else if (!is.null(colnames(original_feature_data))) {
      current_var_names <- colnames(original_feature_data)
    } else {
      current_var_names <- paste0("x", seq_len(num_total_features))
    }

    feat_idx1 <- match(feature_pair[1], current_var_names)
    feat_idx2 <- match(feature_pair[2], current_var_names)
    if (is.na(feat_idx1) || is.na(feat_idx2)) stop("One or both features in 'feature_pair' not found.", call. = FALSE)
    feat_name1 <- feature_pair[1]
    feat_name2 <- feature_pair[2]
  } else if (is.numeric(feature_pair) && length(feature_pair) == 2) {
    feat_idx1 <- .nn2poly_validate_whole_number(feature_pair[1], "feature_pair[1]", min = 1, max = num_total_features)
    feat_idx2 <- .nn2poly_validate_whole_number(feature_pair[2], "feature_pair[2]", min = 1, max = num_total_features)
    feat_name1 <- if (!is.null(variable_names) && feat_idx1 <= length(variable_names)) variable_names[feat_idx1] else as.character(feat_idx1)
    feat_name2 <- if (!is.null(variable_names) && feat_idx2 <= length(variable_names)) variable_names[feat_idx2] else as.character(feat_idx2)
  } else {
    stop("'feature_pair' must be a character or numeric vector of length 2.", call. = FALSE)
  }

  if (feat_idx1 == feat_idx2) stop("Features in 'feature_pair' must be different.", call. = FALSE)

  range1 <- range(original_feature_data[, feat_idx1], na.rm = TRUE)
  range2 <- range(original_feature_data[, feat_idx2], na.rm = TRUE)
  if (!all(is.finite(range1)) || !all(is.finite(range2)))
    stop("Selected features must contain finite values to define the surface grid.", call. = FALSE)

  grid1 <- seq(range1[1], range1[2], length.out = grid_resolution)
  grid2 <- seq(range2[1], range2[2], length.out = grid_resolution)

  surface_data_df <- expand.grid(Feat1_Val_Plot = grid1, Feat2_Val_Plot = grid2)
  reference_values <- colMeans(original_feature_data, na.rm = TRUE)
  if (any(!is.finite(reference_values)))
    stop("'original_feature_data' must contain at least one finite value in every column.", call. = FALSE)

  # Prepare a version of poly_obj with only the selected output's coefficients
  poly_obj_single_output <- poly_obj
  poly_obj_single_output$values <- matrix(poly_obj$values[, poly_output_index], ncol = 1)

  grid_newdata <- matrix(
    rep(reference_values, each = nrow(surface_data_df)),
    nrow = nrow(surface_data_df),
    ncol = num_total_features
  )
  grid_newdata[, feat_idx1] <- surface_data_df$Feat1_Val_Plot
  grid_newdata[, feat_idx2] <- surface_data_df$Feat2_Val_Plot

  if(!is.null(colnames(original_feature_data))) {
    colnames(grid_newdata) <- colnames(original_feature_data)
  } else if (!is.null(variable_names) && length(variable_names) >= num_total_features) {
    colnames(grid_newdata) <- variable_names[seq_len(num_total_features)]
  }

  surface_data_df$Prediction <- as.numeric(eval_poly(poly_obj_single_output, grid_newdata))

  p <- ggplot2::ggplot(surface_data_df, ggplot2::aes(x = .data$Feat1_Val_Plot, y = .data$Feat2_Val_Plot, fill = .data$Prediction)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(name = "Prediction") +
    ggplot2::labs(title = paste("Two-feature response surface:", feat_name1, "&", feat_name2),
                  x = feat_name1, y = feat_name2) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
                   legend.position = "top",
                   legend.direction = "horizontal" ) +
    ggplot2::coord_equal()

  return(p)
}


#' Internal function for plot.nn2poly interaction_network type
#' @noRd
plot_interaction_network_internal <- function(poly_obj,
                                              interaction_order = 2, # Target order to display
                                              metric = "coefficient_abs", # "coefficient_abs", "mean_monomial_abs"
                                              newdata_monomials = NULL,
                                              top_n_interactions = NULL,
                                              variable_names = NULL,
                                              poly_output_index = 1,
                                              layout = "circle") {
  interaction_order <- .nn2poly_validate_whole_number(
    interaction_order,
    "interaction_order_network",
    min = 2
  )
  poly_output_index <- .nn2poly_validate_whole_number(
    poly_output_index,
    "poly_output_index",
    min = 1,
    max = ncol(poly_obj$values)
  )
  top_n_interactions <- .nn2poly_validate_optional_whole_number(
    top_n_interactions,
    "top_n_interactions",
    min = 1
  )
  metric <- .nn2poly_validate_character_scalar(metric, "metric_network")
  metric <- match.arg(metric, c("coefficient_abs", "mean_monomial_abs"))
  layout <- .nn2poly_validate_character_scalar(layout, "layout_network")
  layout <- match.arg(layout, c("circle", "line"))

  if (metric == "mean_monomial_abs" && is.null(newdata_monomials)) {
    stop("If metric is 'mean_monomial_abs', 'newdata_monomials' must be provided.", call. = FALSE)
  }

  edges_collector <- list()

  # Use coefficients from the selected output polynomial
  current_poly_coeffs <- poly_obj$values[, poly_output_index, drop = TRUE]

  # Prepare monomial slice if needed
  monomial_slice_for_metric <- NULL
  if (metric == "mean_monomial_abs") {
    monomial_slice_for_metric <- .nn2poly_slice_monomials_for_output(
      newdata_monomials,
      poly_output_index = poly_output_index
    )
    if (ncol(monomial_slice_for_metric) != length(poly_obj$labels))
      stop("Term count mismatch between 'newdata_monomials' and the nn2poly object labels.", call. = FALSE)
  }


  for (i_term in seq_along(poly_obj$labels)) {
    term_label <- poly_obj$labels[[i_term]]
    vars_in_label <- unique(term_label[term_label > 0]) # Unique positive variables in this term

    # Effective order is the count of unique variables. This treats x1^2*x2
    # as a pairwise interaction between x1 and x2.
    effective_interaction_order <- length(vars_in_label)

    if (effective_interaction_order == interaction_order && interaction_order >= 2) {
      term_coeff_val <- current_poly_coeffs[i_term]
      term_metric_val <- NA

      if (metric == "coefficient_abs") {
        term_metric_val <- abs(term_coeff_val)
      } else if (metric == "mean_monomial_abs") {
        term_metric_val <- mean(abs(monomial_slice_for_metric[, i_term]), na.rm = TRUE)
      }

      if (!is.na(term_metric_val) && term_metric_val > 1e-9) {
        if (length(vars_in_label) >= 2) {
          pairs <- utils::combn(vars_in_label, 2, simplify = FALSE)
          pair_weight <- term_metric_val / length(pairs)
          pair_signed_weight <- sign(term_coeff_val) * pair_weight

          for (p in pairs) {
            edges_collector[[length(edges_collector) + 1]] <- data.frame(
              from = min(p),
              to = max(p),
              strength = pair_weight,
              signed_strength = pair_signed_weight,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  if (length(edges_collector) == 0) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x=0.5,y=0.5,label=paste0("No ", interaction_order, "-order interactions found or all have zero weight.")) + ggplot2::theme_minimal())
  }

  edges_df <- do.call(rbind, edges_collector)

  edges_df_agg <- stats::aggregate(
    cbind(strength, signed_strength) ~ from + to,
    data = edges_df,
    FUN = sum
  )
  rownames(edges_df_agg) <- NULL
  edges_df_agg$sign <- sign(edges_df_agg$signed_strength)

  if (!is.null(top_n_interactions) && top_n_interactions > 0 && nrow(edges_df_agg) > top_n_interactions) {
    edges_df_agg <- edges_df_agg[order(edges_df_agg$strength, decreasing = TRUE), ][1:top_n_interactions, ]
  }

  if (nrow(edges_df_agg) == 0) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x=0.5,y=0.5,label="No interactions left after filtering.") + ggplot2::theme_minimal())
  }

  all_nodes_involved_final <- unique(c(edges_df_agg$from, edges_df_agg$to))
  if (length(all_nodes_involved_final) == 0) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x=0.5,y=0.5,label="No nodes to plot.") + ggplot2::theme_minimal())
  }

  node_df <- data.frame(id = sort(all_nodes_involved_final))
  node_df$label <- sapply(node_df$id, function(id_val) {
    if (!is.null(variable_names) && id_val > 0 && id_val <= length(variable_names)) {
      variable_names[id_val]
    } else {
      as.character(id_val) # Default to numeric if no name
    }
  })

  if (layout == "circle") {
    angles <- seq(pi / 2, pi / 2 - 2 * pi, length.out = nrow(node_df) + 1)
    angles <- angles[-length(angles)]
    node_df$x <- cos(angles)
    node_df$y <- sin(angles)
  } else {
    node_df$x <- seq_len(nrow(node_df))
    node_df$y <- 0
  }

  node_df$node_strength <- vapply(
    node_df$id,
    function(id_val) {
      sum(edges_df_agg$strength[edges_df_agg$from == id_val | edges_df_agg$to == id_val])
    },
    numeric(1)
  )

  edges_plot <- merge(
    edges_df_agg,
    node_df[, c("id", "x", "y")],
    by.x = "from",
    by.y = "id",
    all.x = TRUE
  )
  names(edges_plot)[names(edges_plot) == "x"] <- "x_from"
  names(edges_plot)[names(edges_plot) == "y"] <- "y_from"
  edges_plot <- merge(
    edges_plot,
    node_df[, c("id", "x", "y")],
    by.x = "to",
    by.y = "id",
    all.x = TRUE
  )
  names(edges_plot)[names(edges_plot) == "x"] <- "x_to"
  names(edges_plot)[names(edges_plot) == "y"] <- "y_to"
  edges_plot$sign_factor <- factor(edges_plot$sign, levels = c(-1, 0, 1))

  edge_geom <- if (layout == "circle") {
    ggplot2::geom_curve(
      data = edges_plot,
      ggplot2::aes(
        x = .data$x_from,
        y = .data$y_from,
        xend = .data$x_to,
        yend = .data$y_to,
        linewidth = .data$strength,
        colour = .data$sign_factor
      ),
      curvature = 0.18,
      alpha = 0.75,
      lineend = "round",
      inherit.aes = FALSE
    )
  } else {
    ggplot2::geom_segment(
      data = edges_plot,
      ggplot2::aes(
        x = .data$x_from,
        y = .data$y_from,
        xend = .data$x_to,
        yend = .data$y_to,
        linewidth = .data$strength,
        colour = .data$sign_factor
      ),
      alpha = 0.75,
      lineend = "round",
      inherit.aes = FALSE
    )
  }

  gg_plot <- ggplot2::ggplot() +
    edge_geom +
    ggplot2::geom_point(
      data = node_df,
      ggplot2::aes(x = .data$x, y = .data$y, size = .data$node_strength),
      shape = 21,
      fill = "#4C78A8",
      colour = "white",
      stroke = 0.8
    ) +
    ggplot2::geom_text(
      data = node_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      colour = "black",
      size = 3.5,
      vjust = -1.1
    ) +
    ggplot2::scale_linewidth_continuous(range = c(0.3, 2.5), name = "Strength") +
    ggplot2::scale_size_continuous(range = c(4, 8), name = "Node strength") +
    ggplot2::scale_colour_manual(
      values = c("-1" = "#F8766D", "0" = "grey70", "1" = "#00BA38"),
      labels = c("-1" = "-", "0" = "mixed", "1" = "+"),
      name = "Sign",
      drop = FALSE
    ) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
      plot.margin = ggplot2::margin(12, 24, 12, 24)
    ) +
    ggplot2::labs(title = paste(interaction_order, "-Feature Interaction Network", sep=""))

  return(gg_plot)
}
