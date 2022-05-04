#' Title
#'
#' @param label a
#' @param q_previous_layer a
#' @param all_partitions a
#'
#' @return
#' @export
#'
#' @examples
#'
#'
select_allowed_partitions <- function(equivalent_label, q_previous_layer, all_partitions){

  #REVISETHISLATER This function could be omitted if we already include it when
  # generating the partitions.

  # Obtain chosen label position from the partitions labels list:
  position <- which(all_partitions$labels %in% list(equivalent_label))

  # Extract the list with the partitions for the chosen label:
  all_partitions_for_this_label <- all_partitions$partitions[[position]]

  output <- select_allowed_partitions_loop(all_partitions_for_this_label, q_previous_layer)

  return(output)
}
