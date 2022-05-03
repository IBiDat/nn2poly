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

  #REVISETHISLATER This function could be ommitted if we already include it when
  # generating the partitions.

  # Obtain chosen label position from the partitions labels list:
  position <- which(all_partitions$labels %in% list(equivalent_label))

  # Extract the list with the partitions for the chosen label:
  all_partitions_for_this_label <- all_partitions$partitions[[position]]


  # number of partitions
  n_partitions <- length(all_partitions_for_this_label)

  # initialize list to store the output.
  output <- vector(mode = "list", length = n_partitions)

  for (i in 1:n_partitions){
    # Select one single partition [[1]] corresponds to the partition list:
    partition <- all_partitions_for_this_label[[i]]


    # First of all, check that the given partition has all elements allowed by q_previous_layer.
    # With lapply we sum the vectors associated with each partition and see if
    # they are less or equal than q_previous_layer. Then use all, so if at least one does not meet
    # the condition, then the result is false.
    if( all(lapply(partition,length) <=q_previous_layer)){

      output[[i]] <- partition

    }

  }

  # Finally, remove the NULL elements
  null_indicator <- sapply(output, is.null)
  if( sum(null_indicator) > 0){
    output = output[-which(null_indicator)]
  }

  return(output)
}
