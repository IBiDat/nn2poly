#' Function that implements the formula that obtains the PR coefficients
#' for a classification problem (multiple outputs) based on the regression solution.
#' However, this function implements the formula for a single node, the multiple outputs
#' are obtained in the loop inside nn2poly_algorithm.

#' Note that this step does not need the NN weights as they have been included already
#' in the previous layer coefficients.
#'
#' @param output_index An index denoting which of the neurons (or outputs) in
#' the layer we are computing.
#' @param coeffs_list_input The coefficients obtained in regression in the same
#' layer for the needed neuron/node.
#' @param current_layer An index denoting which layer we are considering.
#' @param af_derivatives_list The derivatives of the activation functions (list)
#' @param q_taylor_vector List containing the degree up to which Taylor
#' expansion should be performed at each layer.
#' @param p The dimension p
#' @param all_partitions List of lists containing all the needed partitions.
#'
#' @noRd
#'

alg_non_linear <- function(output_index,
                                                  coeffs_list_input,
                                                  labels_output,
                                                  current_layer,
                                                  af_derivatives_list,
                                                  q_taylor_vector,
                                                  p,
                                                  all_partitions) {

  # Get the input vector of coeffs for that node.
  # Recall that the +1 is needed because the first list element are the labels
  coeffs_labels <- coeffs_list_input[[1]]
  coeffs_input <- coeffs_list_input[[output_index+1]]

  # Extract the needed parameters and values:
  g <- af_derivatives_list[[current_layer]]
  q_layer <- q_taylor_vector[[current_layer]]
  if (current_layer == 1){
    q_previous_layer <- 1
  } else {
    q_previous_layer <- q_taylor_vector[[current_layer - 1]]
  }

  # The total order of the regression will be the product of
  # all the q_taylor orders up to the current layer
  q_total_current_layer <- prod(q_taylor_vector[1:current_layer])

  # Obtain total number of terms in the polynomial from labels
  n_poly_terms <- length(labels_output)

  # We define the vector that will contain all the coefficients
  coeffs_output <- rep(0, n_poly_terms)

  ##### Intercept #####

  for (n in 0:q_layer) {
    coeffs_output[1] <- coeffs_output[1] + g[n + 1] * coeffs_input[1]^n
    # we have to use g[n+1] to obtain g^(n)/n!,
    # because the function taylor already includes the term 1/n!
  }

  ##### Rest of the coefficients #####

  # As we already have all the coefficient labels, we can loop over them
  # Note that the intercept has to be skipped so start at 2
  for (coeff_index in 2:n_poly_terms){
    label <- labels_output[[coeff_index]]

    # Extract the order of the current coefficient (elements in label)
    #t <- length(label)

    # Find the equivalence between label and a the ones needed for the
    # reduced partitions list
    table_label <- sort(table(label),decreasing=T)

    new<-1:length(table_label)
    old<-as.integer(names(table_label))
    equivalent_label <- sort(c(new, label)[match(label, c(old, label))])

    # Obtain all allowed partitions of the equivalent term
    allowed_partitions <-
      select_allowed_partitions(
        equivalent_label = equivalent_label,
        q_previous_layer = q_previous_layer,
        all_partitions = all_partitions
      )

    # Number of partitions
    n_allowed_partitions <- length(allowed_partitions)

    # Replace again all the partitions to match the original indexes
    for (partition_index in 1:n_allowed_partitions){
      aux <- allowed_partitions[[partition_index]]
      for (i in 1:length(aux)){
        aux_vec <- aux[[i]]
        aux_vec_reverted_equivalence <- sort(c(old, aux_vec)[match(aux_vec, c(new, aux_vec))])
        allowed_partitions[[partition_index]][[i]] <- aux_vec_reverted_equivalence
      }
    }

    # Now, use the correctly renamed partitions
    for (n in 1:q_layer) {
      summatory <- 0

      for (partition_index in 1:n_allowed_partitions) {

        # Extract the chosen partition (a list) from the allowed partitions
        partition <- allowed_partitions[[partition_index]]

        # Obtain the number of terms in the partition
        n_terms_in_partition <- length(partition)

        # We now need to check that each partition does not exceed n elements
        # so we have the condition m_0 + ... + m_C = n satisfied.
        # We also need the difference between the n_terms_in_partition
        # with respect to n, so we can add that difference as the exponent
        # of the intercep term. Then we compute this diff:
        difference <- n - n_terms_in_partition

        # If this diff is <0, we skip the partition
        # This is due to the second restriction to the allowed partitions, that
        # depends on n
        if (difference >= 0) {

          # We need to obtain the m_index values to compute the multinomial
          # coefficient

          # This is simply counting how many times each unique term appears,
          # obtaining the factorials and then doing the product. The terms that
          # do not appear dont need to be counted as they will be 0, their
          # factorial 1 and at the end will, not affect the total product.

          # This can be done as follows. #REVISETHISLATER when this used
          # string vectors, it was easier to count all with table().
          # Now with vectors of different lengths in the vector this no
          # longer works and a not so efficient workaround is used

          unique_in_partition <- unique(partition)
          m <- rep(0,length(unique_in_partition))
          for (i in 1:length(unique_in_partition)){
            m[i] <- sum(partition %in% list(unique_in_partition[[i]]))
          }

          # Add the difference between n and number of terms
          m <- c(difference, m)

          # Compute the multinomial coefficient
          multinomial_coef <- factorial(n) / prod(factorial(m))

          # Now we need to use the labels to get the needed coefficients:
          coeffs_input_needed <- rep(0,n_terms_in_partition)

          for (i in 1:n_terms_in_partition){
            coeffs_input_needed[i] <- coeffs_input[which(coeffs_labels %in% list(partition[[i]]))]
          }

          # Finally compute the product of coefficients according to multinomial
          # theorem and add it to the summatory
          # For the product, it is sufficient to call prod(coeffs_input_needed)
          # without including the exponent m, as this vector will contain
          # each coefficient as many times as its exponent would indicate.
          summatory <- summatory + multinomial_coef *
            prod(coeffs_input_needed) *
            coeffs_input[1]^difference
            # Note that coeffs_input[1] is the intercept
        }
      }
      # After the summatory over the partitions has been computed, we need to
      # get its result and multiply by the correspondent derivative value, and
      # add to the already stored values, here we are computing the summatory
      # over n.
      coeffs_output[coeff_index] <- coeffs_output[coeff_index] + g[n + 1] * summatory
    }
  }
  return(coeffs_output)
}
