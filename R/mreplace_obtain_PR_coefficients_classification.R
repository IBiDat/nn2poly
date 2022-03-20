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
#' @param layer_index An index denoting which layer we are considering.
#' @param af_derivatives_list The derivatives of the activation functions (list)
#' @param q_taylor_vector List containing the degree up to which Taylor
#' expansion should be performed at each layer.
#' @param p The dimension p
#' @param all_partitions List of lists containing all the needed partitions.
#'
#' @return
#' @noRd
#'

mreplace_obtain_PR_coefficients_classification <- function(output_index,
                                                  coeffs_list_input,
                                                  layer_index,
                                                  af_derivatives_list,
                                                  q_taylor_vector,
                                                  p,
                                                  all_partitions){

  # Get the input vector of coeffs for that node.
  coeffs_input <- coeffs_list_input[[output_index]]


  # Extract the needed parameters and values:
  g <- af_derivatives_list[[layer_index]]
  q_layer <- q_taylor_vector[[layer_index]]
  q_previous_layer <- q_taylor_vector[[layer_index - 1]]
  # The total order of the regression will be the product of
  # all the q_taylor orders up to the current layer.+
  q_total_regression <- prod(q_taylor_vector[1:layer_index])


  # We define the vector that will contain all the coefficients
  # and their associated indexes (labels)
  coeffs_output <- c(0)
  labels_output <- c("0")

  ##### Intercept #####

  for (n in 0:q_layer) {
    coeffs_output[1] <- coeffs_output[1] + g[n + 1] * coeffs_input[1]^n
    # we have to use g[n+1] to obtain g^(n)/n!,
    # because the function taylor already includes the term 1/n!
  }

  ##### Rest of the coefficients #####

  # First loop from 1 to the maximum degree of the final expression
  for (t in 1:q_total_regression) {

    # generate the labels and indexes as vectors:
    labels_and_indexes <- generate_PR_labels_and_indexes(p, t)

    indexes <- labels_and_indexes$combinations_indexes
    labels_t <- labels_and_indexes$labels

    n_combinations <- nrow(indexes)

    # Now we create temporal betas for the given t that we will include in the final result vector
    coeffs_t <- rep(0, n_combinations)

    # Loop over all the number of possible combinations
    for (combination_index in 1:n_combinations) {

      # Obtain a vector containing the indexes, l_1,l_2,...,l_t in our notation:
      l_values <- indexes[combination_index, ]

      # Find the equivalence between l_values and a the ones needed for the
      # reduced partitions list
      table_l_values <- sort(table(l_values),decreasing=T)

      new<-1:length(table_l_values)
      old<-as.integer(names(table_l_values))
      equivalent_combination_indexes <- sort(c(new, l_values)[match(l_values, c(old, l_values))])

      # manipulate as strings to use mreplace:
      new_str <- as.character(new)
      old_str <- as.character(old)
      names(new_str)<-old_str

      # Create the label as a string of the form "l_1 l_2 ... l_t"
      equivalent_combination_label <- paste(as.character(equivalent_combination_indexes), collapse = ",")

      # Obtain all allowed partitions of the equivalent term
      partition_list <-
        select_allowed_partitions(
          coeff_label = equivalent_combination_label,
          q_previous_layer = q_previous_layer,
          all_partitions = all_partitions
        )

      # Number of partitions
      n_partition_list <- length(partition_list)


      # Replace again all the partitions to match the original indexes
      for (partition_index in 1:n_partition_list){
        aux <- partition_list[[partition_index]]
        for (i in 1:length(aux)){
          partition_list[[partition_index]][i] <- mreplace(aux[i],new_str)
        }
      }

      # Now, use the correctly renamed partitions
      for (n in 1:q_layer) {
        aux <- 0

        for (partition_index in 1:n_partition_list) {

          # Extract the chosen partition (a vector) from the partition list
          partition <- partition_list[[partition_index]]

          # Obtain the number of terms in the partition
          n_terms_in_partition <- length(partition)

          # We now need to check that each partition does not exceed n elements
          # so we have the condition m_0 + ... + m_C = n satisfied.
          # We also need the difference between the n_terms_in_partition
          # with respect to n, so we can add that difference as the exponent
          # of the intercep term. Then we compute this diff:
          difference <- n - n_terms_in_partition

          # If this diff is <0, we skip the partition
          if (difference >= 0) {

            # We need to obtain the m_index values to compute the multinomial coefficient
            # This is simply counting how many times each term appears, which can be done
            # using the table function. Then we need to add the difference term too.
            m <- unname(table(partition))
            m <- c(difference, m)
            multinomial_coef <- factorial(n) / prod(factorial(m))


            # It is sufficient to call coeffs_input[,partition]
            # to obtain a vector that contains all the needed coefficients
            # with their needed repetitions, ie, their exponent when
            # computing the product of them all.
            aux <- aux + multinomial_coef *
              prod(coeffs_input[, partition]) *
              coeffs_input[, "0"]^difference
          }

          # After the summatory over the partitions has been computed, we need to
          # get tis result "aux" and multiply by the correspondent derivative value:
          coeffs_t[combination_index] <- coeffs_t[combination_index] + g[n + 1] * aux
        }
      }
    }

    # Include all the values and the labels for each t in the final vector
    coeffs_output <- c(coeffs_output, coeffs_t)
    labels_output <- c(labels_output, labels_t)
  }

  # Finally set the coeffs vector as a row matrix and use the labels as the column names
  coeffs_output <- t(as.matrix(coeffs_output))
  colnames(coeffs_output) <- labels_output
  return(coeffs_output)
}
