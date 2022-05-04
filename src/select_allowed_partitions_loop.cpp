#include <Rcpp.h>
using namespace Rcpp;

List rm_null(List x) {
  int n = x.size();
  LogicalVector to_keep(n);
  for (int i = 0; i < n; i++) {
    to_keep[i] = !Rf_isNull(x[i]);
  }
  return x[to_keep];
}

// [[Rcpp::export]]
List select_allowed_partitions_loop(List all_partitions_for_this_label, int q_previous_layer){

  // number of partitions
  int n_partitions = all_partitions_for_this_label.size();

  // initialize list to store the output.
  List  output(n_partitions);

  for (int i = 0; i < n_partitions; i++){

    // Select one single partition
    List partition = all_partitions_for_this_label[i];

    // Check that the given partition has all elements allowed by q_previous_layer.
    // With sapply we sum the lengths of the vectors associated with each partition and see if
    // they are less or equal than q_previous_layer.
    if (is_true(all(sapply(partition, Rf_length) <= q_previous_layer))) {

     output.push_back(partition);
    }

  }


  output = rm_null(output);

  return output;

}


/*** R

library(nn2poly)

equivalent_label <- c(1,1,2)

q_previous_layer <- 2



# Generate partitions with Python script:
partitions <- generate_partitions(as.integer(3), as.integer(4))

labels <- vector(mode = "list", length = length(partitions))
# Obtain labels:
for (i in 1:length(partitions)){
  # Here it is used that the first partition of the multiset is always
  # the multiset itself. This could be generalized in case we change the
  # generation order. #REVISETHISLATER
  labels[[i]] <- partitions[[i]][[1]][[1]]
}

all_partitions <- list("labels" = labels, "partitions" = partitions)



# Obtain chosen label position from the partitions labels list:
position <- which(all_partitions$labels %in% list(equivalent_label))

# Extract the list with the partitions for the chosen label:
all_partitions_for_this_label <- all_partitions$partitions[[position]]

all_partitions_for_this_label

select_allowed_partitions_loop(
  all_partitions_for_this_label, q_previous_layer)


select_allowed_partitions(equivalent_label, q_previous_layer, all_partitions)

*/



