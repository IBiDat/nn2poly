#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
String concat(IntegerVector my_vector) {

  std::stringstream result;
  std::copy(my_vector.begin(), my_vector.end(), std::ostream_iterator<int>(result, ","));

  std::string output = result.str().c_str();

  output.erase(std::find_if(output.rbegin(), output.rend(), std::bind1st(std::not_equal_to<char>(), ',')).base(), output.end());


  return output;
}

// [[Rcpp::export]]
List rm_null(List x) {
  int n = x.size();
  LogicalVector to_keep(n);
  for (int i = 0; i < n; i++) {
    to_keep[i] = !Rf_isNull(x[i]);
  }
  return x[to_keep];
}

//' Select the allowed partitions at a given layer computation
//'
//' @param coeff_label PR coefficient label
//' @param q_previous_layer Taylor expansion degree at the previous layer.
//' @param all_partitions list of lists of partitions
//'
//' @return list
//'
//'
// [[Rcpp::export]]
List select_allowed_partitions(std::string coeff_label, double q_previous_layer, List all_partitions) {

  // Extract the list with the partitions for the chosen label:
  ListOf<List>  all_partitions_for_this_label = as<ListOf<List>>(all_partitions[coeff_label]);

  // number of partitions
  int n_partitions = all_partitions_for_this_label.size();

  // initialize list to store the output.
  List output(n_partitions);

  for (int i = 0; i < n_partitions; i++){

    // Select one single partition
    ListOf<IntegerVector> partition = all_partitions_for_this_label[i];

    // First of all, check that the given partition has all elements allowed by q_previous_layer.
    // With lapply we sum the vectors associated with each partition and see if
    // they are less or equal than q_previous_layer. Then do the product, if at least one does not meet
    // the condition, then the product is zero and condition is not met.
    int n_terms_in_partition = partition.size();


    LogicalVector vector_allowed_previous_layer(n_terms_in_partition);


    if (is_true(all(sapply(partition, Rf_length) <= q_previous_layer))) {
      // Dentro de este if, es el equivalente a dentro de
      // "if( all(lapply(partition,length) <=q_previous_layer))"


      // Initialize a vector that will store the labels
      // of the needed terms according to the partition
      StringVector partition_labels(n_terms_in_partition);

      // Loop over the terms in the chosen partition
      for (int k = 0; k < n_terms_in_partition; k++){
        // Change the vector to string and collapse it to obtain the label
        IntegerVector aux4 = partition[k];
        String aux5 = concat(aux4);

        partition_labels[k] = aux5;

      }
      output[i] = partition_labels;
    }

  }


  output = rm_null(output);

  return output;
}





