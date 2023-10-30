// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "utils.h"
using namespace Rcpp;


// [[Rcpp::export]]
std::vector<ListOf<IntegerVector>> select_allowed_partitions(
    IntegerVector equivalent_label, int q_previous_layer,
    ListOf<IntegerVector> labels, List partitions)
{
  //REVISETHISLATER This function could be omitted if we already include it when
  // generating the partitions.

  // Obtain chosen label position from the partitions labels list:
  int pos; for (pos = 0; pos < labels.size(); pos++) {
    if (labels[pos].size() != equivalent_label.size())
      continue;
    if (is_true(all(labels[pos] == equivalent_label)))
      break;
  }

  // Extract the list with the partitions for the chosen label:
  ListOf<ListOf<IntegerVector>> all_partitions_for_this_label =
    as<ListOf<ListOf<IntegerVector>>>(partitions[pos]);

  // number of partitions
  int n_partitions = all_partitions_for_this_label.size();

  // initialize list to store the output.
  std::vector<ListOf<IntegerVector>> output;

  for (int i = 0; i < n_partitions; i++) {
    // Select one single partition
    ListOf<IntegerVector> partition =
      as<ListOf<IntegerVector>>(all_partitions_for_this_label[i]);

    // Check that the given partition has all elements allowed by q_previous_layer.
    // With sapply we sum the lengths of the vectors associated with each partition and see if
    // they are less or equal than q_previous_layer.
    if (is_true(all(sapply(partition, Rf_length) <= q_previous_layer)))
      output.push_back(clone(partition));
  }

  return output;
}



// [[Rcpp::export]]
arma::mat alg_non_linear(arma::mat coeffs_input,
                             ListOf<IntegerVector> labels_input,
                             ListOf<IntegerVector> labels_output,
                             IntegerVector taylor_orders,
                             int current_layer, arma::vec g,
                             ListOf<IntegerVector> partitions_labels, List partitions)
{
  // Extract the needed parameters and values:
  int q_layer = taylor_orders[current_layer - 1];
  int q_previous_layer = 1;
  if (current_layer != 1)
    q_previous_layer = taylor_orders[current_layer - 2];

  // Obtain total number of terms in the polynomial from labels
  int n_poly_terms = labels_output.size();

  // Obtain number of neurons
  int h_l = coeffs_input.n_rows;

  // We define the vector that will contain all the output coefficients
  arma::mat coeffs_output(h_l,n_poly_terms);

  ////////// Intercept //////////

  for (int n = 0; n <= q_layer; n++) {
    coeffs_output.col(0) = coeffs_output.col(0) + g[n] * arma::pow(coeffs_input.col(0), n);
    // we have to use g[n] to obtain g^(n)/n!,
    // because the function taylor already includes the term 1/n!
  }

  ////////// Rest of the coefficients //////////

  // As we already have all the coefficient labels, we can loop over them
  // Note that the intercept has to be skipped so start at 1
  for (int coeff_index = 1; coeff_index < n_poly_terms; coeff_index++) {
    IntegerVector label = labels_output[coeff_index];

    // Find the equivalence between label and a the ones needed for the
    // reduced partitions list
    std::multiset<int> mset(label.begin(), label.end());
    IntegerVector comp = unique(label).sort();
    IntegerVector mult(comp.size());
    for (int i = 0; i < comp.size(); i++)
      mult[i] = mset.count(comp[i]);
    comp = comp[order(mult, true)]; //decreasing
    IntegerVector seq = Range(1, comp.size());
    // IntegerVector equivalent_label =
    //   concat(seq, label)[match(label, concat(comp, label)) - 1];
    IntegerVector equivalent_label = match(label, comp);
    equivalent_label.sort();

    // Obtain all allowed partitions of the equivalent term
    auto allowed_partitions = select_allowed_partitions(
      equivalent_label, q_previous_layer, partitions_labels, partitions);

    // Number of partitions
    int n_allowed_partitions = allowed_partitions.size();

    // Replace again all the partitions to match the original indexes
    for (int p_index = 0; p_index < n_allowed_partitions; p_index++) {
      ListOf<IntegerVector> aux = allowed_partitions[p_index];
      for (int i = 0; i < aux.size(); i++) {
        IntegerVector auxv = aux[i];
        aux[i] = concat(comp, auxv)[match(auxv, concat(seq, auxv)) - 1];
        aux[i].sort();
      }
    }

    // Now, use the correctly renamed partitions
    for (int n = 1; n <= q_layer; n++) {

      arma::vec summatory(h_l);

      for (int p_index = 0; p_index < n_allowed_partitions; p_index++) {
        // Extract the chosen partition (a list) from the allowed partitions
        ListOf<IntegerVector> partition = allowed_partitions[p_index];

        // We now need to check that each partition does not exceed n elements
        // so we have the condition m_0 + ... + m_C = n satisfied.
        // We also need the difference between the n_terms_in_partition
        // with respect to n, so we can add that difference as the exponent
        // of the intercept term. Then we compute this diff:
        int difference = n - partition.size();

        // If this diff is <0, we skip the partition
        // This is due to the second restriction to the allowed partitions, that
        // depends on n
        if (difference < 0) continue;

        // We need to obtain the m_index values to compute the multinomial
        // coefficient

        // This is simply counting how many times each unique term appears,
        // obtaining the factorials and then doing the product. The terms that
        // do not appear dont need to be counted as they will be 0, their
        // factorial 1 and at the end will, not affect the total product.

        // This can be done as follows. //REVISETHISLATER when this used
        // string vectors, it was easier to count all with table().
        // Now with vectors of different lengths in the vector this no
        // longer works and a not so efficient workaround is used
        ListOf<IntegerVector> unique_in_partition = Function("unique")(partition);
        NumericVector m(unique_in_partition.size() + 1);
        for (int i = 0; i < unique_in_partition.size(); i++)
          m[i + 1] = sum(as<IntegerVector>(Function("%in%")(
            partition, List::create(unique_in_partition[i]))));
        m[0] = difference;

        // Compute the multinomial coefficient
        NumericVector fm = factorial(m);
        double multinomial_coef = std::tgamma(n + 1) / prod(fm);


        // Now we need to use the labels to get the needed coefficients:
        LogicalVector needed = Function("%in%")(labels_input, partition);
        arma::mat coeffs_input_needed = coeffs_input.cols(find(as<arma::vec>(needed) == 1));
        for (unsigned int i = 0; i < coeffs_input_needed.n_cols; i++)
          coeffs_input_needed.col(i) = arma::pow(coeffs_input_needed.col(i), m[i + 1]);


        // Finally compute the product of coefficients according to multinomial
        // theorem and add it to the summatory
        // For the product, it is sufficient to call prod(coeffs_input_needed)
        // without including the exponent m, as this vector will contain
        // each coefficient as many times as its exponent would indicate.
        // REVISETHISLATER esto debería poder hacerse sin bucle con row product
        summatory += multinomial_coef *
          arma::prod(coeffs_input_needed,1) % arma::pow(coeffs_input.col(0), difference);
        // Note that coeffs_input[0] is the intercept


      }
      // After the summatory over the partitions has been computed, we need to
      // get its result and multiply by the correspondent derivative value, and
      // add to the already stored values, here we are computing the summatory
      // over n.
      coeffs_output.col(coeff_index) =  coeffs_output.col(coeff_index) +  g[n] * summatory;
    }
  }
  return coeffs_output;
}
