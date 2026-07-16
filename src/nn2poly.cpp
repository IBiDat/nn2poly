#include "partitions.h"
#include "multiset.h"
#include "taylor.h"
using namespace nn2poly::linalg;

// [[Rcpp::export]]
Term obtain_taylor_vector(const Term& taylor_orders,
                          const Functions& af_string_list) {
  if (taylor_orders.size() == 1) {
    // Single value case, set 1 in linear, taylor_orders in other AF
    Term out(af_string_list.size());
    const int base = taylor_orders[0];
    for (size_t i = 0; i < af_string_list.size(); i++)
      out[i] = (af_string_list[i] == "linear") ? 1 : base;
    return out;
  }

  if (taylor_orders.size() != af_string_list.size())
    throw std::invalid_argument(
      "`taylor_orders` length does not match provided number of layers");

  return taylor_orders;
}

// [[Rcpp::export]]
CoeffsList obtain_derivatives_list(const Term& taylor_orders,
                                   const Functions& af_string_list) {
  if (taylor_orders.size() != af_string_list.size())
    throw std::invalid_argument(
      "`taylor_orders` length does not match provided number of layers");

  CoeffsList out(af_string_list.size());
  for (size_t i = 0; i < af_string_list.size(); i++) {
    if (taylor_orders[i] < 0)
      throw std::invalid_argument("`taylor_orders` must be non-negative");
    // Obtain the vector with the derivatives of the activation function up to
    // the given degree centered at 0
    out[i] = coeffs_taylor(af_string_list[i], taylor_orders[i]);
  }

  return out;
}

Weights alg_non_linear_impl(const Weights& coeffs_input,
                            const TermMap& labels_input_map,
                            const Terms& labels_output,
                            const int previous_order, const int q_layer,
                            const Coeffs& g, PartitionCache& pcache) {
  // Number of terms, number of neurons h_l, output matrix
  const int n_poly_terms = static_cast<int>(labels_output.size());
  const int h_l = n_rows(coeffs_input);
  Weights coeffs_output = zeros(h_l, n_poly_terms);

  ////////// Intercept //////////

  add_poly_eval(coeffs_output, 0, coeffs_input, g, q_layer);

  ////////// Rest of the coefficients //////////

  // As we already have all the coefficient labels, we can loop over them
  // Note that the intercept has to be skipped so start at 1
  for (int coeff_index = 1; coeff_index < n_poly_terms; coeff_index++) {
    const PartitionCounts& pcounts = build_partition_counts(
      labels_output[coeff_index], previous_order, labels_input_map, pcache);

    // Now, use the correctly renamed partitions
    for (int n = 1; n <= q_layer; n++) {
      if (g[n] == 0) continue;
      auto summatory = zeros(h_l);

      for (const auto& pcount : pcounts) {
        // We now need to check that each partition does not exceed n elements
        // so we have the condition m_0 + ... + m_C = n satisfied.
        // We also need the difference between the n_terms_in_partition
        // with respect to n, so we can add that difference as the exponent
        // of the intercept term. Then we compute this diff:
        const int diff = n - static_cast<int>(pcount.size);

        // If this diff is < 0, we skip the partition. This is due to the
        // second restriction to the allowed partitions, that depends on n
        if (diff < 0)
          continue;

        NN2POLY_DEBUG_LOG(5, DTAG(n), DTAG(diff), DTAG(g[n]), DTAG(pcount));

        // Finally compute the product of coefficients according to multinomial
        // theorem and add it to the summatory
        double m_coef = factorials[n] / factorials[diff];
        for (int m : pcount.counts) m_coef /= factorials[m];

        summatory += m_coef * accumulate_partition(
          coeffs_input, diff, pcount.idx, pcount.counts);
      }
      // After the summatory over the partitions has been computed, we need to
      // get its result and multiply by the correspondent derivative value, and
      // add to the already stored values, here we are computing the summatory
      // over n.
      add_partition(coeffs_output, coeff_index, g[n], summatory);
    }
    CHECK_INTERRUPT();
  }

  return coeffs_output;
}

// [[Rcpp::export]]
Weights alg_non_linear(const Weights& coeffs_input,
                       const Terms& labels_input,
                       const Terms& labels_output,
                       const int previous_order, const int q_layer,
                       const Coeffs& g) {
  PartitionCache pcache;
  TermMap labels_map;
  for (size_t i = 0; i < labels_input.size(); i++)
    labels_map[labels_input[i]] = i;
  return alg_non_linear_impl(
    coeffs_input,
    labels_map,
    labels_output,
    previous_order,
    q_layer,
    g, pcache
  );
}

inline void check_weights_dimensions(const Layers& layers) {
  for (size_t i = 1; i < layers.size(); i++) {
    if (n_rows(layers[i]) != n_cols(layers[i - 1]) + 1)
      throw std::invalid_argument(
        "the list of weights has incorrect dimensions, "
        "please check the right dimmensions in the documentation");
  }
}

// [[Rcpp::export]]
List nn2poly_algorithm(const Layers& layers, const Functions& af_list,
                       int max_order, bool keep_layers,
                       const Term& taylor_orders) {
  if (layers.empty())
    throw std::invalid_argument("`layers` is empty");
  if (af_list.empty())
    throw std::invalid_argument("activation functions are missing in `af_list`");
  if (layers.size() != af_list.size())
    throw std::invalid_argument("`layers` and `af_list` must have the same length");

  check_weights_dimensions(layers);

  // Dimension p, layers L (L-1 hidden + 1 output, input is denoted by 0)
  const int p = static_cast<int>(n_rows(layers[0])) - 1;
  const int L = static_cast<int>(af_list.size());
  const bool last_linear = (af_list.back() == "linear");

  // The list with the results of coefficients at each layer,
  // depending on the last layer being linear or not
  WeightsLists results(static_cast<size_t>(last_linear ? 2 * L - 1 : 2 * L));
  const Term taylor = obtain_taylor_vector(taylor_orders, af_list);
  const CoeffsList af_dlist = obtain_derivatives_list(taylor, af_list);

  // Starting point for the algorithm: Set weights as coefficients
  // of an order 1 polynomial.

  // The labels for each coefficient vector at the same layer and linear or
  // same layer and non linear case will have be the same, so they can be
  // stored only once as the element `labels` of the coeffs_list,
  // so we define length h+1

  // For each neuron in the first hidden layer, when computing the activation
  // potentials (u_j), each column of the weight matrix represents the
  // coefficients of an order 1 polynomial for that neuron potential.
  // The first element will be the bias, and the rest the coefficient
  // associated with each variable from x_1 to x_p.

  PartitionCache pcache;
  TermMap labels_map;
  Weights coeffs_list;
  Terms labels_list;
  labels_list.reserve(p + 1);
  for (int i = 0; i <= p; i++)
    labels_list.push_back(Term{ i });
  int new_order = 1;

  ////////////////// Loop over all layers ///////////////////
  // Note that the loop will iterate the current layer from 1 to L
  // and compute the linear and then non linear situation.

  for (int l = 1; l <= L; l++) {

    ////////// Linear case //////////
    // Apply the weights for the first layer, or linear algorithm
    coeffs_list = (l == 1) ? trans(layers[0])
      : alg_linear(coeffs_list, layers[l - 1]);

    // Save results and check if finished
    results[2 * l - 2] = WeightsList{labels_list, coeffs_list};
    if (l == L && last_linear)
      goto out;

    ////////// Non linear case //////////
    // The output dimension remains the same as in the previous linear case,
    // because the same number of neurons is considered

    // In the non linear case the polynomial order increases (unless max_order
    // is reached), so the new labels need to be computed. However, the previous
    // ones can be reused.

    // The new labels will be for monomials of orders between the total order
    // of the previous polynomial and the total order of the new polynomial:
    int previous_order = new_order;
    int q_layer = taylor[l - 1];
    new_order = std::min(previous_order * q_layer, max_order);

    // Update map with new labels, if any
    for (size_t i = labels_map.size(); i < labels_list.size(); i++)
      labels_map[labels_list[i]] = i;

    // If the order has increased, create new labels
    if (previous_order != new_order)
      for (int order = previous_order + 1; order <= new_order; order++) {
        Terms comb = combinations_with_repetition(p, order);
        for (const Term& term : comb) labels_list.push_back(term);
      }

    NN2POLY_DEBUG_LOG(2, "[layer", l, "]",
      DTAG(previous_order), DTAG(q_layer), DTAG(labels_list.size()));
    // Apply non-linear algorithm
    coeffs_list = alg_non_linear_impl(
      coeffs_list,
      labels_map,
      labels_list,
      previous_order,
      q_layer,
      af_dlist[l - 1],
      pcache
    );
    NN2POLY_DEBUG_LOG(2, "[layer", l, "]", pcache.debug(true));

    // Save results and check if finished
    results[2 * l - 1] = WeightsList{labels_list, coeffs_list};
    if (l == L && !last_linear)
      goto out;
  }

out:
  NN2POLY_DEBUG_LOG(1, pcache.debug());
  if (keep_layers)
    return Rcpp::wrap(results);
  return Rcpp::wrap(results.back());
}
