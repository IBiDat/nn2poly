#include "partitions.h"
#include "multiset.h"
#include "taylor.h"

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
    stop("Argument `taylor_orders` length does not match provided number of layers");

  return taylor_orders;
}

// [[Rcpp::export]]
CoeffsList obtain_derivatives_list(const Term& taylor_orders,
                                   const Functions& af_string_list) {
  if (taylor_orders.size() != af_string_list.size())
    stop("Argument `taylor_orders` length does not match provided number of layers");

  CoeffsList out(af_string_list.size());
  for (size_t i = 0; i < af_string_list.size(); i++) {
    if (taylor_orders[i] < 0)
      stop("Argument `taylor_orders` must be non-negative");
    // Obtain the vector with the derivatives of the activation function up to
    // the given degree centered at 0
    out[i] = coeffs_taylor(af_string_list[i], taylor_orders[i]);
  }

  return out;
}

Weights alg_non_linear_impl(const Weights& coeffs_input,
                            const Terms& labels_input,
                            const Terms& labels_output,
                            const Term& taylor_orders,
                            int current_layer,
                            const Coeffs& g,
                            PartitionCache& pcache) {
  // Extract the needed parameters and values:
  const int q_layer = taylor_orders[current_layer - 1];
  int q_previous_layer = 1;
  if (current_layer != 1)
    q_previous_layer = taylor_orders[current_layer - 2];

  // Obtain total number of terms in the polynomial from labels
  const int n_poly_terms = static_cast<int>(labels_output.size());

  // Obtain number of neurons
  const int h_l = static_cast<int>(coeffs_input.n_rows);

  // We define the vector that will contain all the output coefficients
  Weights coeffs_output(h_l, n_poly_terms, arma::fill::zeros);

  ////////// Intercept //////////

  for (int n = 0; n <= q_layer; n++) {
    coeffs_output.col(0) = coeffs_output.col(0) + g[n] * arma::pow(coeffs_input.col(0), n);
    // we have to use g[n] to obtain g^(n)/n!,
    // because the function taylor already includes the term 1/n!
  }

  ////////// Rest of the coefficients //////////

  // Build a hash map for labels_input to optimize term positions lookup
  TermMap labels_input_map;
  labels_input_map.reserve(labels_input.size());
  for (size_t i = 0; i < labels_input.size(); i++) {
    labels_input_map[labels_input[i]] = i;
  }

  // As we already have all the coefficient labels, we can loop over them
  // Note that the intercept has to be skipped so start at 1
  for (int coeff_index = 1; coeff_index < n_poly_terms; coeff_index++) {
    const Term& label = labels_output[coeff_index];
    Partition allowed_terms = build_allowed_terms(label, q_previous_layer, pcache);

    // Now, use the correctly renamed partitions
    for (int n = 1; n <= q_layer; n++) {
      arma::vec summatory(h_l, arma::fill::zeros);

      for (const Terms& terms : allowed_terms) {
        // We now need to check that each partition does not exceed n elements
        // so we have the condition m_0 + ... + m_C = n satisfied.
        // We also need the difference between the n_terms_in_partition
        // with respect to n, so we can add that difference as the exponent
        // of the intercept term. Then we compute this diff:
        const int difference = n - static_cast<int>(terms.size());

        // If this diff is <0, we skip the partition
        // This is due to the second restriction to the allowed partitions, that
        // depends on n
        if (difference < 0)
          continue;

        // We need to obtain the m_index values to compute the multinomial
        // coefficient

        // This is simply counting how many times each unique term appears,
        // obtaining the factorials and then doing the product. The terms that
        // do not appear dont need to be counted as they will be 0, their
        // factorial 1 and at the end will, not affect the total product.
        TermSummary term_summary = summarize_terms(terms);
        Term mult(term_summary.unique_terms.size() + 1, 0);
        for (size_t i = 0; i < term_summary.unique_terms.size(); i++) {
          auto it = term_summary.counts.find(term_summary.unique_terms[i]);
          if (it == term_summary.counts.end())
            stop("Internal error while counting partition terms.");
          mult[i + 1] = it->second;
        }
        mult[0] = difference;

        // Compute the multinomial coefficient
        double multinomial_coef = std::tgamma(static_cast<double>(n) + 1.0);
        for (int m : mult)
          multinomial_coef /= std::tgamma(static_cast<double>(m) + 1.0);

        // Now we need to use the labels to get the needed coefficients:
        const std::vector<size_t> idx = in_terms_positions(labels_input_map, term_summary);
        Weights coeffs_input_needed(h_l, idx.size());
        if (!idx.empty()) {
          coeffs_input_needed = coeffs_input.cols(to_arma_indices(idx));
          for (unsigned int i = 0; i < coeffs_input_needed.n_cols; i++)
            coeffs_input_needed.col(i) = arma::pow(coeffs_input_needed.col(i), mult[i + 1]);
        }

        // Finally compute the product of coefficients according to multinomial
        // theorem and add it to the summatory
        // For the product, it is sufficient to call prod(coeffs_input_needed)
        // without including the exponent m, as this vector will contain
        // each coefficient as many times as its exponent would indicate.
        // REVISETHISLATER esto debería poder hacerse sin bucle con row product
        summatory += multinomial_coef *
          arma::prod(coeffs_input_needed, 1) % arma::pow(coeffs_input.col(0), difference);
        // Note that coeffs_input[0] is the intercept
      }
      // After the summatory over the partitions has been computed, we need to
      // get its result and multiply by the correspondent derivative value, and
      // add to the already stored values, here we are computing the summatory
      // over n.
      coeffs_output.col(coeff_index) = coeffs_output.col(coeff_index) + g[n] * summatory;
    }
  }

  return coeffs_output;
}

// [[Rcpp::export]]
Weights alg_non_linear(const Weights& coeffs_input,
                       const Terms& labels_input,
                       const Terms& labels_output,
                       const Term& taylor_orders,
                       int current_layer,
                       const Coeffs& g) {
  PartitionCache pcache;
  return alg_non_linear_impl(
    coeffs_input,
    labels_input,
    labels_output,
    taylor_orders,
    current_layer,
    g,
    pcache
  );
}

inline void check_weights_dimensions(const Layers& layers) {
  for (size_t i = 1; i < layers.size(); i++) {
    if (layers[i].n_rows != layers[i - 1].n_cols + 1)
      stop("The list of weights has incorrect dimensions. Please, check the right dimmensions in the documentation.");
  }
}

// [[Rcpp::export]]
List nn2poly_algorithm(const Layers& layers,
                       const Functions& af_list,
                       double max_order,
                       bool keep_layers,
                       const Term& taylor_orders) {
  if (layers.empty())
    stop("Argument `layers` is empty");
  if (af_list.empty())
    stop("Activation functions are missing in `af_list`");
  if (layers.size() != af_list.size())
    stop("`layers` and `af_list` must have the same length");

  check_weights_dimensions(layers);

  // Obtain number of variables (dimension p)
  const int p = static_cast<int>(layers[0].n_rows) - 1;

  // Obtain number of layers L (L-1 hidden + 1 output, input is denoted by 0)
  const int L = static_cast<int>(af_list.size());

  // Check if the last layer is linear
  const bool last_linear = (af_list.back() == "linear");

  // The list with the results of coefficients at each layer,
  // depending on the last layer being linear or not
  WeightsLists results(static_cast<size_t>(last_linear ? 2 * L - 1 : 2 * L));

  // Create a default taylor_orders if it is not given by the user
  const Term taylor = obtain_taylor_vector(taylor_orders, af_list);

  // Obtain all the derivatives up to the desired Taylor degree at each layer
  const CoeffsList af_derivatives_list = obtain_derivatives_list(taylor, af_list);

  // Reuse partitions across non linear layers.
  PartitionCache pcache;

  ////////////////// current_layer = 1, linear ///////////////////

  // Starting point for the algorithm: Set weights as coefficients
  // of an order 1 polynomial.

  // The labels for each coefficient vector at the same layer and linear or
  // same layer and non linear case will have be the same, so they can be
  // stored only once as the element `labels` of the coeffs_list_output,
  // so we define length h+1
  // For each neuron in the first hidden layer, when computing the activation
  // potentials (u_j), each column of the weight matrix represents the
  // coefficients of an order 1 polynomial for that neuron potential.
  // The first element will be the bias, and the rest the coefficient
  // associated with each variable from x_1 to x_p.
  Weights coeffs_list_output = arma::trans(layers[0]);

  // generate and store the labels (as a list of integer vectors)
  // In this case the integer vectors are of length 1.
  Terms labels_output;
  labels_output.reserve(p + 1);
  for (int i = 0; i <= p; i++)
    labels_output.push_back(Term{ i });

  // Store the results
  results[0] = WeightsList{labels_output, coeffs_list_output};

  // Stop if last layer and the last layer is linear
  int new_total_order = 1;
  if (L == 1 && last_linear)
    goto out;

  ////////////////// Loop over all layers ///////////////////

  // Note that the loop will iterate the current layer from 1 to L
  // and compute the linear and then non linear situation.
  // The linear case at layer 1 has been computed outside so we skip it

  for (int current_layer = 1; current_layer <= L; current_layer++) {

    ///////////////// Linear case //////////////////
    if (current_layer != 1) {

      // Treat the previous coefficients output as input
      const Weights coeffs_list_input = coeffs_list_output;

      // Note that the polynomial in this case does not increase its order
      // from the one in the non linear previous layer, so the labels
      // will be the same and are already stored in $labels output.
      // Only the matrix of $values will change its number of rows

      /////// New  version alg linear START  ----------------------------------
      // apply the linear algorithm
      arma::rowvec first_row(coeffs_list_input.n_cols, arma::fill::zeros);
      first_row[0] = 1.0;
      Weights stacked(coeffs_list_input.n_rows + 1, coeffs_list_input.n_cols);
      stacked.row(0) = first_row;
      stacked.rows(1, coeffs_list_input.n_rows) = coeffs_list_input;
      coeffs_list_output = arma::trans(layers[current_layer - 1]) * stacked;

      /////// New  version alg linear END  ----------------------------------

      // Save results from this layer:
      results[2 * current_layer - 2] = WeightsList{labels_output, coeffs_list_output};

      // Stop if last layer and the last layer is linear
      if (current_layer == L && last_linear)
        goto out;
    }

    ////////// Non linear case //////////
    // The output dimension remains the same as in the previous linear case,
    // because the same number of neurons is considered

    // Treat the previous coeff output as input
    const Weights coeffs_list_input = coeffs_list_output;
    const Terms labels_input = labels_output;

    // In the non linear case the polynomial order increases (unless max_order is
    // reached), so the new labels need to be computed. However, the previous
    // ones can be reused.
    // The new labels will be for monomials of orders between the total order
    // of the previous polynomial and the total order of the new polynomial:
    int previous_total_order = 1;
    if (current_layer != 1)
      previous_total_order = new_total_order;

    // Compute the new total order with the product of taylor_orders.
    // If a max_order value is used, its taken as the minimum between both.
    new_total_order = std::min(previous_total_order * taylor[current_layer - 1],
                               static_cast<int>(max_order));

    // If the order has increased, create new needed labels.
    // If not, max_order has been reached and no new labels are needed.
    if (previous_total_order != new_total_order) {
      // Loop over each of the new orders up to the maximum one
      for (int order = previous_total_order + 1; order <= new_total_order; order++) {
        Terms comb = combinations_with_repetition(p, order);

        // update labels with the new ones
        for (const Term& term : comb)
          labels_output.push_back(term);
      }
    }

    // The output index is already computed in the linear case
    // but not for l=1 #REVISETHISLATER
    coeffs_list_output = alg_non_linear_impl(
      coeffs_list_input,
      labels_input,
      labels_output,
      taylor,
      current_layer,
      af_derivatives_list[current_layer - 1],
      pcache
    );
#ifdef NN2POLY_DEBUG
    pcache.debug(current_layer);
#endif

    // Save results from this layer:
    results[2 * current_layer - 1] = WeightsList{labels_output, coeffs_list_output};

    // Check if this is the last layer and if the last layer is not linear
    if (current_layer == L && !last_linear)
      goto out;
  }

out:
#ifdef NN2POLY_DEBUG
  pcache.debug();
#endif
  if (keep_layers)
    return wrap(results);
  return wrap(results.back());
}
