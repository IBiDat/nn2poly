#include "nn2poly_types.h"

namespace Rcpp {

List wrap(const PartitionsList& data) {
  return List::create(
    _["labels"]     = data.labels,
    _["partitions"] = data.partitions
  );
}

List wrap(const WeightsList& data) {
  return List::create(
    _["labels"] = data.labels,
    // Transpose to have polynomials as columns
    _["values"] = arma::trans(data.values)
  );
}

List wrap(const WeightsLists& data) {
  const int n_items = static_cast<int>(data.size());
  const int n_layers = static_cast<int>(std::ceil(static_cast<double>(n_items) / 2.0));

  List out(n_layers);
  CharacterVector out_names(n_layers);

  for (int i = 0; i < n_layers; i++) {
    const int input_index = 2 * i;
    const int output_index = 2 * i + 1;

    List layer(2);
    CharacterVector layer_names = CharacterVector::create("input", "output");

    layer[0] = wrap(data[input_index]);
    if (output_index < n_items) {
      layer[1] = wrap(data[output_index]);
    } else {
      // If there is a linear output, i.e. single polynomial in final layer
      // and odd number of items, then we repeat the input as the output, as
      // the activation functions takes no effect on the polynomial
      layer[1] = layer[0];
    }

    layer.attr("names") = layer_names;
    out[i] = layer;
    out_names[i] = "layer_" + std::to_string(i + 1);
  }

  out.attr("names") = out_names;
  return out;
}

}
