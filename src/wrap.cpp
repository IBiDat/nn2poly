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
    _["values"] = wrap(nn2poly::linalg::trans(data.values))
    // Transpose to have polynomials as columns
  );
}

List wrap(const WeightsLists& data) {
  const int n_items = static_cast<int>(data.size());
  const int n_layers = (n_items + 1) / 2;

  List out(n_layers);
  CharacterVector out_names(n_layers);
  const CharacterVector layer_names = CharacterVector::create("input", "output");

  for (int i = 0; i < n_layers; i++) {
    const int input_index = 2 * i;
    const int output_index = 2 * i + 1;

    List layer(2);
    layer[0] = wrap(data[input_index]);
    layer[1] = (output_index < n_items) ? wrap(data[output_index]) : layer[0];
    // If there is a linear output, i.e. single polynomial in final layer
    // and odd number of items, then we repeat the input as the output, as
    // the activation functions takes no effect on the polynomial

    layer.attr("names") = layer_names;
    out[i] = layer;
    out_names[i] = "layer_" + std::to_string(i + 1);
  }

  out.attr("names") = out_names;
  return out;
}

#ifdef TORCH_VERSION

template <>
Weights as(SEXP x) {
  NumericMatrix data(x);
  return torch::from_blob(const_cast<double*>(data.begin()),
    {data.nrow(), data.ncol()}, {1, data.nrow()}, torch::kFloat64
  ).clone();
}

NumericMatrix wrap(const Weights& data) {
  Weights cpu_t = data.to(torch::kCPU).to(torch::kFloat64).contiguous();
  int rows = cpu_t.size(0);
  int cols = cpu_t.size(1);
  NumericMatrix r_mat(rows, cols);
  double* src_ptr = cpu_t.data_ptr<double>();
  std::copy(src_ptr, src_ptr + (rows * cols), r_mat.begin());
  return r_mat;
}

#endif

}
