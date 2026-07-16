#ifndef nn2poly__linalg_torch_h
#define nn2poly__linalg_torch_h

#include <torch/torch.h>

namespace nn2poly {
namespace linalg {

using Weights = torch::Tensor;
using Vector = torch::Tensor;

inline int n_rows(const Weights& m) { return m.size(0); }
inline int n_cols(const Weights& m) { return m.size(1); }

inline Weights zeros(int rows, int cols) {
  return torch::zeros({rows, cols}, torch::kFloat64);
}

inline Vector zeros(int size) {
  return torch::zeros({size}, torch::kFloat64);
}

inline Weights trans(const Weights& mat) {
  return mat.t();
}

inline Weights alg_linear(Weights& coeffs_list, const Weights& layer) {
  auto intercept = torch::zeros({1, coeffs_list.size(1)}, coeffs_list.options());
  intercept[0][0] = 1.0;
  auto joined = torch::cat({intercept, coeffs_list}, /*dim=*/0);
  return torch::matmul(layer.t(), joined);
}

inline void add_partition(Weights& mat, int i, double scalar, const Vector& vec) {
  mat.select(1, i).add_(vec * scalar);
}

inline void add_poly_eval(Weights& mat, int i,
                          const Weights& in, const Coeffs& g, int q) {
  if (q < 0) throw std::invalid_argument("negative q in add_poly_eval");

  // Horner's method: work backwards down to the constant term
  auto result = torch::full({in.size(0)}, g[q], in.options());
  auto in_col = in.select(1, i);
  for (int n = q - 1; n >= 0; --n)
    result = (result * in_col) + g[n];
  mat.select(1, i).add_(result);
}

inline Vector accumulate_partition(const Weights& coeffs_input, int d,
                                   const Term& idx, const Term& mult) {
  if (idx.empty()) throw std::invalid_argument("empty idx in accumulate_partition");

  auto idx_tensor = torch::tensor(idx, torch::kLong);
  auto mult_tensor = torch::tensor(mult, coeffs_input.options());
  auto needed = coeffs_input.index_select(1, idx_tensor);
  needed = torch::pow(needed, mult_tensor);
  auto row_prod = torch::prod(needed, /*dim=*/1);
  return row_prod * torch::pow(coeffs_input.select(1, 0), d);
}

} // namespace linalg
} // namespace nn2poly

#include <Rcpp.h>

namespace Rcpp {
template <> torch::Tensor as(SEXP x);
NumericMatrix wrap(const torch::Tensor& data);
}

#endif
