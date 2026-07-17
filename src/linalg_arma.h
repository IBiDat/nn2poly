#ifndef nn2poly__linalg_arma_h
#define nn2poly__linalg_arma_h

namespace nn2poly {
namespace linalg {

using Weights = arma::mat;
using Vector = arma::vec;

inline int n_rows(const Weights& m) { return m.n_rows; }
inline int n_cols(const Weights& m) { return m.n_cols; }

inline Weights zeros(int rows, int cols) {
  return arma::zeros<Weights>(rows, cols);
}

inline Vector zeros(int size) {
  return arma::zeros<Vector>(size);
}

inline Weights trans(const Weights& mat) {
  return arma::trans(mat);
}

inline Weights alg_linear(Weights& coeffs_list, const Weights& layer) {
  Weights mat = layer.rows(1, layer.n_rows - 1).t() * coeffs_list;
  mat.col(0) += layer.row(0).t();
  return mat;
}

inline void add_partition(Weights& mat, int i, double scalar, const Vector& vec) {
  mat.col(i) += scalar * vec;
}

inline void add_poly_eval(Weights& mat, int i,
                          const Weights& in, const Coeffs& g, int q) {
  if (q < 0) throw std::invalid_argument("negative q in add_poly_eval");

  // Horner's method: work backwards down to the constant term
  Vector result(in.n_rows, arma::fill::value(g[q]));
  for (int n = q - 1; n >= 0; --n)
    result = (result % in.col(i)) + g[n];
  mat.col(i) += result;
}

inline Vector accumulate_partition(const Weights& coeffs_input, int d,
                                   const Term& idx, const Term& mult) {
  if (idx.empty()) throw std::invalid_argument("empty idx in accumulate_partition");

  Weights needed = coeffs_input.cols(arma::conv_to<arma::uvec>::from(idx));
  for (unsigned int i = 0; i < needed.n_cols; ++i)
    needed.col(i) = arma::pow(needed.col(i), mult[i]);
  return arma::prod(needed, 1) % arma::pow(coeffs_input.col(0), d);
}

} // namespace linalg
} // namespace nn2poly

#endif
