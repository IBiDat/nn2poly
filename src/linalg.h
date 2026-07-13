#ifndef nn2poly__linalg_h
#define nn2poly__linalg_h

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

namespace nn2poly {
namespace linalg {

using Weights = arma::mat;
using Vector = arma::vec;

constexpr std::array<double, 171> precompute_factorials() {
  std::array<double, 171> f{};
  f[0] = 1.0;
  for (size_t i = 1; i < f.size(); ++i)
    f[i] = f[i - 1] * static_cast<double>(i);
  return f;
}
inline constexpr std::array<double, 171> factorials = precompute_factorials();

inline int n_rows(const Weights& m) { return m.n_rows; }

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
  arma::rowvec intercept = arma::zeros<arma::rowvec>(coeffs_list.n_cols);
  intercept[0] = 1.0;
  return arma::trans(layer) * arma::join_cols(intercept, coeffs_list);
}

inline void add_partition(Weights& mat, int i, double scalar, const Vector& vec) {
  mat.col(i) += scalar * vec;
}

inline void add_poly_eval(Weights& mat, int i,
                          const Weights& in, const Coeffs& g, int q) {
  if (q < 0) stop("negative q in add_peval");

  // Horner's method: work backwards down to the constant term
  Vector result(in.n_rows, arma::fill::value(g[q]));
  for (int n = q - 1; n >= 0; --n)
    result = (result % in.col(i)) + g[n];
  mat.col(i) += result;
}

inline Vector accumulate_partition(const Weights& coeffs_input,
                                   const std::vector<size_t>& idx,
                                   const Term& mult, int n) {
  if (idx.empty()) stop("empty idx in accumulate_partition");

  Weights needed = coeffs_input.cols(arma::conv_to<arma::uvec>::from(idx));
  std::vector<int> exponents(mult.begin() + 1, mult.begin() + 1 + idx.size());
  // In torch this would become a single broadcasted tensor operation
  for (unsigned int i = 0; i < needed.n_cols; ++i)
    needed.col(i) = arma::pow(needed.col(i), exponents[i]);

  // Compute the multinomial coefficient
  double m_coef = factorials[n];
  for (int m : mult) if (m > 1)
    m_coef /= factorials[m];

  return m_coef * (arma::prod(needed, 1) % arma::pow(coeffs_input.col(0), mult[0]));
}

} // namespace linalg
} // namespace nn2poly

#endif
