#ifndef nn2poly__linalg_eigen_h
#define nn2poly__linalg_eigen_h

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

namespace nn2poly {
namespace linalg {

using Weights = Eigen::MatrixXd;
using Vector = Eigen::VectorXd;

inline int n_rows(const Weights& m) { return m.rows(); }
inline int n_cols(const Weights& m) { return m.cols(); }

inline Weights zeros(int rows, int cols) {
  return Eigen::MatrixXd::Zero(rows, cols);
}

inline Vector zeros(int size) {
  return Eigen::VectorXd::Zero(size);
}

inline Weights trans(const Weights& mat) {
  return mat.transpose();
}

inline Weights alg_linear(const Weights& coeffs_list, const Weights& layer) {
  Weights joined(coeffs_list.rows() + 1, coeffs_list.cols());
  joined.row(0).setZero();
  joined(0, 0) = 1.0;
  joined.bottomRows(coeffs_list.rows()) = coeffs_list;
  return layer.transpose() * joined;
}

inline void add_partition(Weights& mat, int i, double scalar, const Vector& vec) {
  mat.col(i) += scalar * vec;
}

inline void add_poly_eval(Weights& mat, int i,
                          const Weights& in, const Coeffs& g, int q) {
  if (q < 0) throw std::invalid_argument("negative q in add_poly_eval");

  // Horner's method: work backwards down to the constant term
  Vector result = Vector::Constant(in.rows(), g[q]);
  for (int n = q - 1; n >= 0; --n)
    result = (result.array() * in.col(i).array()) + g[n];
  mat.col(i) += result;
}

inline Vector accumulate_partition(const Weights& coeffs_input, int d,
                                   const Term& idx, const Term& mult) {
  if (idx.empty()) throw std::invalid_argument("empty idx in accumulate_partition");

  Weights needed(coeffs_input.rows(), idx.size());
  for (size_t i = 0; i < idx.size(); ++i)
    needed.col(i) = coeffs_input.col(idx[i]).array().pow(mult[i]);
  return needed.rowwise().prod().array() * coeffs_input.col(0).array().pow(d);
}

} // namespace linalg
} // namespace nn2poly

#endif
