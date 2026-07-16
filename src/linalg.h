#ifndef nn2poly__linalg_h
#define nn2poly__linalg_h

#include <array>

namespace nn2poly {
namespace linalg {

constexpr std::array<double, 171> precompute_factorials() {
  std::array<double, 171> f{};
  f[0] = 1.0;
  for (size_t i = 1; i < f.size(); ++i)
    f[i] = f[i - 1] * static_cast<double>(i);
  return f;
}
inline constexpr std::array<double, 171> factorials = precompute_factorials();

} // namespace linalg
} // namespace nn2poly

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#ifndef NN2POLY_USE_TORCH
  #include "linalg_arma.h"
#else
  #include "linalg_torch.h"
#endif

#endif
