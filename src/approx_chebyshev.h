#ifndef nn2poly__approx_chebyshev_h
#define nn2poly__approx_chebyshev_h

#if __cplusplus >= 202002L
#include <numbers>
#endif

namespace nn2poly {
namespace detail {

#if __cplusplus >= 202002L
inline constexpr double PI = std::numbers::pi;
#else
inline constexpr double PI = 3.14159265358979323846;
#endif

inline constexpr double EPSILON = 10.0 * std::numeric_limits<double>::epsilon();

// Helper to compute Chebyshev coefficients using Gauss-Chebyshev quadrature in-place
template<typename Func>
void compute_chebyshev_coeffs(Coeffs& c, Func f, double a, double b) {
  size_t M = c.size(); // Number of Chebyshev nodes equals vector size (order + 1)

  // Precompute mapped Chebyshev nodes and function values
  std::vector<double> y(M);
  for (size_t j = 0; j < M; ++j) {
    // Node in standard interval [-1, 1]
    double t = std::cos(PI * (static_cast<double>(j) + 0.5) / static_cast<double>(M));
    // Map node from [-1, 1] to target interval [a, b]
    double x = 0.5 * (b - a) * t + 0.5 * (b + a);
    y[j] = f(x);
  }

  // Compute coefficients using discrete orthogonality into the reference vector
  for (size_t k = 0; k < M; ++k) {
    double sum = 0.0;
    for (size_t j = 0; j < M; ++j)
      sum += y[j] * std::cos(PI * static_cast<double>(k) *
        (static_cast<double>(j) + 0.5) / static_cast<double>(M));
    // c[0] uses factor 1/M, while c[k>0] uses factor 2/M
    double coeff = (k == 0 ? 1.0 : 2.0) * sum / static_cast<double>(M);
    c[k] = (std::abs(coeff) < EPSILON) ? 0.0 : coeff;
  }
}

inline void coeffs_chebyshev_sigmoid(Coeffs& c, double a, double b) {
  compute_chebyshev_coeffs(c, [](double x) {
    return 1.0 / (1.0 + std::exp(-x));
  }, a, b);
}

inline void coeffs_chebyshev_tanh(Coeffs& c, double a, double b) {
  compute_chebyshev_coeffs(c, [](double x) {
    return std::tanh(x);
  }, a, b);
}

inline void coeffs_chebyshev_linear(Coeffs& c, double a, double b) {
  compute_chebyshev_coeffs(c, [](double x) {
    return x;
  }, a, b);
}

inline void coeffs_chebyshev_softplus(Coeffs& c, double a, double b) {
  compute_chebyshev_coeffs(c, [](double x) {
    // Numerically stable softplus safeguard
    if (x > 30.0) return x;
    if (x < -30.0) return 0.0;
    return std::log(1.0 + std::exp(x));
  }, a, b);
}

inline void coeffs_chebyshev(const std::string& af, Coeffs& c, double a = -1.0, double b = 1.0) {
  if (c.empty()) return;
  if (af == "sigmoid")
    return coeffs_chebyshev_sigmoid(c, a, b);
  if (af == "tanh")
    return coeffs_chebyshev_tanh(c, a, b);
  if (af == "softplus")
    return coeffs_chebyshev_softplus(c, a, b);
  if (af == "linear")
    return coeffs_chebyshev_linear(c, a, b);
  throw std::invalid_argument("function '" + af + "' not supported");
}

} // namespace detail
} // namespace nn2poly

#endif
