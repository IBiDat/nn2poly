#ifndef nn2poly__approx_taylor_h
#define nn2poly__approx_taylor_h

namespace nn2poly {
namespace detail {

// Helper to handle the shared Taylor convolution loop structure
template<typename Func>
void compute_taylor_conv(Coeffs& c, Func step, double c0) {
  c[0] = c0;
  for (size_t n = 0; n < c.size() - 1; n++) {
    double conv = 0.0;
    for (size_t k = 0; k <= n; k++)
      conv += c[k] * c[n - k];
    c[n + 1] = step(n, conv, c) / static_cast<double>(n + 1);
  }
}

inline void coeffs_taylor_sigmoid(Coeffs& c, double a) {
  compute_taylor_conv(c, [](size_t n, double conv, const Coeffs& c) {
    return (c[n] - conv);
  }, 1.0 / (1.0 + std::exp(-a)));
}

inline void coeffs_taylor_tanh(Coeffs& c, double a) {
  compute_taylor_conv(c, [](size_t n, double conv, const Coeffs&) {
    return (n == 0 ? 1.0 : 0.0) - conv;
  }, std::tanh(a));
}

inline void coeffs_taylor_linear(Coeffs& c, double a) {
  c[0] = a;
  if (c.size() > 1) {
    c[1] = 1.0;
    std::fill(c.begin() + 2, c.end(), 0.0);
  }
}

inline void coeffs_taylor_softplus(Coeffs& c, double a) {
  coeffs_taylor_sigmoid(c, a);
  for (size_t n = c.size() - 1; n > 0; n--)
    c[n] = c[n - 1] / static_cast<double>(n);
  if (a > 0.0)
    c[0] = a + std::log(1.0 + std::exp(-a));
  else c[0] = std::log(1.0 + std::exp(a));
}

inline void coeffs_taylor(const std::string& af, Coeffs& c, double a = 0.0) {
  if (c.empty()) return;
  if (af == "sigmoid")
    return coeffs_taylor_sigmoid(c, a);
  if (af == "tanh")
    return coeffs_taylor_tanh(c, a);
  if (af == "softplus")
    return coeffs_taylor_softplus(c, a);
  if (af == "linear")
    return coeffs_taylor_linear(c, a);
  throw std::invalid_argument("function '" + af + "' not supported");
}

} // namespace detail
} // namespace nn2poly

#endif
