#ifndef nn2poly__taylor_h
#define nn2poly__taylor_h

#include "nn2poly_types.h"

void coeffs_taylor_sigmoid(Coeffs& c, double a) {
  c[0] = 1.0 / (1.0 + std::exp(-a));
  for (size_t n = 0; n < c.size() - 1; n++) {
    double conv = 0.0;
    for (size_t k = 0; k <= n; k++)
      conv += c[k] * c[n - k];
    c[n + 1] = (c[n] - conv) / static_cast<double>(n + 1);
  }
}

void coeffs_taylor_tanh(Coeffs& c, double a) {
  c[0] = std::tanh(a);
  for (size_t n = 0; n < c.size() - 1; n++) {
    double conv = 0.0;
    for (size_t k = 0; k <= n; k++)
      conv += c[k] * c[n - k];
    const double rhs = (n == 0 ? 1.0 : 0.0) - conv;
    c[n + 1] = rhs / static_cast<double>(n + 1);
  }
}

void coeffs_taylor_linear(Coeffs& c, double a) {
  c[0] = a;
  if (c.size() > 1) {
    c[1] = 1.0;
    std::fill(c.begin() + 2, c.end(), 0.0);
  }
}

void coeffs_taylor_softplus(Coeffs& c, double a) {
  coeffs_taylor_sigmoid(c, a);
  for (size_t n = c.size() - 1; n > 0; n--)
    c[n] = c[n - 1] / static_cast<double>(n);
  if (a > 0.0)
    c[0] = a + std::log(1.0 + std::exp(-a));
  else c[0] = std::log(1.0 + std::exp(a));
}

void coeffs_taylor(const std::string& af, Coeffs& c, double a = 0.0) {
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

#endif
