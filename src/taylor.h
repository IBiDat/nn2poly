#ifndef nn2poly__taylor_h
#define nn2poly__taylor_h

#include "nn2poly_types.h"

Coeffs coeffs_taylor_sigmoid(int order) {
  Coeffs a(order + 1, 0.0);
  a[0] = 0.5;
  for (int n = 0; n < order; n++) {
    double conv = 0.0;
    for (int k = 0; k <= n; k++)
      conv += a[k] * a[n - k];
    a[n + 1] = (a[n] - conv) / static_cast<double>(n + 1);
  }
  return a;
}

Coeffs coeffs_taylor_tanh(int order) {
  Coeffs a(order + 1, 0.0);
  for (int n = 0; n < order; n++) {
    double conv = 0.0;
    for (int k = 0; k <= n; k++)
      conv += a[k] * a[n - k];
    const double rhs = (n == 0 ? 1.0 : 0.0) - conv;
    a[n + 1] = rhs / static_cast<double>(n + 1);
  }
  return a;
}

Coeffs coeffs_taylor_linear(int order) {
  Coeffs a(order + 1, 0.0);
  if (order >= 1)
    a[1] = 1.0;
  return a;
}

Coeffs coeffs_taylor_softplus(int order) {
  Coeffs a(order + 1, 0.0);
  a[0] = std::log(2.0);
  if (order == 0)
    return a;
  Coeffs sig = coeffs_taylor_sigmoid(order - 1);
  for (int n = 1; n <= order; n++)
    a[n] = sig[n - 1] / static_cast<double>(n);
  return a;
}

Coeffs coeffs_taylor(const std::string& af, int order) {
  if (af == "sigmoid")
    return coeffs_taylor_sigmoid(order);
  if (af == "tanh")
    return coeffs_taylor_tanh(order);
  if (af == "softplus")
    return coeffs_taylor_softplus(order);
  if (af == "linear")
    return coeffs_taylor_linear(order);
  throw std::invalid_argument("Function '" + af + "' not supported");
}

#endif
