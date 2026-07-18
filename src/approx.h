#ifndef nn2poly__approx_h
#define nn2poly__approx_h

#include "nn2poly_types.h"
#include "approx_taylor.h"

template <typename... Args>
void coeffs(const std::string& type, const std::string& af, Coeffs& c, Args&&... args) {
  if (type == "taylor") if constexpr (sizeof...(Args) < 2)
    return nn2poly::detail::coeffs_taylor(af, c, std::forward<Args>(args)...);
  throw std::invalid_argument("approximation type '" + type + "' not supported");
}

#endif
