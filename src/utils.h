#ifndef nn2poly__utils_h
#define nn2poly__utils_h

#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
IntegerVector order(Vector<RTYPE>& x, bool decreasing=false) {
  IntegerVector out(x.size());
  std::iota(out.begin(), out.end(), 0);
  std::sort(out.begin(), out.end(), [&](size_t a, size_t b) {
    if (decreasing)
      return x[a] > x[b];
    return x[a] < x[b];
  });
  return out;
}

template <int RTYPE>
Vector<RTYPE> concat(Vector<RTYPE>& x, Vector<RTYPE>&y) {
  int xn = x.size();
  int yn = y.size();
  Vector<RTYPE> out(xn + yn);
  for (int i = 0; i < xn; i++)
    out[i] = x[i];
  for (int i = 0; i < yn; i++)
    out[xn + i] = y[i];
  return out;
}

template <int RTYPE>
typename traits::storage_type<RTYPE>::type prod(Vector<RTYPE>& x) {
  using type = typename traits::storage_type<RTYPE>::type;
  type init = 1;
  return std::accumulate(x.begin(), x.end(), init, std::multiplies<type>());
}

#endif
