#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix combinations_with_repetition(int n, int k) {
  int n_row = Rf_choose(n + k - 1, k);
  IntegerMatrix out(n_row, k);
  IntegerVector pos(k, 1);

  for (int row = 0; row < n_row; row++) {
    for (int i = k - 1; i >= 0; i--) {
      if (pos[i] > n) {
        pos[i - 1]++;
        for (int j = i; j < k; j++)
          pos[j] = pos[j - 1];
      }
    }
    out(row, _) = pos;
    pos[k - 1]++;
  }

  return out;
}
