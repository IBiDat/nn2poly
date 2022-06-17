#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix combinations_with_repetition(int n, int k) {
  IntegerMatrix out(Rf_choose(n + k - 1, k), k);
  IntegerVector pos(k, 1);

  for (int row = 0; true; row++) {
    for (int i = k - 1; i >= 0; i--) {
      if (pos[i] > n) {
        if (i == 0) goto end;
        pos[i - 1]++;
        for (int j = i; j < k; j++)
          pos[j] = pos[j - 1];
      }
    }
    out(row, _) = pos;
    pos[k - 1]++;
  }

end:
  return out;
}
