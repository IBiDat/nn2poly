#include <Rcpp.h>
#include "multiset.h"
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

// [[Rcpp::export]]
List generate_partitions(int p, int q_max) {
  // Initialize the output
  std::list<std::list<Partition<int>>> output;

  // Iterate over the degrees
  for (int i = 1; i <= q_max; i++) {
    // Generate all the combinations for the coeffs of the given order
    IntegerMatrix comb = combinations_with_repetition(p, i);

    // Iterate over those combinations obtaining its partitions when needed
    for (int j = 0; j < comb.nrow(); j++) {
      // Count the occurences of each variable in the combination
      std::multiset<int> mset(comb(j, _).begin(), comb(j, _).end());

      // Check if occurrences are sorted in descending order, which
      // eliminates the equivalent situations (i.e., 112 is equivalent to 223,
      // to 332, to 221 and so on)
      int k = 1;
      for (size_t last = mset.count(k++); k <= p; k++) {
        size_t cur = mset.count(k);
        if (last < cur) break;
        last = cur;
      }
      if (k <= p) continue;

      // Add the list of partitions for that combination to the output
      std::list<Partition<int>> tmp;
      for (auto partition: multiset_partitions<int>(mset))
        tmp.push_back(partition);
      output.push_back(tmp);
    }
  }

  return wrap(output);
}


// [[Rcpp::export]]
List generate_partitions_full(int p, int q_max) {
  // Initialize the output
  std::list<std::list<Partition<int>>> output;

  // Iterate over the degrees
  for (int i = 1; i <= q_max; i++) {
    // Generate all the combinations for the coeffs of the given order
    IntegerMatrix comb = combinations_with_repetition(p, i);

    // Iterate over those combinations obtaining its partitions
    for (int j = 0; j < comb.nrow(); j++) {
      // Count the occurences of each variable in the combination
      std::multiset<int> mset(comb(j, _).begin(), comb(j, _).end());

      // Add the list of partitions for that combination to the output
      std::list<Partition<int>> tmp;
      for (auto partition: multiset_partitions<int>(mset))
        tmp.push_back(partition);
      output.push_back(tmp);
    }
  }

  return wrap(output);
}
