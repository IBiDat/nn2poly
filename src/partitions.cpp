#include "nn2poly_types.h"
#include "multiset.h"

// [[Rcpp::export]]
Terms combinations_with_repetition(int n, int k) {
  if (n <= 0 || k < 0)
    stop("Arguments `n` and `k` must satisfy n > 0 and k >= 0");

  Terms out;
  if (k == 0) {
    out.push_back(Term{});
    return out;
  }

  Term pos(k, 1);
  while (true) {
    out.push_back(pos);

    int pivot = k - 1;
    while (pivot >= 0 && pos[pivot] == n)
      pivot--;

    if (pivot < 0)
      break;

    pos[pivot]++;
    for (int j = pivot + 1; j < k; j++)
      pos[j] = pos[pivot];
  }

  return out;
}

// [[Rcpp::export]]
Partitions generate_partitions(int p, int q_max) {
  // Initialize the output
  Partitions output;

  // Iterate over the degrees
  for (int i = 1; i <= q_max; i++) {
    // Generate all the combinations for the coeffs of the given order
    Terms comb = combinations_with_repetition(p, i);

    // Iterate over those combinations obtaining its partitions when needed
    for (size_t j = 0; j < comb.size(); j++) {
      // Count the occurences of each variable in the combination
      std::multiset<int> mset(comb[j].begin(), comb[j].end());

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
      Partition tmp;
      for (auto terms: multiset_partitions<int>(mset))
        tmp.push_back(terms);
      output.push_back(tmp);
    }
  }

  return output;
}
