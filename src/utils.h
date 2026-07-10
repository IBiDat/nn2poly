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

using TermKey = std::vector<int>;

struct TermKeyHash {
  std::size_t operator()(TermKey const& key) const noexcept {
    std::size_t seed = key.size();
    for(auto x : key) {
      x = ((x >> 16) ^ x) * 0x45d9f3b;
      x = ((x >> 16) ^ x) * 0x45d9f3b;
      x = (x >> 16) ^ x;
      seed ^= x + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
  }
};

TermKey as_termkey(const IntegerVector& x) {
  return TermKey(x.begin(), x.end());
}

struct PartitionSummary {
  std::vector<IntegerVector> unique_terms;
  std::unordered_map<TermKey, int, TermKeyHash> counts;
};

PartitionSummary summarize_partition(const ListOf<IntegerVector>& partition) {
  PartitionSummary out;
  out.unique_terms.reserve(partition.size());
  out.counts.reserve(partition.size());

  for (int i = 0; i < partition.size(); i++) {
    IntegerVector term = partition[i];
    TermKey key = as_termkey(term);
    auto it = out.counts.find(key);
    if (it == out.counts.end()) {
      out.counts.emplace(std::move(key), 1);
      out.unique_terms.push_back(term);
    } else it->second++;
  }

  return out;
}

LogicalVector in_partition(const ListOf<IntegerVector>& labels_input,
                           const PartitionSummary& part_summary) {
  LogicalVector needed(labels_input.size());
  for (int i = 0; i < labels_input.size(); i++)
    needed[i] = (part_summary.counts.find(as_termkey(labels_input[i])) != part_summary.counts.end());
  return needed;
}

#endif
