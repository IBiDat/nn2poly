#ifndef nn2poly__types_h
#define nn2poly__types_h

#include <vector>
#include <unordered_map>

using Term = std::vector<int>;
using Terms = std::vector<Term>;
using Partition = std::vector<Terms>;
using Partitions = std::vector<Partition>;

struct PartitionsList {
  Terms labels;
  Partitions partitions;
};

struct TermHash {
  std::size_t operator()(Term const& key) const noexcept {
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

using PartitionCache = std::unordered_map<Term, Partition, TermHash>;
using TermMap = std::unordered_map<Term, size_t, TermHash>;

struct TermSummary {
  Terms unique_terms;
  TermMap counts;
};

struct TermEquivalence {
  Term signature;
  Term canonical_order;
};

using Coeffs = std::vector<double>;
using CoeffsList = std::vector<Coeffs>;

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

using Weights = arma::mat;
using Layers = std::vector<arma::mat>;
using Functions = std::vector<std::string>;

struct WeightsList {
  Terms labels;
  Weights values;
};

using WeightsLists = std::vector<WeightsList>;

namespace Rcpp {
List wrap(const PartitionsList& data);
List wrap(const WeightsList& data);
List wrap(const WeightsLists& data);
}

#endif