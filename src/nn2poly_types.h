#ifndef nn2poly__types_h
#define nn2poly__types_h

#include <vector>
#include <unordered_map>
#include <vector>
#include <iostream>
#include <string>
#include <cstdint>

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
    for(auto item : key) {
      uint32_t x = static_cast<uint32_t>(item);
      x = ((x >> 16) ^ x) * 0x45d9f3b;
      x = ((x >> 16) ^ x) * 0x45d9f3b;
      x = (x >> 16) ^ x;
      seed ^= x + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
  }
};

struct TermQ {
  Term signature;
  int q_previous_layer;

  bool operator==(const TermQ& other) const {
    return q_previous_layer == other.q_previous_layer && signature == other.signature;
  }
};

struct TermQHash {
  std::size_t operator()(const TermQ& key) const noexcept {
    TermHash term_hash;
    std::size_t seed = term_hash(key.signature);
    std::size_t q_hash = std::hash<int>{}(key.q_previous_layer);
    seed ^= q_hash + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    return seed;
  }
};

using TermMap = std::unordered_map<Term, size_t, TermHash>;

struct PartitionCount {
  Term idx;
  Term counts;
  size_t size;
};

using PartitionCounts = std::vector<PartitionCount>;

struct TermEquivalence {
  Term signature;
  Term canonical_order;
};

#include "debug.h"

using PartitionCache = nn2poly::PartitionCache;
using Coeffs = std::vector<double>;
using CoeffsList = std::vector<Coeffs>;

#include "linalg.h"

using Weights = nn2poly::linalg::Weights;
using Layers = std::vector<Weights>;
using Functions = std::vector<std::string>;

struct WeightsList {
  Terms labels;
  Weights values;
};

using WeightsLists = std::vector<WeightsList>;

#define CHECK_INTERRUPT() Rcpp::checkUserInterrupt()
using List = Rcpp::List;

namespace Rcpp {
List wrap(const PartitionsList& data);
List wrap(const WeightsList& data);
List wrap(const WeightsLists& data);
}

#endif
