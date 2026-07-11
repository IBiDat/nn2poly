#ifndef nn2poly__partitions_h
#define nn2poly__partitions_h

#include "nn2poly_types.h"

Terms combinations_with_repetition(int n, int k);
Partition build_allowed_terms(const Term& label,
                              int q_previous_layer,
                              PartitionCache& partition_cache);

inline TermEquivalence summarize_label_equivalence(const Term& label) {
  std::unordered_map<int, int> counts;
  for (int value : label)
    counts[value]++;

  std::vector<std::pair<int, int>> frequency;
  frequency.reserve(counts.size());
  for (const auto& entry : counts)
    frequency.emplace_back(entry.first, entry.second);

  std::sort(frequency.begin(), frequency.end(), [](const auto& left, const auto& right) {
    if (left.second != right.second)
      return left.second > right.second;
    return left.first < right.first;
  });

  Term canonical_order;
  canonical_order.reserve(frequency.size());
  std::unordered_map<int, int> rank_by_value;
  for (size_t i = 0; i < frequency.size(); i++) {
    canonical_order.push_back(frequency[i].first);
    rank_by_value[frequency[i].first] = static_cast<int>(i) + 1;
  }

  Term signature;
  signature.reserve(label.size());
  for (int value : label)
    signature.push_back(rank_by_value[value]);
  std::sort(signature.begin(), signature.end());

  return {signature, canonical_order};
}

inline arma::uvec to_arma_indices(const std::vector<size_t>& positions) {
  arma::uvec indices(positions.size());
  for (size_t i = 0; i < positions.size(); i++)
    indices[i] = positions[i];
  return indices;
}

inline TermSummary summarize_terms(const Terms& terms) {
  TermSummary out;
  out.unique_terms.reserve(terms.size());
  out.counts.reserve(terms.size());

  for (size_t i = 0; i < terms.size(); i++) {
    const Term& term = terms[i];
    Term key = term;
    auto it = out.counts.find(key);
    if (it == out.counts.end()) {
      out.counts.emplace(std::move(key), 1);
      out.unique_terms.push_back(term);
    } else it->second++;
  }

  return out;
}

inline std::vector<size_t> in_terms_positions(const TermMap& labels_map,
                                              const TermSummary& term_summary) {
  std::vector<size_t> needed;
  needed.reserve(term_summary.unique_terms.size());
  for (const auto& term : term_summary.unique_terms) {
    auto it = labels_map.find(term);
    if (it != labels_map.end()) {
      needed.push_back(it->second);
    } else {
      stop("Internal error: term not found in labels_input.");
    }
  }
  return needed;
}

#endif
