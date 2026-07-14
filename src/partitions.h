#ifndef nn2poly__partitions_h
#define nn2poly__partitions_h

#include "nn2poly_types.h"

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

inline Terms rename_terms(const Terms& terms, const Term& canonical_order) {
  Terms renamed;
  renamed.reserve(terms.size());

  for (const Term& term : terms) {
    Term mapped;
    mapped.reserve(term.size());
    for (int value : term) {
      const int index = value - 1;
      if (index < 0 || index >= static_cast<int>(canonical_order.size())) {
        stop("Internal error while renaming partition terms.");
      }
      mapped.push_back(canonical_order[index]);
    }
    std::sort(mapped.begin(), mapped.end());
    renamed.push_back(std::move(mapped));
  }

  return renamed;
}

inline Partition filter_allowed_terms(const Term& equivalent_label,
                                      int q_previous_layer,
                                      const Terms& labels,
                                      const Partitions& partitions) {
  //REVISETHISLATER This function could be omitted if we already include it when
  // generating the partitions.

  // Obtain chosen label position from the partitions labels list:
  auto label_it = std::find(labels.begin(), labels.end(), equivalent_label);
  if (label_it == labels.end())
    stop("Internal error while locating the equivalent partition label.");
  const size_t label_index = static_cast<size_t>(std::distance(labels.begin(), label_it));

  Partition output;
  for (const Terms& terms : partitions[label_index]) {
    bool allowed = true;
    // Check that the given partition has all elements allowed by q_previous_layer.
    for (const Term& term : terms) {
      if (static_cast<int>(term.size()) > q_previous_layer) {
        allowed = false;
        break;
      }
    }
    if (allowed)
      output.push_back(terms);
  }

  return output;
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

inline std::vector<size_t> in_terms_positions(const Terms& labels_input,
                                              const TermSummary& term_summary) {
  std::vector<size_t> needed;
  needed.reserve(labels_input.size());
  for (size_t i = 0; i < labels_input.size(); i++) {
    if (term_summary.counts.find(labels_input[i]) != term_summary.counts.end())
      needed.push_back(i);
  }
  return needed;
}

#endif
