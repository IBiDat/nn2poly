#include "nn2poly_types.h"
#include "partitions.h"
#include "multiset.h"

// [[Rcpp::export]]
Terms combinations_with_repetition(int n, int k) {
  if (n <= 0 || k < 0) throw std::invalid_argument(
    "arguments `n` and `k` must satisfy n > 0 and k >= 0");

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

TermEquivalence summarize_label_equivalence(const Term& label) {
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

const Partition& parts_signature(const Term& sign, PartitionCache& pcache) {
  // ---------------------------------------------------------
  // LEVEL 3: signature cache (structural matches only)
  // ---------------------------------------------------------
  auto it = pcache.signature.find(sign);
  if (it == pcache.signature.end()) {
    Partition parts;
    auto partitions = multiset_partitions(sign);
    for (auto it = partitions.begin(); it != partitions.end(); ++it)
      parts.push_back(*it);
    it = pcache.signature.emplace(sign, std::move(parts)).first;
  }
  return it->second;
}

Partition parts_filtering(const Term& sign, int q, PartitionCache& pcache) {
  const Partition& parts_sig = parts_signature(sign, pcache);
  NN2POLY_DEBUG_LOG(4, DTAG(parts_sig));

  Partition parts;
  parts.reserve(parts_sig.size()); // Maximum possible size

  for (const Terms& terms : parts_sig) {
    bool allowed = true;
    for (const Term& term : terms) {
      if (static_cast<int>(term.size()) > q) {
        allowed = false;
        break;
      }
    }
    if (allowed) parts.push_back(terms);
  }

  return parts;
}

const Partition& parts_filtered(const Term& sign, int q, PartitionCache& pcache) {
  // ---------------------------------------------------------
  // LEVEL 2: filtered cache (filtered structural matches)
  // ---------------------------------------------------------
  TermQ key{sign, q};
  auto it = pcache.filtered.find(key);
  if (it == pcache.filtered.end()) {
    Partition parts = parts_filtering(sign, q, pcache);
    it = pcache.filtered.emplace(key, std::move(parts)).first;
  }
  return it->second;
}

// Index values to compute the multinomial coefficient
// This is simply counting how many times each unique term appears,
// obtaining the factorials and then doing the product. The terms that
// do not appear dont need to be counted as they will be 0, their
// factorial 1 and at the end will, not affect the total product.
// Then we need to use the labels to get the needed coefficients
PartitionCount summarize_terms(const Terms& terms, const TermMap& map) {
  PartitionCount pcount;
  pcount.idx.reserve(terms.size());
  pcount.counts.reserve(terms.size());
  pcount.size = terms.size();

  // terms are already sorted!
  for (auto it = terms.begin(); it != terms.end(); ) {
    auto next_it = std::find_if(it, terms.end(), [&](const Term& v) {
      return v != *it;
    });
    pcount.counts.push_back(std::distance(it, next_it));
    pcount.idx.push_back(map.find(*it)->second);
    it = next_it;
  }

  return pcount;
}

PartitionCounts parts_counting(const TermEquivalence& eq, int q,
                               const TermMap& map, PartitionCache& pcache) {
  const Partition& parts_filt = parts_filtered(eq.signature, q, pcache);
  NN2POLY_DEBUG_LOG(4, DTAG(parts_filt));

  PartitionCounts pcounts;
  pcounts.reserve(parts_filt.size()); // Exact size known

  for (const Terms& terms : parts_filt) {
    Terms ren_terms;
    ren_terms.reserve(terms.size());
    for (const Term& term : terms) {
      Term ren_term;
      ren_terms.reserve(term.size());
      for (int rank : term)
        ren_term.push_back(eq.canonical_order[rank - 1]);
      std::sort(ren_term.begin(), ren_term.end());
      ren_terms.push_back(std::move(ren_term));
    }
    std::sort(ren_terms.begin(), ren_terms.end());
    PartitionCount pcount = summarize_terms(ren_terms, map);
    NN2POLY_DEBUG_LOG(5, DTAG(ren_terms), DTAG(pcount));
    pcounts.push_back(std::move(pcount));
  }

  return pcounts;
}

const PartitionCounts& build_partition_counts(const Term& label, int q,
                                              const TermMap& map,
                                              PartitionCache& pcache) {
  // ---------------------------------------------------------
  // LEVEL 1: renamed cache, exact match
  // ---------------------------------------------------------
  TermQ key{label, q};
  auto it = pcache.pcounts.find(key);

  if (it != pcache.pcounts.end())
    return it->second;

  // Find the equivalence between label and the ones needed for the
  // reduced partitions list
  const TermEquivalence eq = summarize_label_equivalence(label);
  NN2POLY_DEBUG_LOG(3, DTAG(label), DTAG(eq));

  PartitionCounts pcounts = parts_counting(eq, q, map, pcache);
  it = pcache.pcounts.emplace(key, std::move(pcounts)).first;

  return it->second;
}
