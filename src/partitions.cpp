#include "nn2poly_types.h"
#include "partitions.h"
#include "multiset.h"

// [[Rcpp::export]]
Terms combinations_with_repetition(int n, int k) {
  if (n <= 0 || k < 0)
    throw std::invalid_argument("arguments `n` and `k` must satisfy n > 0 and k >= 0");

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

Partition build_allowed_terms(const Term& label,
                              int q_previous_layer,
                              PartitionCache& pcache) {
  // ---------------------------------------------------------
  // LEVEL 1: renamed cache, exact match
  // ---------------------------------------------------------
  TermQ renamed_key{label, q_previous_layer};
  auto ren_it = pcache.renamed.find(renamed_key);

  if (ren_it != pcache.renamed.end())
    return ren_it->second;

  // Find the equivalence between label and the ones needed for the
  // reduced partitions list
  const TermEquivalence eq = summarize_label_equivalence(label);
  NN2POLY_DEBUG_LOG(3, DTAG(eq));

  // ---------------------------------------------------------
  // LEVEL 2: filtered cache (filtered structural matches)
  // ---------------------------------------------------------
  TermQ filter_key{eq.signature, q_previous_layer};
  auto filt_it = pcache.filtered.find(filter_key);

  if (filt_it == pcache.filtered.end()) {
    // ---------------------------------------------------------
    // LEVEL 3: signature cache (structural matches only)
    // ---------------------------------------------------------
    auto sig_it = pcache.signature.find(eq.signature);

    if (sig_it == pcache.signature.end()) {
      std::multiset<int> mset(eq.signature.begin(), eq.signature.end());
      Partition sig_parts;
      auto partitions = multiset_partitions(mset);
      for (auto it = partitions.begin(); it != partitions.end(); ++it)
        sig_parts.push_back(*it);

      sig_it = pcache.signature.emplace(eq.signature, std::move(sig_parts)).first;
    }
    NN2POLY_DEBUG_LOG(4, DTAG(sig_it->second));

    // Filtering
    Partition filt_parts;
    filt_parts.reserve(sig_it->second.size()); // Maximum possible size
    for (const Terms& terms : sig_it->second) {
      bool allowed = true;
      for (const Term& term : terms) {
        if (static_cast<int>(term.size()) > q_previous_layer) {
          allowed = false;
          break;
        }
      }
      if (allowed) filt_parts.push_back(terms);
    }

    filt_it = pcache.filtered.emplace(filter_key, std::move(filt_parts)).first;
  }
  NN2POLY_DEBUG_LOG(4, DTAG(filt_it->second));

  // Renaming
  Partition ren_parts;
  ren_parts.reserve(filt_it->second.size()); // Exact size known
  for (const Terms& terms : filt_it->second) {
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
    ren_parts.push_back(std::move(ren_terms));
  }

  ren_it = pcache.renamed.emplace(renamed_key, std::move(ren_parts)).first;

  return ren_it->second;
}
