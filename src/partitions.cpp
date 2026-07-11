#include "nn2poly_types.h"
#include "partitions.h"
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

Partition build_allowed_terms(const Term& label,
                              int q_previous_layer,
                              PartitionCache& pcache) {
  // ---------------------------------------------------------
  // LEVEL 1: renamed cache, exact match
  // ---------------------------------------------------------
  TermQ renamed_key{label, q_previous_layer};
  auto renamed_it = pcache.renamed.find(renamed_key);

  if (renamed_it != pcache.renamed.end())
    return renamed_it->second;

  // Find the equivalence between label and the ones needed for the
  // reduced partitions list
  const TermEquivalence equivalence = summarize_label_equivalence(label);

  // ---------------------------------------------------------
  // LEVEL 2: filtered cache (filtered structural matches)
  // ---------------------------------------------------------
  TermQ filter_key{equivalence.signature, q_previous_layer};
  auto filtered_it = pcache.filtered.find(filter_key);

  if (filtered_it == pcache.filtered.end()) {
    // ---------------------------------------------------------
    // LEVEL 3: signature cache (structural matches only)
    // ---------------------------------------------------------
    const Partition* sig_parts_ptr = nullptr;
    auto sig_it = pcache.signature.find(equivalence.signature);

    if (sig_it != pcache.signature.end()) {
      sig_parts_ptr = &(sig_it->second);
    } else { // Generate base partitions
      std::multiset<int> mset(equivalence.signature.begin(), equivalence.signature.end());
      Partition sig_parts;
      auto partitions = multiset_partitions(mset);
      for (auto it = partitions.begin(); it != partitions.end(); ++it)
        sig_parts.push_back(*it);
      auto inserted_sig = pcache.signature.emplace(equivalence.signature, std::move(sig_parts));
      sig_parts_ptr = &(inserted_sig.first->second);
    }

    // Filtering
    Partition filtered_terms;
    for (const Terms& partition : *sig_parts_ptr) {
      bool allowed = true;
      for (const Term& part : partition) {
        if (static_cast<int>(part.size()) > q_previous_layer) {
          allowed = false;
          break;
        }
      }
      if (allowed) filtered_terms.push_back(partition);
    }

    // Save and grab the iterator
    filtered_it = pcache.filtered.emplace(filter_key, std::move(filtered_terms)).first;
  }

  // Renaming
  Partition renamed_terms;
  for (const Terms& partition : filtered_it->second) {
    Terms renamed_partition;
    renamed_partition.reserve(partition.size());

    for (const Term& part : partition) {
      Term renamed_part;
      renamed_part.reserve(part.size());
      for (int rank : part)
        renamed_part.push_back(equivalence.canonical_order[rank - 1]);
      std::sort(renamed_part.begin(), renamed_part.end());
      renamed_partition.push_back(std::move(renamed_part));
    }

    std::sort(renamed_partition.begin(), renamed_partition.end());
    renamed_terms.push_back(std::move(renamed_partition));
  }

  auto inserted_renamed = pcache.renamed.emplace(renamed_key, std::move(renamed_terms));
  return inserted_renamed.first->second;
}
