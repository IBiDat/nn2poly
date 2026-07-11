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
                              PartitionCache& partition_cache) {
  // Find the equivalence between label and the ones needed for the
  // reduced partitions list
  const TermEquivalence equivalence = summarize_label_equivalence(label);

  const Partition* sig_parts_ptr = nullptr;
  auto cache_it = partition_cache.find(equivalence.signature);
  if (cache_it != partition_cache.end()) {
    sig_parts_ptr = &(cache_it->second);
  } else {
    std::multiset<int> mset(equivalence.signature.begin(), equivalence.signature.end());
    Partition sig_parts;
    auto partitions = multiset_partitions(mset);
    for (auto it = partitions.begin(); it != partitions.end(); ++it)
      sig_parts.push_back(*it);
    auto inserted = partition_cache.emplace(equivalence.signature, std::move(sig_parts));
    sig_parts_ptr = &(inserted.first->second);
  }

  // Filter and rename cached partitions on the fly
  Partition allowed_terms;
  for (const Terms& partition : *sig_parts_ptr) {
    bool allowed = true;
    Terms renamed_partition;
    renamed_partition.reserve(partition.size());
    for (const Term& part : partition) {
      if (static_cast<int>(part.size()) > q_previous_layer) {
        allowed = false;
        break;
      }
      Term renamed_part;
      renamed_part.reserve(part.size());
      for (int rank : part)
        renamed_part.push_back(equivalence.canonical_order[rank - 1]);
      std::sort(renamed_part.begin(), renamed_part.end());
      renamed_partition.push_back(std::move(renamed_part));
    }
    if (allowed) {
      std::sort(renamed_partition.begin(), renamed_partition.end());
      allowed_terms.push_back(std::move(renamed_partition));
    }
  }

  return allowed_terms;
}
