#ifndef nn2poly__partitions_h
#define nn2poly__partitions_h

#include "nn2poly_types.h"

Terms combinations_with_repetition(int n, int k);
const PartitionCounts& build_partition_counts(const Term& label, int q,
                                              const TermMap& map,
                                              PartitionCache& pcache);

#endif
