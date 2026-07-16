# nn2poly 1.0.0

- Implement print method for nn2poly objects.
- Add missing registration for internal S3 methods.
- New optimized algorithm in C++(#77, #79, #81 addressing #8, #9, #15, #47, #50):
  - New integrated way to generate, filter, rename and count partitions.
  - New transparent 3-level cache for partitions.
  - New comprehensive debugging system.
  - Several other optimizations.
  - Fixes a long-standing bug in label-to-column matching for indexing
    which caused wrong results in some high-order monomials.
  - Rename class for constrained NN objects to avoid method collision.
- Fix order propagation across layers (#80 addressing #78).

# nn2poly 0.1.3

- Add URLs for source repo and BugReports in DESCRIPTION file.
- Drop C++14 specification, as requested by CRAN.

# nn2poly 0.1.2

- Solve documentation problem with empty list item (#70).

# nn2poly 0.1.1

- Solve test rounding precision problem due to new architecture of mac OS (#69).

# nn2poly 0.1.0

- Initial release of the package.
