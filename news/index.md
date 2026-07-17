# Changelog

## nn2poly 1.0.0

CRAN release: 2026-07-17

- Implement print method for nn2poly objects.
- Add missing registration for internal S3 methods.
- New optimized algorithm in
  C++([\#77](https://github.com/IBiDat/nn2poly/issues/77),
  [\#79](https://github.com/IBiDat/nn2poly/issues/79),
  [\#81](https://github.com/IBiDat/nn2poly/issues/81) addressing
  [\#8](https://github.com/IBiDat/nn2poly/issues/8),
  [\#9](https://github.com/IBiDat/nn2poly/issues/9),
  [\#15](https://github.com/IBiDat/nn2poly/issues/15),
  [\#47](https://github.com/IBiDat/nn2poly/issues/47),
  [\#50](https://github.com/IBiDat/nn2poly/issues/50)):
  - New integrated way to generate, filter, rename and count partitions.
  - New transparent 3-level cache for partitions.
  - New comprehensive debugging system.
  - Several other optimizations.
  - Fixes a long-standing bug in label-to-column matching for indexing
    which caused wrong results in some high-order monomials.
  - Rename class for constrained NN objects to avoid method collision.
- Fix order propagation across layers
  ([\#80](https://github.com/IBiDat/nn2poly/issues/80) addressing
  [\#78](https://github.com/IBiDat/nn2poly/issues/78)).

## nn2poly 0.1.3

CRAN release: 2025-12-12

- Add URLs for source repo and BugReports in DESCRIPTION file.
- Drop C++14 specification, as requested by CRAN.

## nn2poly 0.1.2

CRAN release: 2024-11-11

- Solve documentation problem with empty list item
  ([\#70](https://github.com/IBiDat/nn2poly/issues/70)).

## nn2poly 0.1.1

CRAN release: 2024-01-30

- Solve test rounding precision problem due to new architecture of mac
  OS ([\#69](https://github.com/IBiDat/nn2poly/issues/69)).

## nn2poly 0.1.0

CRAN release: 2024-01-24

- Initial release of the package.
