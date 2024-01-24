## Resubmission

1. Examples with unexported functions have been removed.
2. Examples using `\dontrun` are required as they use `keras` and `torch`, which require an interactive setup.

## Test environments

- macOS-latest, windows-latest, ubuntu-latest (on GA), R devel, release, oldrel
- win-builder, R devel

## R CMD check results

There is one NOTE:

> checking C++ specification ... NOTE
    Specified C++14: please drop specification unless essential

which is essential.
