## Patch release

- Solve test rounding precision problem due to new architechture of mac OS.

## Test environments

- macOS-latest, windows-latest, ubuntu-latest (on GA), R devel, release, oldrel
- win-builder, R devel

## R CMD check results

There is one NOTE:

> checking C++ specification ... NOTE
    Specified C++14: please drop specification unless essential

which is essential.
