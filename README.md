# nn2poly: Transforming Neural Networks into Polynomials

<!-- badges: start -->
[![Build\_Status](https://github.com/IBiDat/nn2poly/actions/workflows/build.yml/badge.svg)](https://github.com/IBiDat/nn2poly/actions/workflows/build.yml)
[![Coverage\_Status](https://img.shields.io/codecov/c/github/IBiDat/nn2poly/master.svg)](https://app.codecov.io/github/IBiDat/nn2poly?branch=master)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/nn2poly)](https://cran.r-project.org/package=nn2poly)
[![DOI](https://img.shields.io/badge/doi-10.1016/j.neunet.2021.04.036-informational.svg)](https://doi.org/10.1016/j.neunet.2021.04.036)
[![DOI](https://img.shields.io/badge/doi-10.1109/TNNLS.2023.3330328-informational.svg)](https://doi.org/10.1109/TNNLS.2023.3330328)
<!-- badges: end -->

The **nn2poly** package implements the NN2Poly method that allows to transform an already trained deep feed-forward fully connected neural network into a polynomial representation that predicts as similar as possible to the original neural network.


## Related Papers:

- Pablo Morala, J. Alexandra Cifuentes, Rosa E. Lillo, Iñaki Ucar (2021).
  "Towards a mathematical framework to inform neural network modelling via polynomial regression."
  _Neural Networks_, *142*, 57-72.
  doi:[10.1016/j.neunet.2021.04.036](https://doi.org/10.1016/j.neunet.2021.04.036)

- Pablo Morala, J. Alexandra Cifuentes, Rosa E. Lillo, Iñaki Ucar (2023).
  "NNN2Poly: A Polynomial Representation for Deep Feed-Forward Artificial Neural Networks."
  _IEEE Transactions on Neural Networks and Learning Systems_, (Early Access).
  doi:[10.1109/TNNLS.2023.3330328](https://doi.org/10.1109/TNNLS.2023.3330328)


## Installation

The installation from GitHub requires the [remotes](https://cran.r-project.org/package=remotes) package.

```r
# install.packages("remotes")
remotes::install_github("IBiDat/nn2poly")
```

