# nn2poly: Transforming Neural Networks into Polynomials

<!-- badges: start -->
[![Build\_Status](https://github.com/IBiDat/nn2poly/actions/workflows/build.yml/badge.svg)](https://github.com/IBiDat/nn2poly/actions/workflows/build.yml)
[![Coverage\_Status](https://img.shields.io/codecov/c/github/IBiDat/nn2poly/master.svg)](https://app.codecov.io/github/IBiDat/nn2poly?branch=master)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/nn2poly)](https://cran.r-project.org/package=nn2poly)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/nn2poly)](https://cran.r-project.org/package=nn2poly)
[![DOI](https://img.shields.io/badge/doi-10.1016/j.neunet.2021.04.036-informational.svg)](https://doi.org/10.1016/j.neunet.2021.04.036)
[![DOI](https://img.shields.io/badge/doi-10.1109/TNNLS.2023.3330328-informational.svg)](https://doi.org/10.1109/TNNLS.2023.3330328)
<!-- badges: end -->

The **nn2poly** package implements the NN2Poly method that allows to transform an already trained deep feed-forward fully connected neural network into a polynomial representation that predicts as similar as possible to the original neural network. The obtained polynomial coefficients can be used to explain features (and their interactions) importance  in the neural network, therefore working as a tool for interpretability or eXplainable Artificial Intelligence (XAI).


## Related Papers:

- Pablo Morala, J. Alexandra Cifuentes, Rosa E. Lillo, Iñaki Ucar (2021).
  "Towards a mathematical framework to inform neural network modelling via polynomial regression."
  _Neural Networks_, *142*, 57-72.
  doi: [10.1016/j.neunet.2021.04.036](https://doi.org/10.1016/j.neunet.2021.04.036)

- Pablo Morala, J. Alexandra Cifuentes, Rosa E. Lillo, Iñaki Ucar (2023).
  "NN2Poly: A Polynomial Representation for Deep Feed-Forward Artificial Neural Networks."
  _IEEE Transactions on Neural Networks and Learning Systems_, (Early Access).
  doi: [10.1109/TNNLS.2023.3330328](https://doi.org/10.1109/TNNLS.2023.3330328)


## Installation

The latest release version available in CRAN can be installed as:

```r
install.packages("nn2poly")
```

The installation from GitHub requires the [remotes](https://cran.r-project.org/package=remotes) package.

```r
# install.packages("remotes")
remotes::install_github("IBiDat/nn2poly")
```

## Funding

This package is part of the project/grant PDC2022-133359-I00 funded by MCIN/AEI/10.13039/501100011033 and by the European Union “NextGenerationEU/PRTR”.

![Funding](man/figures/Logo-Funding.png)

