# nn2poly: Transforming Neural Networks into Polynomials

The **nn2poly** package implements the NN2Poly method that allows to transform an already trained deep feed-forward fully connected neural network into a polynomial representation that predicts as similar as possible to the original neural network.

## Documentation

Vignettes:
  
- [Introduction]
- [Constraining the weights]

## Related Papers:

- Pablo Morala, Jenny Alexandra Cifuentes, Rosa E. Lillo and Iñaki Ucar (2021). "Towards a mathematical framework to inform neural network modelling via polynomial regression." _Neural Networks_ (ISSN 0893-6080), vol. 142, 57-72. DOI: [10.1016/j.neunet.2021.04.036](https://doi.org/10.1016/j.neunet.2021.04.036)

- Pablo Morala, Jenny Alexandra Cifuentes, Rosa E. Lillo and Iñaki Ucar (2021). "NN2Poly: A polynomial representation for deep feed-forward artificial neural networks". Arxiv preprint: [arXiv:2112.11397](https://arxiv.org/abs/2112.11397)



## Installation


The installation from GitHub requires the [remotes](https://cran.r-project.org/package=remotes) package.

```r
# install.packages("remotes")
remotes::install_github(paste("IBiDat", c("nn2poly", "nn2poly.tools"), sep="/"))
```
