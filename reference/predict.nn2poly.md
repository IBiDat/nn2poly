# Predict method for `nn2poly` objects.

Predicted values obtained with a `nn2poly` object on given data.

## Usage

``` r
# S3 method for class 'nn2poly'
predict(object, newdata, monomials = FALSE, layers = NULL, ...)
```

## Arguments

- object:

  Object of class inheriting from 'nn2poly'.

- newdata:

  Input data as matrix, vector or dataframe. Number of columns (or
  elements in vector) should be the number of variables in the
  polynomial (dimension p). Response variable to be predicted should not
  be included.

- monomials:

  Boolean determining if the returned item should contain the
  evaluations of all the monomials of the provided polynomials
  (`monomials==TRUE`), or if the final polynomial evaluation should be
  computed, i.e., adding up all the monomials (`monomials==FALSE`).
  Defaults to `FALSE`.

- layers:

  Vector containing the chosen layers from `object` to be evaluated. If
  set to `NULL`, all layers are computed. Default is set to `NULL`.

- ...:

  Further arguments passed to or from other methods.

## Value

Returns a matrix or list of matrices with the evaluation of each
polynomial at each layer as given by the provided `object` of class
`nn2poly`. The format can be as follows, depending on the layers
contained in `object` and the parameters `layers` and `monomials`
values:

- If `object` contains the polynomials of the last layer, as given by
  `nn2poly(object, keep_layers = FALSE)`, then the output is:

  - A matrix: if `monomials==FALSE`, returns a matrix containing the
    evaluation of the polynomials on the given data. The matrix has
    dimensions `(n_sample, n_polynomials)`, meaning that each column
    corresponds to the result of evaluating all the data for a
    polynomial. If a single polynomial is provided, the output is a
    vector instead of a row matrix.

  - A 3D array: If `monomials==TRUE`, returns a 3D array containing the
    monomials of each polynomial evaluated on the given data. The array
    has dimensions `(n_sample, n_monomial_terms, n_polynomials)`, where
    element `[i,j,k]` contains the evaluation on observation `i` on
    monomial `j` of polynomial `k`, where monomial `j` corresponds to
    the one on `poly$labels[[j]]`.

- If `object` contains all the internal polynomials, as given by
  `nn2poly(object, keep_layers = TRUE)`, then the output is a list of
  layers (represented by `layer_i`), where each of them is another list
  with `input` and `output` elements. Each of those elements contains
  the corresponding evaluation of the "input" or "output" polynomial at
  the given layer, as explained in the last layer case, which will be a
  matrix if `monomials==FALSE` and a 3D array if `monomials==TRUE`.

## Details

Internally uses
[`eval_poly()`](https://ibidat.github.io/nn2poly/reference/eval_poly.md)
to obtain the predictions. However, this only works with a objects of
class `nn2poly` while
[`eval_poly()`](https://ibidat.github.io/nn2poly/reference/eval_poly.md)
can be used with a manually created polynomial in list form.

When `object` contains all the internal polynomials also, as given by
`nn2poly(object, keep_layers = TRUE)`, it is important to note that
there are two polynomial items per layer (input/output). These
polynomial items will also contain several polynomials of the same
structure, one per neuron in the layer, stored as matrix rows in
`$values`. Please see the NN2Poly original paper for more details.

Note also that "linear" layers will contain the same input and output
results as Taylor expansion is not used and thus the polynomials are
also the same. Because of this, in the situation of evaluating multiple
layers we provide the final layer with "input" and "output" even if they
are the same, for consistency.

## See also

[`nn2poly()`](https://ibidat.github.io/nn2poly/reference/nn2poly.md):
function that obtains the `nn2poly` polynomial object,
[`eval_poly()`](https://ibidat.github.io/nn2poly/reference/eval_poly.md):
function that can evaluate polynomials in general,
[`stats::predict()`](https://rdrr.io/r/stats/predict.html): generic
predict function.

## Examples

``` r
# Build a NN structure with random weights, with 2 (+ bias) inputs,
# 4 (+bias) neurons in the first hidden layer with "tanh" activation
# function, 4 (+bias) neurons in the second hidden layer with "softplus",
# and 1 "linear" output unit

weights_layer_1 <- matrix(rnorm(12), nrow = 3, ncol = 4)
weights_layer_2 <- matrix(rnorm(20), nrow = 5, ncol = 4)
weights_layer_3 <- matrix(rnorm(5), nrow = 5, ncol = 1)

# Set it as a list with activation functions as names
nn_object = list("tanh" = weights_layer_1,
                 "softplus" = weights_layer_2,
                 "linear" = weights_layer_3)

# Obtain the polynomial representation (order = 3) of that neural network
final_poly <- nn2poly(nn_object, max_order = 3)

# Define some new data, it can be vector, matrix or dataframe
newdata <- matrix(rnorm(10), ncol = 2, nrow = 5)

# Predict using the obtained polynomial
predict(object = final_poly, newdata = newdata)
#> [1] -6.7032102  3.2858822  2.3306972  3.5507715  0.6646975

# Predict the values of each monomial of the obtained polynomial
predict(object = final_poly, newdata = newdata, monomials = TRUE)
#> , , 1
#> 
#>          [,1]       [,2]       [,3]       [,4]       [,5]      [,6]        [,7]
#> [1,] 1.479247 -1.6819978  1.8211215 0.19780157 -1.8814399 1.0167186  0.31878703
#> [2,] 1.479247 -2.2666642 -0.8576864 0.35921403  1.1941029 0.2255174  0.78016413
#> [3,] 1.479247  1.0368867 -0.9440221 0.07516948 -0.6012284 0.2732041 -0.07468242
#> [4,] 1.479247  2.7904914 -1.3794270 0.54442780 -2.3643155 0.5833382 -1.45568190
#> [5,] 1.479247  0.6687121  1.2564238 0.03126492  0.5160613 0.4839441 -0.02003281
#>             [,8]       [,9]     [,10]
#> [1,] -0.71223928  2.4896885 -9.750898
#> [2,]  0.60917083  0.7441938  1.018623
#> [3,]  0.14030753 -0.4124178  1.358233
#> [4,]  1.48489686 -2.3698448  4.237639
#> [5,] -0.07766953 -0.4711435 -3.202110
#> 

# Change the last layer to have 3 outputs (as in a multiclass classification)
# problem
weights_layer_4 <- matrix(rnorm(20), nrow = 5, ncol = 4)

# Set it as a list with activation functions as names
nn_object = list("tanh" = weights_layer_1,
                 "softplus" = weights_layer_2,
                 "linear" = weights_layer_4)

# Obtain the polynomial representation of that neural network
# Polynomial representation of each hidden neuron is given by
final_poly <- nn2poly(nn_object, max_order = 3, keep_layers = TRUE)

# Define some new data, it can be vector, matrix or dataframe
newdata <- matrix(rnorm(10), ncol = 2, nrow = 5)

# Predict using the obtained polynomials (for all layers)
predict(object = final_poly, newdata = newdata)
#> $layer_1
#> $layer_1$input
#>            [,1]       [,2]       [,3]       [,4]
#> [1,] -2.2982951  6.8322228  1.8701077 -6.3762748
#> [2,] -0.3059160 -2.3812127 -0.6939258  0.6545819
#> [3,] -1.2929450  0.1069224 -0.7602996 -1.1709467
#> [4,] -0.8006397  1.8367722  1.1853521 -2.6322438
#> [5,] -2.2192979 -0.8395190 -2.9351919 -0.2643359
#> 
#> $layer_1$output
#>            [,1]         [,2]       [,3]        [,4]
#> [1,]  0.8320532 -88.12332320  0.4290622 270.1761709
#> [2,] -0.2967167   1.71992070 -0.5481295  -5.8762805
#> [3,] -0.7054090   0.10688153 -0.5883721  -0.7664119
#> [4,] -0.6529002   0.02112404  0.7837662   7.6435132
#> [5,]  0.6067677  -0.65571760  2.4048928  -0.8362815
#> 
#> 
#> $layer_2
#> $layer_2$input
#>              [,1]         [,2]       [,3]       [,4]
#> [1,] -160.1210128 209.57661155 -12.299216 20.3150971
#> [2,]    5.5182461  -3.48719383   1.601213 -2.0247809
#> [3,]    2.3403119   0.18831304   2.224970 -1.3505012
#> [4,]   -3.9020012   6.31525170   3.833259 -3.2000409
#> [5,]   -0.5570712  -0.07324635  -1.925328 -0.4123114
#> 
#> $layer_2$output
#>             [,1]       [,2]       [,3]       [,4]
#> [1,] -117.508717 97.1775056 20.0709077 30.3422783
#> [2,]    4.613801 -0.8338783  1.3448880 -0.3880567
#> [3,]    2.504353  0.7564453  2.4754596  0.2005291
#> [4,]   -2.301199  3.6862128  4.4501375  0.4786586
#> [5,]    2.353666  0.2694684  0.3571213 -0.2442615
#> 
#> 
#> $layer_3
#> $layer_3$input
#>             [,1]        [,2]      [,3]          [,4]
#> [1,] -164.937236 -203.586057 83.240964 173.594296320
#> [2,]   12.915244    5.776361  3.113188  -2.493126875
#> [3,]   11.990104    1.502841  5.449384   0.335906793
#> [4,]    7.319824   -6.847628  8.507151   6.169536098
#> [5,]    5.860170    3.528252  2.281727   0.009246967
#> 
#> $layer_3$output
#>             [,1]        [,2]      [,3]          [,4]
#> [1,] -164.937236 -203.586057 83.240964 173.594296320
#> [2,]   12.915244    5.776361  3.113188  -2.493126875
#> [3,]   11.990104    1.502841  5.449384   0.335906793
#> [4,]    7.319824   -6.847628  8.507151   6.169536098
#> [5,]    5.860170    3.528252  2.281727   0.009246967
#> 
#> 

# Predict using the obtained polynomials (for chosen layers)
predict(object = final_poly, newdata = newdata, layers = c(2,3))
#> $layer_2
#> $layer_2$input
#>              [,1]         [,2]       [,3]       [,4]
#> [1,] -160.1210128 209.57661155 -12.299216 20.3150971
#> [2,]    5.5182461  -3.48719383   1.601213 -2.0247809
#> [3,]    2.3403119   0.18831304   2.224970 -1.3505012
#> [4,]   -3.9020012   6.31525170   3.833259 -3.2000409
#> [5,]   -0.5570712  -0.07324635  -1.925328 -0.4123114
#> 
#> $layer_2$output
#>             [,1]       [,2]       [,3]       [,4]
#> [1,] -117.508717 97.1775056 20.0709077 30.3422783
#> [2,]    4.613801 -0.8338783  1.3448880 -0.3880567
#> [3,]    2.504353  0.7564453  2.4754596  0.2005291
#> [4,]   -2.301199  3.6862128  4.4501375  0.4786586
#> [5,]    2.353666  0.2694684  0.3571213 -0.2442615
#> 
#> 
#> $layer_3
#> $layer_3$input
#>             [,1]        [,2]      [,3]          [,4]
#> [1,] -164.937236 -203.586057 83.240964 173.594296320
#> [2,]   12.915244    5.776361  3.113188  -2.493126875
#> [3,]   11.990104    1.502841  5.449384   0.335906793
#> [4,]    7.319824   -6.847628  8.507151   6.169536098
#> [5,]    5.860170    3.528252  2.281727   0.009246967
#> 
#> $layer_3$output
#>             [,1]        [,2]      [,3]          [,4]
#> [1,] -164.937236 -203.586057 83.240964 173.594296320
#> [2,]   12.915244    5.776361  3.113188  -2.493126875
#> [3,]   11.990104    1.502841  5.449384   0.335906793
#> [4,]    7.319824   -6.847628  8.507151   6.169536098
#> [5,]    5.860170    3.528252  2.281727   0.009246967
#> 
#> 
```
