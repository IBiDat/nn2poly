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
#> [1] 206050.00  34324.06 -18190.28  58432.11 187762.89

# Predict the values of each monomial of the obtained polynomial
predict(object = final_poly, newdata = newdata, monomials = TRUE)
#> , , 1
#> 
#>           [,1]      [,2]       [,3]     [,4]       [,5]      [,6]       [,7]
#> [1,] 0.9445806 113.37604  31.547263 4371.048  2634.5865  381.9276  89705.232
#> [2,] 0.9445806 -53.39627 113.458709  969.538 -4462.5001 4940.0731  -9371.015
#> [3,] 0.9445806 -58.77120  -8.773393 1174.551   379.8058   29.5388 -12495.322
#> [4,] 0.9445806 -85.87784 145.980112 2507.871 -9234.3064 8177.9620 -38984.969
#> [5,] 0.9445806  78.22013  60.454785 2080.559  3483.2021 1402.5523  29458.417
#>            [,8]         [,9]        [,10]
#> [1,]  83591.611   22765.1651   2454.55950
#> [2,]  66683.455 -138679.6496 114183.15279
#> [3,]  -6246.764    -912.6954    -52.79482
#> [4,] 221929.132 -369228.0011 243203.37293
#> [5,]  76247.589   57677.4446  17273.50833
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
#>            [,1]       [,2]       [,3]      [,4]
#> [1,]  4.4380289 -1.3652709  2.7836868 -3.194937
#> [2,]  1.0660953  2.6519031 -3.2447299  1.392775
#> [3,]  2.1627470  1.9906083 -1.7915246  0.198094
#> [4,]  2.8862858  0.6550504 -0.1255387 -1.004590
#> [5,] -0.5204576  1.9970351 -4.0797186  2.378368
#> 
#> $layer_1$output
#>             [,1]        [,2]        [,3]       [,4]
#> [1,] -468.216085  46.9786089 -474.783280  7.2617095
#> [2,]    4.374436 -13.9715713   57.926812  0.5339210
#> [3,]   -7.700099  -1.0218898    2.448094  0.1586882
#> [4,]  -50.501736  -0.3808232  -13.340427 -0.6445344
#> [5,]  149.863922  26.2053943  204.802969 -2.7994892
#> 
#> 
#> $layer_2
#> $layer_2$input
#>             [,1]       [,2]       [,3]        [,4]
#> [1,] -139.552574 1083.04810 139.324903 -593.375713
#> [2,]  -16.746926  -38.34357  31.503905   87.795806
#> [3,]   -4.953262   13.76396   6.281792    5.568672
#> [4,]  -28.936616  100.35774  33.782313  -11.484938
#> [5,]   28.442274 -323.45468 -74.335968  217.117024
#> 
#> $layer_2$output
#>              [,1]        [,2]        [,3]        [,4]
#> [1,]  531.9983452 -653210.693  524.200207 -808.814815
#> [2,]  -26.0148479   18689.896   -8.997529   83.093760
#> [3,]   -0.5508578   -1535.297    5.173041    4.004517
#> [4,]   22.6631399  -40274.070   37.868319  -20.219724
#> [5,] -344.0581903  454573.689 -351.561418  201.621087
#> 
#> 
#> $layer_3
#> $layer_3$input
#>             [,1]         [,2]       [,3]        [,4]
#> [1,]  259961.300 -246267.9132 -888712.73  718927.356
#> [2,]   -7488.705    7127.9330   25416.69  -20700.947
#> [3,]     603.462    -565.4866   -2089.32    1672.516
#> [4,]   15996.008  -15140.3597  -54797.61   44258.211
#> [5,] -180679.245  170952.6345  618548.31 -499571.697
#> 
#> $layer_3$output
#>             [,1]         [,2]       [,3]        [,4]
#> [1,]  259961.300 -246267.9132 -888712.73  718927.356
#> [2,]   -7488.705    7127.9330   25416.69  -20700.947
#> [3,]     603.462    -565.4866   -2089.32    1672.516
#> [4,]   15996.008  -15140.3597  -54797.61   44258.211
#> [5,] -180679.245  170952.6345  618548.31 -499571.697
#> 
#> 

# Predict using the obtained polynomials (for chosen layers)
predict(object = final_poly, newdata = newdata, layers = c(2,3))
#> $layer_2
#> $layer_2$input
#>             [,1]       [,2]       [,3]        [,4]
#> [1,] -139.552574 1083.04810 139.324903 -593.375713
#> [2,]  -16.746926  -38.34357  31.503905   87.795806
#> [3,]   -4.953262   13.76396   6.281792    5.568672
#> [4,]  -28.936616  100.35774  33.782313  -11.484938
#> [5,]   28.442274 -323.45468 -74.335968  217.117024
#> 
#> $layer_2$output
#>              [,1]        [,2]        [,3]        [,4]
#> [1,]  531.9983452 -653210.693  524.200207 -808.814815
#> [2,]  -26.0148479   18689.896   -8.997529   83.093760
#> [3,]   -0.5508578   -1535.297    5.173041    4.004517
#> [4,]   22.6631399  -40274.070   37.868319  -20.219724
#> [5,] -344.0581903  454573.689 -351.561418  201.621087
#> 
#> 
#> $layer_3
#> $layer_3$input
#>             [,1]         [,2]       [,3]        [,4]
#> [1,]  259961.300 -246267.9132 -888712.73  718927.356
#> [2,]   -7488.705    7127.9330   25416.69  -20700.947
#> [3,]     603.462    -565.4866   -2089.32    1672.516
#> [4,]   15996.008  -15140.3597  -54797.61   44258.211
#> [5,] -180679.245  170952.6345  618548.31 -499571.697
#> 
#> $layer_3$output
#>             [,1]         [,2]       [,3]        [,4]
#> [1,]  259961.300 -246267.9132 -888712.73  718927.356
#> [2,]   -7488.705    7127.9330   25416.69  -20700.947
#> [3,]     603.462    -565.4866   -2089.32    1672.516
#> [4,]   15996.008  -15140.3597  -54797.61   44258.211
#> [5,] -180679.245  170952.6345  618548.31 -499571.697
#> 
#> 
```
