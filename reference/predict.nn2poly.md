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
#> [1]   9.351224 -25.505057  34.719666   6.247844  91.053817

# Predict the values of each monomial of the obtained polynomial
predict(object = final_poly, newdata = newdata, monomials = TRUE)
#> , , 1
#> 
#>           [,1]       [,2]       [,3]       [,4]        [,5]        [,6]
#> [1,] 0.5998362  0.1956268  3.6839954 0.05099631  -1.3684534 -0.97273007
#> [2,] 0.5998362 -1.4157464  1.3379416 2.67087139   3.5967084 -0.12830055
#> [3,] 0.5998362  0.6667685  4.8118638 0.59242342  -6.0921559 -1.65951371
#> [4,] 0.5998362  0.7338862 -0.3720858 0.71769420   0.5185067 -0.00992294
#> [5,] 0.5998362  1.0723715  6.1911194 1.53240156 -12.6065731 -2.74721440
#>              [,7]        [,8]       [,9]        [,10]
#> [1,]   0.07381407  0.01091141  4.7237937  2.353433367
#> [2,] -27.97760627  0.20754546 -4.5090416  0.112734331
#> [3,]   2.92266751  0.16556503 27.4679454  5.244265374
#> [4,]   3.89708831 -0.01550978  0.1807754 -0.002424789
#> [5,]  12.15877904  0.55101681 73.1321043 11.169975574
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
#>            [,1]        [,2]       [,3]        [,4]
#> [1,] -5.6735417 -0.32513524  1.6814923  1.62302779
#> [2,]  4.5589447  3.87422294 -2.6519402  0.13562721
#> [3,] -2.9341045 -0.62868026  2.3703111  0.20284204
#> [4,] -1.0384512 -0.09566662  1.8847146 -0.24804201
#> [5,]  0.9666878  1.55422076 -0.0353892  0.05242729
#> 
#> $layer_1$output
#>              [,1]        [,2]        [,3]        [,4]
#> [1,]  48.20486265 -27.7250526  8.59200940  0.39427109
#> [2,] -31.65159736 -17.0734437 -4.00627030  0.19575252
#> [3,]   3.36625783   1.5250084 -0.07809946  0.19920248
#> [4,]  -0.60308233  -0.3957372  0.79671108 -0.27897526
#> [5,]  -0.07149144   1.6906254 -0.36895992  0.06026532
#> 
#> 
#> $layer_2
#> $layer_2$input
#>             [,1]        [,2]        [,3]         [,4]
#> [1,]  31.5809235 100.1570991 -73.7892050  19.65320413
#> [2,] -45.6669367   3.9530106  75.0068291 -10.76281414
#> [3,]   3.9445754   0.4584441  -6.8056997   1.62897865
#> [4,]  -1.3308726   1.8814099   0.6507434  -1.68789840
#> [5,]   0.4767643  -2.8508475  -0.5016165  -0.06090673
#> 
#> $layer_2$output
#>            [,1]       [,2]        [,3]      [,4]
#> [1,]  2.4586391 27.6727464 -62.9655489 7.9150715
#> [2,] -2.6859591 -5.2350994  60.5183983 7.4428759
#> [3,]  1.6125714  2.4137782  -3.6892159 0.3460346
#> [4,]  0.2265399  1.9037634   0.8811226 0.1730364
#> [5,]  1.2921931 -0.1571784   1.4721249 0.6753352
#> 
#> 
#> $layer_3
#> $layer_3$input
#>            [,1]       [,2]       [,3]        [,4]
#> [1,]  53.452427 -7.2556396 -95.358988  59.6811039
#> [2,] -38.043966 25.2478182  83.650617 -65.2359662
#> [3,]   6.797121  1.5914101  -3.851680  -0.2259946
#> [4,]   2.968127  0.6026495   1.748960  -1.7792371
#> [5,]   1.221525  3.0026318   4.054609  -5.4887796
#> 
#> $layer_3$output
#>            [,1]       [,2]       [,3]        [,4]
#> [1,]  53.452427 -7.2556396 -95.358988  59.6811039
#> [2,] -38.043966 25.2478182  83.650617 -65.2359662
#> [3,]   6.797121  1.5914101  -3.851680  -0.2259946
#> [4,]   2.968127  0.6026495   1.748960  -1.7792371
#> [5,]   1.221525  3.0026318   4.054609  -5.4887796
#> 
#> 

# Predict using the obtained polynomials (for chosen layers)
predict(object = final_poly, newdata = newdata, layers = c(2,3))
#> $layer_2
#> $layer_2$input
#>             [,1]        [,2]        [,3]         [,4]
#> [1,]  31.5809235 100.1570991 -73.7892050  19.65320413
#> [2,] -45.6669367   3.9530106  75.0068291 -10.76281414
#> [3,]   3.9445754   0.4584441  -6.8056997   1.62897865
#> [4,]  -1.3308726   1.8814099   0.6507434  -1.68789840
#> [5,]   0.4767643  -2.8508475  -0.5016165  -0.06090673
#> 
#> $layer_2$output
#>            [,1]       [,2]        [,3]      [,4]
#> [1,]  2.4586391 27.6727464 -62.9655489 7.9150715
#> [2,] -2.6859591 -5.2350994  60.5183983 7.4428759
#> [3,]  1.6125714  2.4137782  -3.6892159 0.3460346
#> [4,]  0.2265399  1.9037634   0.8811226 0.1730364
#> [5,]  1.2921931 -0.1571784   1.4721249 0.6753352
#> 
#> 
#> $layer_3
#> $layer_3$input
#>            [,1]       [,2]       [,3]        [,4]
#> [1,]  53.452427 -7.2556396 -95.358988  59.6811039
#> [2,] -38.043966 25.2478182  83.650617 -65.2359662
#> [3,]   6.797121  1.5914101  -3.851680  -0.2259946
#> [4,]   2.968127  0.6026495   1.748960  -1.7792371
#> [5,]   1.221525  3.0026318   4.054609  -5.4887796
#> 
#> $layer_3$output
#>            [,1]       [,2]       [,3]        [,4]
#> [1,]  53.452427 -7.2556396 -95.358988  59.6811039
#> [2,] -38.043966 25.2478182  83.650617 -65.2359662
#> [3,]   6.797121  1.5914101  -3.851680  -0.2259946
#> [4,]   2.968127  0.6026495   1.748960  -1.7792371
#> [5,]   1.221525  3.0026318   4.054609  -5.4887796
#> 
#> 
```
