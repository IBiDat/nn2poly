# Plot method for `nn2poly` objects.

A function that takes a polynomial (or several ones) as given by the
nn2poly algorithm, and then plots their absolute magnitude as barplots
to be able to compare the most important coefficients.

## Usage

``` r
# S3 method for class 'nn2poly'
plot(x, ..., n = NULL)
```

## Arguments

- x:

  A `nn2poly` object, as returned by the nn2poly algorithm.

- ...:

  Ignored.

- n:

  An integer denoting the number of coefficients to be plotted, after
  ordering them by absolute magnitude.

## Value

A plot showing the `n` most important coefficients.

## Details

The plot method represents only the polynomials at the final layer, even
if `x` is generated using
[`nn2poly()`](https://ibidat.github.io/nn2poly/reference/nn2poly.md)
with `keep_layers=TRUE`.

## Examples

``` r
# --- Single polynomial output ---
# Build a NN structure with random weights, with 2 (+ bias) inputs,
# 4 (+bias) neurons in the first hidden layer with "tanh" activation
# function, 4 (+bias) neurons in the second hidden layer with "softplus",
# and 2 "linear" output units

weights_layer_1 <- matrix(rnorm(12), nrow = 3, ncol = 4)
weights_layer_2 <- matrix(rnorm(20), nrow = 5, ncol = 4)
weights_layer_3 <- matrix(rnorm(5), nrow = 5, ncol = 1)

# Set it as a list with activation functions as names
nn_object = list("tanh" = weights_layer_1,
                 "softplus" = weights_layer_2,
                 "linear" = weights_layer_3)

# Obtain the polynomial representation (order = 3) of that neural network
final_poly <- nn2poly(nn_object, max_order = 3)

# Plot all the coefficients, one plot per output unit
plot(final_poly)


# Plot only the 5 most important coeffcients (by absolute magnitude)
# one plot per output unit
plot(final_poly, n = 5)


# --- Multiple output polynomials ---
# Build a NN structure with random weights, with 2 (+ bias) inputs,
# 4 (+bias) neurons in the first hidden layer with "tanh" activation
# function, 4 (+bias) neurons in the second hidden layer with "softplus",
# and 2 "linear" output units

weights_layer_1 <- matrix(rnorm(12), nrow = 3, ncol = 4)
weights_layer_2 <- matrix(rnorm(20), nrow = 5, ncol = 4)
weights_layer_3 <- matrix(rnorm(10), nrow = 5, ncol = 2)

# Set it as a list with activation functions as names
nn_object = list("tanh" = weights_layer_1,
                 "softplus" = weights_layer_2,
                 "linear" = weights_layer_3)

# Obtain the polynomial representation (order = 3) of that neural network
final_poly <- nn2poly(nn_object, max_order = 3)

# Plot all the coefficients, one plot per output unit
plot(final_poly)


# Plot only the 5 most important coeffcients (by absolute magnitude)
# one plot per output unit
plot(final_poly, n = 5)

```
