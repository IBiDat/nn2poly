# Build a `luz` model composed of a linear stack of layers

Helper function to build `luz` models as a sequential model, by feeding
it a stack of `luz` layers.

## Usage

``` r
luz_model_sequential(...)
```

## Arguments

- ...:

  Sequence of modules to be added.

## Value

A `nn_sequential` module.

## Details

This step is needed so we can get the activation functions and layers
and neurons architecture easily with `nn2poly:::get_parameters()`.
Furthermore, this step is also needed to be able to impose the needed
constraints when using the `luz/torch` framework.

## See also

[`add_constraints()`](https://ibidat.github.io/nn2poly/reference/add_constraints.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("luz", quietly=TRUE)) {
# Create a NN using luz/torch as a sequential model
# with 3 fully connected linear layers,
# the first one with input = 5 variables,
# 100 neurons and tanh activation function, the second
# one with 50 neurons and softplus activation function
# and the last one with 1 linear output.
nn <- luz_model_sequential(
  torch::nn_linear(5,100),
  torch::nn_tanh(),
  torch::nn_linear(100,50),
  torch::nn_softplus(),
  torch::nn_linear(50,1)
)

nn

# Check that the nn is of class nn_squential
class(nn)
}
} # }
```
