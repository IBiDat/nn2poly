# Plots activation potentials and Taylor expansion.

Function that allows to take a NN and the data input values and plot the
distribution of data activation potentials (sum of input values \*
weights) at all neurons together at each layer with the Taylor expansion
used in the activation functions. If any layer is `'linear'` (usually
will be the output), then that layer will not be an approximation as
Taylor expansion is not needed.

## Usage

``` r
plot_taylor_and_activation_potentials(
  object,
  data,
  max_order,
  taylor_orders = 8,
  constraints,
  taylor_interval = 1.5,
  ...
)
```

## Arguments

- object:

  An object for which the computation of the NN2Poly algorithm is
  desired. Currently supports models from the following deep learning
  frameworks:

  - `tensorflow`/`keras` models built as a sequential model.

  - `torch`/`luz` models built as a sequential model.

  It also supports a named `list` as input which allows to introduce by
  hand a model from any other source. This `list` should be of length L
  (number of hidden layers + 1) containing the weights matrix for each
  layer. Each element of the list should be named as the activation
  function used at each layer. Currently supported activation functions
  are `"tanh"`, `"softplus"`, `"sigmoid"` and `"linear"`.

  At any layer \\l\\, the expected shape of such matrices is of the form
  \\(h\_{(l-1)} + 1)\*(h_l)\\, that is, the number of rows is the number
  of neurons in the previous layer plus the bias vector, and the number
  of columns is the number of neurons in the current layer L. Therefore,
  each column corresponds to the weight vector affecting each neuron in
  that layer. The bias vector should be in the first row.

- data:

  Matrix or data frame containing the predictor variables (X) to be used
  as input to compute their activation potentials. The response variable
  column should not be included.

- max_order:

  `integer` that determines the maximum order that will be forced in the
  final polynomial, discarding terms of higher order that would
  naturally arise when considering all Taylor expansions allowed by
  `taylor_orders`.

- taylor_orders:

  `integer` or `vector` of length L that sets the degree at which Taylor
  expansion is truncated at each layer. If a single value is used, that
  value is set for each non linear layer and 1 for linear at each layer
  activation function. Default set to `8`.

- constraints:

  Boolean parameter determining if the NN is constrained (TRUE) or not
  (FALSE). This only modifies the plots title to show "constrained" or
  "unconstrained" respectively.

- taylor_interval:

  optional parameter determining the interval in which the Taylor
  expansion is represented. Default is 1.5.

- ...:

  Additional parameters.

## Value

A list of plots.
