# Polynomial evaluation

Evaluates one or several polynomials on the given data.

## Usage

``` r
eval_poly(poly, newdata, monomials = FALSE)
```

## Arguments

- poly:

  List containing 2 items: `labels` and `values`.

  - `labels`: List of integer vectors with same length (or number of
    cols) as `values`, where each integer vector denotes the combination
    of variables associated to the coefficient value stored at the same
    position in `values`. That is, the monomials in the polynomial. Note
    that the variables are numbered from 1 to p, with the intercept is
    represented by 0.

  - `values`: Matrix (can also be a vector if single polynomial), where
    each column represents a polynomial, with same number of rows as the
    length of `labels`, containing at each row the value of the
    coefficient of the monomial given by the equivalent label in that
    same position.

  Example: If `labels` contains the integer vector `c(1,1,3)` at
  position 5, then the value stored in `values` at row 5 is the
  coefficient associated with the term \\x_1^2\*x_3\\.

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

## Value

If `monomials==FALSE`, returns a matrix containing the evaluation of the
polynomials on the given data. The matrix has dimensions
`(n_sample, n_polynomials)`, meaning that each column corresponds to the
result of evaluating all the data for a polynomial. If a single
polynomial is provided, the output is a vector instead of a row matrix.

If `monomials==TRUE`, returns a 3D array containing the monomials of
each polynomial evaluated on the given data. The array has dimensions
`(n_sample, n_monomial_terms, n_polynomials)`, where element `[i,j,k]`
contains the evaluation on observation `i` on monomial `j` of polynomial
`k`, where monomial `j` corresponds to the one on `poly$labels[[j]]`.

## Details

Note that this function is unstable and subject to change. Therefore it
is not exported but this documentations is left available so users can
use it if needed to simulate data by using `nn2poly:::eval_poly()`.

## See also

`eval_poly()` is also used in
[`predict.nn2poly()`](https://ibidat.github.io/nn2poly/reference/predict.nn2poly.md).
