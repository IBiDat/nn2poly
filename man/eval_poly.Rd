% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eval_poly.R
\name{eval_poly}
\alias{eval_poly}
\title{Evaluates a polynomial on a single observation of variables.}
\usage{
eval_poly(x, labels, coeffs)
}
\arguments{
\item{x}{Input data as matrix row, vector or dataframe row.
The number of columns should be the number of variables in the polynomial
(the dimension p).}

\item{labels}{Labels of the coefficients, that is, a list of vectors where
each one represents the combination of variables associated with that
coefficient. The variables are numbered from 1 to p.}

\item{coefficients}{The vector of numeric coefficients for the polynomial.}
}
\value{
Numerical value with the polynomial evaluated at x.
}
\description{
Evaluates a polynomial on a single observation of variables.
}