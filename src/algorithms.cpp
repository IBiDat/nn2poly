#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector alg_linear(NumericVector weights_layer,
                         ListOf<NumericVector> coeffs_list_input)
{
  // Number of neurons from layer previous to current layer:
  int h_layer = coeffs_list_input.size();

  // The number of coefficients is the same as the obtained in the previous
  // non linear case:
  int length_coeffs = coeffs_list_input[0].size();

  // Initialize output
  NumericVector coeffs_output(length_coeffs);

  // Special case: the intercept:
  coeffs_output[0] = weights_layer[0];
  for (int j = 0; j < h_layer; j++) {
    coeffs_output[0] = coeffs_output(0) +
      weights_layer[j + 1] * coeffs_list_input[j][0];
  }

  // Rest of the coeffs (start from the 2nd postion (1))
  for (int i = 1; i < length_coeffs; i++) {
    for (int j = 0; j < h_layer; j++) {
      coeffs_output(i) = coeffs_output(i) +
        weights_layer[j + 1] * coeffs_list_input[j][i];
    }
  }

  return coeffs_output;
}
