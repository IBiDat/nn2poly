#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector alg_linear_cpp(List coeffs_list_input,
                    int current_layer,
                    List weights_list,
                    int output_index){
  output_index = output_index -1;


  // Extract the needed weights at the given layer
  NumericMatrix weights_layer = weights_list[current_layer];
  NumericVector weights_neuron = weights_layer(_, output_index);

  // Number of neurons from layer previous to current layer:
  int h_layer = weights_neuron.size() - 1;

  // The number of coefficients is the same as the obtained in the previous
  // non linear case:
  List coeffs_labels =  coeffs_list_input[0];
  int length_coeffs = coeffs_labels.size();

  // Initialize output
  NumericVector coeffs_output(length_coeffs);

  // Special case: the intercept:
  coeffs_output(0) = weights_neuron(0);
  NumericVector aux;
  for (int j = 0; j < h_layer; j++){
    aux = coeffs_list_input[j+1];
    coeffs_output(0) = coeffs_output(0) + weights_neuron(j + 1) * aux(0);
  }


  // Rest of the coeffs (start from the 2nd postion (1))
  for (int i = 1; i < length_coeffs; i++){
    for (int j = 0; j < h_layer; j++){
      aux = coeffs_list_input[j+1];
      coeffs_output(i) = coeffs_output(i) + weights_neuron(j + 1) * aux(i);
    }
  }

  return coeffs_output;

}


/*** R

library(nn2poly)

labels <- list(1,2,c(1,1), c(1,2), c(2,2))
coeffs <- rep(1.000,6)

coeffs_list_input <- list(labels,coeffs,coeffs,coeffs)

weights_list <- list(matrix(1.000,4,4),matrix(1.000,4,4),matrix(1.000,4,4))

output_index <- 5

current_layer <- 2


alg_linear_cpp(coeffs_list_input,current_layer,weights_list, output_index)

alg_linear(coeffs_list_input,current_layer,weights_list, output_index)

*/
