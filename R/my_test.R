my_test <- function(nn2poly_example){



  # Get the needed data
  weights_list <- nn2poly_example$weights_list
  coefficients <- nn2poly_example$coefficients
  af_string_list <- nn2poly_example$af_string_list
  af_derivatives_list <- nn2poly_example$af_derivatives_list
  q_taylor_vector <- nn2poly_example$q_taylor_vector



  print(system.time({
    result <- nn2poly_algorithm(
      weights_list = weights_list,
      af_string_list = af_string_list,
      q_taylor_vector = q_taylor_vector,
      historical_coeffs = TRUE
    )
  }))

  comparison <- vector(mode = "list", length = 0)

  comparison$result<- result
  comparison$original <- coefficients

  # Comparamos el potencial en la segunda capa para neurona 1
  # Note that in new, 2 denotes neuron 1 as 1  are the labels
  print("Potential u at layer 2, neuron 1")
  print(comparison$result[[3]][[2]])

  print("Output y at layer 2, neuron 1")
  print(comparison$result[[4]][[2]][4])

  print(1/2*0.25+1/8*1.5^2+1/4*3.636294*0.25)


  0.633518334


  return(comparison)
}
