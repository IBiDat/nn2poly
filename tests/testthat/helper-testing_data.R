
# Testing data 1 ----
# Created to test with all weights equal to 1, not training any NN.
# With these values we have some tests with manually computed coefficients.
testing_helper_1 <- function(){
  testing_data_1 <- vector(mode="list", length= 0)
  testing_data_1$weights_list <- vector(mode="list", length= 3)
  testing_data_1$weights_list[[1]] <- matrix(1,3,2)
  testing_data_1$weights_list[[2]] <- matrix(1,3,2)
  testing_data_1$weights_list[[3]] <- matrix(1,3,1)
  testing_data_1$af_string_list <- list("softplus", "softplus", "linear")
  testing_data_1$taylor_orders <- c(2, 2, 1)
  return(testing_data_1)
}


# Testing data 2 ----
# In this case we will generate some polynomial data with linear output
# This data will be used in testing keras/tensorflow and luz/torch
# Note that the data is random so the NN will not learn properly but
# we can still test if the coefficients are as expected by nn2poly.
testing_helper_2 <- function(){
  set.seed(42)
  n_sample <- 100
  p <- 2
  testing_data_2 <- vector(mode="list", length= 0)
  testing_data_2$train_x <- matrix(rnorm(n_sample*p),ncol = p)
  testing_data_2$train_y <- matrix(rnorm(n_sample),ncol = 1)
  testing_data_2$taylor_orders <- c(2, 2, 1)
  return(testing_data_2)
}




