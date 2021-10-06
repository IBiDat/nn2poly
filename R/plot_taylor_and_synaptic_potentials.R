################################
## Function that allows to take a NN and the data input values

## Author: Pablo Morala
###############################

#' Function that allows to take a NN and the data input values
#' and plot the distribution of data synaptic potentials
#' (sum of input values * weights) at all neurons together AT EACH HIDDEN LAYER
#' with the Taylor expansion used in the activation functions.
#' @param data a
#' @param weights_list a
#' @param af_string_list a
#' @param q_taylor_vector a
#'
#' @return a
#' @export
#'

plot_taylor_and_synpatic_potentials <- function(data,
                                                weights_list,
                                                af_string_list,
                                                q_taylor_vector) {

  # This function is currently only prepared for regression cases:
  if (af_string_list[[length(af_string_list)]] != "linear") {
    print("The NN is not a regression")
    return(NULL)
  }

  # The number of plots that we want to obtain is the number of hidden layers:
  n_plots <- length(weights_list) - 1

  # Initialize the list containing plots:
  plots_list <- vector(mode = "list", length = n_plots)

  # get the dimension
  p <- dim(data)[2] - 1

  # get the AF as R functions:
  af_function_list <- change_string_to_function(af_string_list)


  # We have to store the output of each layer to use it as input in the next one
  # and use it to compute the synaptic potentials.
  # Therefore, we initialize the variable "output" with data so the loop starts correctly.
  output <- data[, -(p + 1)]
  # The number of inputs is then:
  n_input <- dim(output)[1]

  for (k in 1:n_plots) {

    # Initialize input as output from previous layer
    input <- output

    # Obtain the weights at this layer
    weights <- weights_list[[k]]

    # We need to add a column with 1's to the input data to multiply them by the biases
    input <- cbind(rep(1, n_input), input)

    # Obtain number of neurons h at the desired layer
    h <- dim(weights)[2]

    # Compute the synaptic potentials first as a matrix, to compute each neuron separately
    synaptic_potentials <- matrix(0, n_input, h)
    for (j in 1:h) {
      for (i in 1:n_input) {
        synaptic_potentials[i, j] <- sum(weights[, j] * input[i, ])
      }
    }

    # Now join all the values in a vector to plot the density because
    # we don't care in which neuron we have the problems as long as there is one.
    synaptic_potentials_vectorized <- as.vector(synaptic_potentials)

    # However, we still need to use the matrix to compute the AF, so we obtain the
    # output to use in the next layer:
    fun <- af_function_list[[k]]
    output <- fun(synaptic_potentials)


    ################## Plot creation ########################


    # Depending on the function we need to obtain an adequate interval:
    if (af_string_list[[k]] == "tanh") {
      taylor_interval <- 2.5
    } else if (af_string_list[[k]] == "sigmoid") {
      taylor_interval <- 5
    } else if (af_string_list[[k]] == "softplus") {
      taylor_interval <- 5
    }

    # Create the x values for the Taylor plot
    x <- seq(-taylor_interval, taylor_interval, length.out = 1000)

    # tolerance predefined to be 0.1
    tol <- 0.1

    # create data frame and create an empty plot with only the density of those values
    df.density <- as.data.frame(synaptic_potentials_vectorized)
    names(df.density) <- c("x")

    plot.density <- ggplot2::ggplot(df.density) +
      ggplot2::aes(x = x, y = ..scaled..) +
      ggplot2::geom_density(linetype = "dashed") +
      ggplot2::xlim(x[1], x[length(x)]) +
      ggplot2::theme_void()

    #### Taylor graph ######

    # compute the true function
    yf <- fun(x)
    # compute the Taylor approximation
    pol <- pracma::taylor(fun, 0, q_taylor_vector[k])
    yp <- pracma::polyval(pol, x)
    # compute the error as the absolute value of the difference
    error <- abs(yf - yp)

    # get points to place error bars for error <= tol
    ind <- which(error <= tol)
    error1 <- x[ind[1]]
    error2 <- x[ind[length(ind)]]

    # Now we create the Taylor plot and add the density behind it.
    df.plot <- data.frame(x, yf, yp, error)

    plot.Taylor <- ggplot2::ggplot() +
      ggplot2::geom_line(data = df.plot, ggplot2::aes(x, yf)) +
      ggplot2::geom_line(data = df.plot, ggplot2::aes(x, yp), color = "red") +
      ggplot2::geom_line(data = df.plot, ggplot2::aes(x, error), color = "blue") +
      ggplot2::geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
      ggplot2::labs(x = "x") +
      ggplot2::labs(y = "y") +
      ggplot2::geom_vline(xintercept = error1, color = "gray", linetype = "dashed") +
      ggplot2::geom_vline(xintercept = error2, color = "gray", linetype = "dashed") +
      ggplot2::annotation_custom(ggplot2::ggplotGrob(plot.density), ymin = 0, ymax = 3) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10)) +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 10), axis.title = ggplot2::element_text(size = 10))

    plots_list[[k]] <- plot.Taylor
  }

  return(plots_list)
}
