#' Plots activation potentials and Taylor expansion.
#'
#' Function that allows to take a NN and the data input values
#' and plot the distribution of data activation potentials
#' (sum of input values * weights) at all neurons together at each layer
#' with the Taylor expansion used in the activation functions. If any layer
#' is \code{'linear'} (usually will be the output), then that layer will not
#' be an approximation as Taylor expansion is not needed.
#'
#' @param data Matrix or data frame containing the predictor variables (X)
#' to be used as input to compute their activation potentials. The response
#' variable column should not be included.
#' @param weights_list \code{list} of length L ( number of hidden layers + 1)
#' containing the weights matrix for each layer.
#' The expected shape of such matrices at any layer L is of the form
#' $(h_(l-1) + 1)*(h_l)$, that is, the number of rows is the number of neurons
#' in the previous layer plus the bias vector, and the number of columns is the
#' number of neurons in the current layer L. Therefore, each column
#' corresponds to the weight vector affecting each neuron in that layer.
#' @param af_string_list \code{list} of length L containing \code{character}
#' strings with the names of the activation function used at each layer, as
#' used in [`nn2poly_algorithm`].
#' @param q_taylor_vector \code{vector} of length L containing the degree
#' (\code{numeric}) up to which Taylor expansion should be performed at each
#' layer, as used in [`nn2poly_algorithm`].
#' @param forced_max_Q Integer that determines the maximum order
#' that we will force in the final polynomial, discarding terms of higher order
#' that would naturally arise using all the orders in `q_taylor_vector`,
#' as used in [`nn2poly_algorithm`].
#' @param my_max_norm List containing type of norm and maximum value. See
#' documentation on how to constrain NN weights.
#' @param taylor_interval optional parameter determining the interval in which
#' the Taylor expansion is represented. Default is 1.5.
#'
#' @return A list of plots.
#' @export
#'

plot_taylor_and_activation_potentials <- function(data,
                                                weights_list,
                                                af_string_list,
                                                q_taylor_vector,
                                                forced_max_Q,
                                                my_max_norm,
                                                taylor_interval = 1.5) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("package 'ggplot2' is required for this functionality", call.=FALSE)

  if (!requireNamespace("cowplot", quietly = TRUE))
    stop("package 'cowplot' is required for this functionality", call.=FALSE)

  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("package 'patchwork' is required for this functionality", call.=FALSE)

  # # Check if the last layer is linear or not. If it is linear the number of
  # # plots in the list will be only the hidden layers, while if the last
  # # layer is not linear, plots will be made for each layer.
  # if (af_string_list[[length(af_string_list)]] != "linear") {
  #   print("The NN is not a regression")
  #   return(NULL)
  # }

  # The number of plots that we want to obtain is the number of hidden layers
  # (L-1) plus the output layer (L in total).
  n_plots <- length(weights_list)

  # Initialize the list containing plots:
  plots_list <- vector(mode = "list", length = n_plots)

  # get the dimension
  p <- dim(data)[2] - 1

  # get the AF as R functions:
  af_function_list <- change_string_to_function(af_string_list)


  # We have to store the output of each layer to use it as input in the next one
  # and use it to compute the activation potentials.
  # Therefore, we initialize the variable "output" with data so the loop starts
  # correctly.
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

    # Compute the activation potentials first as a matrix, to compute each neuron separately
    activation_potentials <- matrix(0, n_input, h)
    for (j in 1:h) {
      for (i in 1:n_input) {
        activation_potentials[i, j] <- sum(weights[, j] * input[i, ])
      }
    }

    # Now join all the values in a vector to plot the density because
    # we don't care in which neuron we have the problems as long as there is one.
    activation_potentials_vectorized <- as.vector(activation_potentials)

    # However, we still need to use the matrix to compute the AF, so we obtain the
    # output to use in the next layer:
    fun <- af_function_list[[k]]
    output <- fun(activation_potentials)


    ################## Plot creation ########################

    # Create the x values for the Taylor plot
    x <- seq(-taylor_interval, taylor_interval, length.out = 1000)

    # create data frame and create an empty plot with only the density of those values
    df.density <- as.data.frame(activation_potentials_vectorized)

    #### Taylor graph ######
    # compute the true function
    yf <- fun(x)
    # compute the Taylor approximation
    pol <- pracma::taylor(fun, 0, min(q_taylor_vector[k],forced_max_Q))
    yp <- pracma::polyval(pol, x)
    # compute the error as the absolute value of the difference
    error <- abs(yf - yp)

    # Now we create the Taylor plot and add the density behind it.
    df.plot <- data.frame(x, yf, yp, error)

    name_plot <- paste0("Layer ",k, ",")
    if (my_max_norm[[1]] %in% c("l1_norm","l2_norm")){
      name_plot <- paste0(name_plot," constrained")
    } else {
      name_plot <- paste0(name_plot," no constraints")
    }

    plot.taylor.simple <- ggplot2::ggplot() +
      ggplot2::geom_line(data = df.plot, ggplot2::aes(x, yf, color = "black")) +
      # This line is only used to add the density color in the legend, and then
      # covered by the red line.
      ggplot2::geom_line(data = df.plot, ggplot2::aes(x, yp, color ="darkgreen")) +
      ggplot2::geom_line(data = df.plot, ggplot2::aes(x, yp, color ="red")) +
      ggplot2::geom_line(data = df.plot, ggplot2::aes(x, error, color = "blue")) +
      ggplot2::geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10)) +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 10), axis.title = ggplot2::element_text(size = 10)) +
      ggplot2::scale_color_identity(name = "Legend",
                           breaks = c("black", "red", "blue", "darkgreen"),
                           labels = c("True function","Taylor approximation", "Error", "Activation potentials density (log(x+1) scaled)"),
                           guide = "legend") +
      ggplot2::theme_minimal() +
      ggplot2::labs(x=NULL, y=NULL)

    # Create density plot
    density_plot <- cowplot::axis_canvas(plot.taylor.simple, axis = "x") +
      ggplot2::geom_density(data = df.density,
                   ggplot2::aes(x = activation_potentials_vectorized, y = ggplot2::after_stat(log10(density+1))),
                   color = "darkgreen",
                   fill = "lightgreen", trim=TRUE) +
      ggplot2::theme_void() +
      ggplot2::ylab(name_plot) +
      ggplot2::theme(axis.title.y=ggplot2::element_text(hjust = -1, angle=0, vjust=1,
                                      margin = ggplot2::margin(r = -110)))

    plot.Taylor <- patchwork::wrap_plots(density_plot, plot.taylor.simple, ncol=1, heights=c(0.1, 0.9))

    plots_list[[k]] <- plot.Taylor
  }

  return(plots_list)
}