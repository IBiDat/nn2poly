# Plots a comparison between two sets of points.

If the points come from the predictions of an NN and a PM and the line
`(plot.line = TRUE)` is displayed, in case the method does exhibit
asymptotic behavior, the points should not fall in the line.

## Usage

``` r
plot_diagonal(
  x_axis,
  y_axis,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  plot.line = TRUE
)
```

## Arguments

- x_axis:

  Values to plot in the `x` axis.

- y_axis:

  Values to plot in the `y` axis.

- xlab:

  Lab of the `x` axis

- ylab:

  Lab of the `y` axis.

- title:

  Title of the plot.

- plot.line:

  If a red line with `slope = 1` and `intercept = 0` should be plotted.

## Value

Plot (ggplot object).
