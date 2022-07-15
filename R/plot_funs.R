#' Plotting distributions by group variables.
#'
#' @importFrom magrittr %>%
#'
#' @param data Input data frame.
#' @param metric_var Metric variable whose distribution is displayed.
#' @param group_var Categorical variable that is used to group the metric variable visually.
#' @param plot_type Type of plot, either "ridge_plot" or "box_plot". Default is "ridge_plot".
#'
#' @return A `ggplot` object.
#' @export
#'
plot_dist <- function(data, metric_var, group_var, plot_type = "ridge_plot") {
  plot <- data %>%
    ggplot2::ggplot(
      aes(x = {{ metric_var }}, y = as.factor( {{ group_var }} ))
      # aes(x = metric_var, y = as.factor(group_var))
    )
  if (plot_type == "ridge_plot") {
    plot +
      ggridges::geom_density_ridges()
  } else if (plot_type == "box_plot") {
    plot +
      ggplot2::geom_boxplot()
  }
}
