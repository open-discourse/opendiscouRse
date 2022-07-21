#' Plotting distributions by group variables.
#'
#' @param data Input data frame.
#' @param metric_var Metric variable whose distribution is displayed.
#' @param group_var Categorical variable that is used to group the metric variable visually.
#' @param plot_type Type of plot, either "ridge_plot" or "box_plot". Default is "ridge_plot".
#'
#' @return A `ggplot` object.
#' @importFrom magrittr %>%
#' @export
#'
plot_dist <- function(data, metric_var, group_var, plot_type = "ridge_plot") {
  check_data_frame(data)

  metric_var <- rlang::sym(metric_var)
  group_var <- rlang::sym(group_var)

  plot <- data %>%
    ggplot2::ggplot(
      ggplot2::aes(x = {{ metric_var }}, y = as.factor( {{ group_var }} ))
    ) +
    ggplot2::labs(y = group_var)
  if (plot_type == "ridge_plot") {
    plot +
      ggridges::geom_density_ridges()
  } else if (plot_type == "box_plot") {
    plot +
      ggplot2::geom_boxplot()
  }
}
