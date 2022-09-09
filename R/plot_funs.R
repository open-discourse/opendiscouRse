#' Plotting distributions by group variables.
#'
#' @param data Input data frame.
#' @param metric_var Metric variable whose distribution is displayed.
#' @param group_var Categorical variable that is used to group the metric variable visually.
#' @param plot_type Type of plot, either "both", "ridge_plot" or "box_plot". Default is "both" (Combine both plot types).
#'
#' @return A `ggplot` object.
#' @importFrom magrittr %>%
#' @export
#'
plot_dist <- function(data, metric_var, group_var, plot_type = "both") {
  checkmate::check_data_frame(data)

  metric_var <- rlang::sym(metric_var)
  group_var <- rlang::sym(group_var)

  plot <- data %>%
    ggplot2::ggplot(
      ggplot2::aes(x = {{ metric_var }}, y = as.factor( {{ group_var }} ))
    ) +
    ggplot2::labs(y = group_var)

  if (plot_type == "both") {
    width_val <- 1 / log2(
      data %>%
        dplyr::pull(group_var) %>%
        as.factor() %>%
        nlevels()
    )
    if (width_val > 0.4) {
      width_val <- 0.4
    }
    plot +
      ggridges::geom_density_ridges(scale = 0.75) +
      ggplot2::geom_boxplot(width = width_val)
  } else if (plot_type == "ridge_plot") {
    plot +
      ggridges::geom_density_ridges(scale = 0.75)
  } else if (plot_type == "box_plot") {
    plot +
      ggplot2::geom_boxplot()
  }
}
