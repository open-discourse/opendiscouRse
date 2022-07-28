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

plot_cov <- function(data, group_var = NULL, exclude_vars = NULL) {
  checkmate::check_data_frame(data)

  if (is.null(group_var)) {
    data %>%
      dplyr::select(-exclude_vars) %>%
      suppressMessages() %>%
      purrr::map_df(~ sum(is.na(.))) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ . / nrow(data))) %>%
      tidyr::pivot_longer(everything()) %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = value)) +
      ggplot2::geom_col() +
      ggplot2::scale_y_continuous(
        name = "Percentage\nMissing",
        limits = c(0, 1),
        breaks = seq(0, 1, 0.2),
        labels = scales::label_percent()
      ) +
      ggplot2::labs(x = "", y = "")
  } else {
    df <- data %>%
      dplyr::select(-exclude_vars) %>%
      dplyr::group_by(dplyr::across(group_var)) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        n = purrr::map(data, ~ dplyr::count(.)) %>% unlist(),
        data = purrr::map(data, ~ purrr::map_df(., ~ sum(is.na(.))))
      ) %>%
      tidyr::unnest() %>%
      suppressWarnings() %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ . / n)) %>%
      tidyr::pivot_longer(- {{ group_var }} )

    group_var <- rlang::sym(group_var)

    df %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = {{ group_var }}, fill = value)) +
      ggplot2::geom_tile(color = "black", lwd = 0.5) +
      ggplot2::scale_fill_continuous(
        name = "Percentage\nMissing",
        low = "white",
        high = "black",
        limits = c(0, 1),
        breaks = seq(0, 1, 0.2),
        labels = scales::label_percent()
      ) +
      ggplot2::coord_fixed() +
      ggplot2::labs(x = "" , y = "")
  }
}
