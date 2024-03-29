#' Plotting distributions by group variables.
#'
#' @param data Input `data.frame`.
#' @param metric_var `character` indicating metric variable whose distribution is displayed.
#' @param group_var `character` indicating categorical variable that is used to group the metric variable visually.
#' @param plot_type `character` indicating type of plot, either "ridge_plot" or "box_plot". Default is "ridge_plot".
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

#' Plotting coverage (`NA`s) of data tables.
#'
#' @param data Input `data.frame`.
#' @param group_var `character` indicating categorical variable that is used to group the data before plotting.
#' @param exclude_vars `character` indicating which variable(s) should be excluded from the data before plotting.
#' @param exclude_na_group `character` indicating whether to include or exclude potential `NA`s in the grouping variable. Default is `FALSE`.
#'
#' @return A `ggplot` object.
#' @importFrom magrittr %>%
#' @export
#'
plot_cov <- function(data, group_var = NULL, exclude_vars = NULL, exclude_na_group = FALSE) {
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
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::scale_y_continuous(
        name = "Percentage\nMissing",
        limits = c(0, 1),
        breaks = seq(0, 1, 0.2),
        labels = scales::label_percent()
      ) +
      ggplot2::labs(x = "", y = "")
  } else {
    if (exclude_na_group == FALSE) {
      df <- data %>%
        dplyr::select(-exclude_vars) %>%
        dplyr::group_by(dplyr::across(group_var))
    } else {
      df <- data %>%
        dplyr::select(-exclude_vars) %>%
        dplyr::filter(!is.na(!!rlang::sym(group_var))) %>%
        dplyr::group_by(dplyr::across(group_var))
    }
    df <- df %>%
      tidyr::nest() %>%
      dplyr::mutate(
        n = purrr::map(data, ~ dplyr::count(.)) %>% unlist(),
        data = purrr::map(data, ~ purrr::map_df(., ~ sum(is.na(.))))
      ) %>%
      tidyr::unnest() %>%
      suppressWarnings() %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ . / n)) %>%
      dplyr::select(- n) %>%
      tidyr::pivot_longer(- {{ group_var }} )

    group_var <- rlang::sym(group_var)

    df %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = as.factor( {{ group_var }} ), fill = value)) +
      ggplot2::geom_tile(color = "black", lwd = 0.5) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
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

#' Plotting summarized data frames generated by `count_data()`.
#'
#' @param data Input `data.frame` (function is designed to work with data generated from `count_data()`).
#' @param x_var Single `character` value indicating which variable is displayed on the x axis.
#' @param fill_var Single `character` value indicating which variable is used to fill the plot (`fill` argument in `ggplot()`). Default is `NULL`.
#' @param facet_var Single `character` value indicating which variable is used to facet the plot (via `facet_wrap()`) . Default is `NULL`.
#' @param exclude_na `logical` value indicating whether to exclude `NA`s in the plot (checks all variables indicated via other parameters). Default is `FALSE`.
#'
#' @return A `ggplot` object.
#' @importFrom magrittr %>%
#' @export
#'
plot_count <- function(data, x_var, fill_var = NULL, facet_var = NULL, exclude_na = FALSE) {
  checkmate::assert_data_frame(data)
  checkmate::assert(
    check_character(x_var),
    check_true(length(x_var) == 1),
    combine = "and"
  )

  x_var <- rlang::sym(x_var)

  if (!is.null(fill_var)) {
    fill_var <- rlang::sym(fill_var)
  }

  if (!is.null(facet_var)) {
    facet_var <- rlang::sym(facet_var)
  }

  if (exclude_na == FALSE) {
    df <- data
  } else {
    df <- data %>%
      dplyr::ungroup() %>%
      dplyr::filter(dplyr::if_all(c( {{ x_var }}, {{ fill_var }}, {{ facet_var }} ), ~ !is.na(.)))
  }
  df
  plot <- df %>%
    ggplot2::ggplot(ggplot2::aes(x = as.factor( {{ x_var }} ), y = n, fill = {{ fill_var }} )) +
    ggplot2::labs(x = x_var) +
    ggplot2::geom_col()

  if (is.null(facet_var)) {
    plot
  } else {
    plot +
      ggplot2::facet_wrap(ggplot2::vars(as.factor( {{ facet_var }} )))
  }
}

#' Plotting summarized data frames generated by `rel_freq_data()`.
#'
#' @param data Input `data.frame` (function is designed to work with data generated from `rel_freq_data()`).
#' @param x_var Single `character` value indicating which variable is displayed on the x axis.
#' @param fill_var Single `character` value indicating which variable is used to fill the plot (`fill` argument in `ggplot()`).
#' @param facet_var Single `character` value indicating which variable is used to facet the plot (via `facet_wrap()`) . Default is `NULL`.
#' @param exclude_na `logical` value indicating whether to exclude `NA`s in the plot (checks all variables indicated via other parameters). Default is `FALSE`.
#'
#' @return A `ggplot` object.
#' @importFrom magrittr %>%
#' @export
#'
plot_rel_freq <- function(data, x_var, fill_var, facet_var = NULL, exclude_na = FALSE) {
  checkmate::assert_data_frame(data)
  checkmate::assert(
    check_character(x_var),
    check_true(length(x_var) == 1),
    combine = "and"
  )
  checkmate::assert(
    check_character(fill_var),
    check_true(length(fill_var) == 1),
    combine = "and"
  )

  sum_test <- data %>%
    dplyr::group_by(dplyr::across( {{ fill_var }} )) %>%
    dplyr::summarise(sum = sum(rel_freq)) %>%
    dplyr::distinct(sum) %>%
    dplyr::pull()

  checkmate::assert_true(all(dplyr::near(sum_test, 1)))

  x_var <- rlang::sym(x_var)
  fill_var <- rlang::sym(fill_var)

  if (!is.null(facet_var)) {
    facet_var <- rlang::sym(facet_var)
  }

  if (exclude_na == FALSE) {
    df <- data
  } else {
    df <- data %>%
      dplyr::ungroup() %>%
      dplyr::filter(dplyr::if_all(c( {{ x_var }}, {{ fill_var }}, {{ facet_var }} ), ~ !is.na(.)))
  }
  df
  plot <- df %>%
    ggplot2::ggplot(ggplot2::aes(x = as.factor( {{ x_var }} ), y = rel_freq, fill = as.factor( {{ fill_var }} ))) +
    ggplot2::labs(x = x_var, y = "", fill = fill_var) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::percent_format())

  if (is.null(facet_var)) {
    plot
  } else {
    plot +
      ggplot2::facet_wrap(ggplot2::vars(as.factor( {{ facet_var }} )))
  }
}
