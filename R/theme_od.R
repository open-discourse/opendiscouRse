#' ggplot2 theme for Open Discourse
#'
#' @return `ggplot2` theme
#' @export
#'
theme_od <- function() {

  sysfonts::font_add_google("Source Sans Pro", "ssp")

  # base theme
  ggplot2::theme_linedraw() %+replace%

    ggplot2::theme(

      text = element_text(family = "ssp"),
      legend.text = element_text(size = 6),
      legend.title = element_blank(),
      legend.box.background = element_rect(colour = "black", fill = "white", linetype = "solid"),
      # legend.position = c(0.85, 0.85),

      # grid lines
      panel.grid.major = element_line(color = "grey60", size = 0.2),
      panel.grid.minor = element_line(color = "grey80", size = 0.1),

      # faceting
      strip.background = element_blank(),
      strip.text = element_text(color = "black")

    )

}
