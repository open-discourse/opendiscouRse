#' ggplot2 theme for Open Discourse
#'
#' @return `ggplot2` theme
#' @importFrom ggplot2 `%+replace%`
#' @export
#'
theme_od <- function() {

  sysfonts::font_add_google("Source Sans Pro", "ssp")
  showtext::showtext_auto()

  # base theme
  ggplot2::theme_linedraw() %+replace%

    ggplot2::theme(

      text = ggplot2::element_text(family = "ssp"),
      # text = element_text(family = "Times"),
      legend.text = ggplot2::element_text(size = 6),
      legend.title = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(colour = "black", fill = "white", linetype = "solid"),
      # legend.position = c(0.85, 0.85),

      # grid lines
      panel.grid.major = ggplot2::element_line(color = "grey60", size = 0.2),
      panel.grid.minor = ggplot2::element_line(color = "grey80", size = 0.1),

      # faceting
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(color = "black")

    )

}
