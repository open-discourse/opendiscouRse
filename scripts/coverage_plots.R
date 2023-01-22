devtools::load_all()
library(tidyverse)

## Automatically use showtext to render text

library(showtext)
font_add_google("Source Sans Pro")
showtext::showtext_auto()


coverage_speeches <- plot_cov(speeches) +
  theme_linedraw() +
  theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    text = element_text(family = "Source Sans Pro"),
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

coverage_speeches

ggsave(
  "figures/coverage_speeches.png",
  coverage_speeches,
  width = 8,
  height = 5
)


plot_cov(contributions_simplified) +
  theme_linedraw() +
  theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    text = element_text(family = "Source Sans Pro"),
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

plot_cov(contributions_extended) +
  theme_linedraw() +
  theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    text = element_text(family = "Source Sans Pro"),
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


coverage_politicians <- plot_cov(politicians) +
  theme_linedraw() +
  theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    text = element_text(family = "Source Sans Pro"),
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

coverage_politicians

ggsave(
  "figures/coverage_politicians.png",
  coverage_politicians,
  width = 8,
  height = 5
)


plot_cov(factions) +
  theme_linedraw() +
  theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    text = element_text(family = "Source Sans Pro"),
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
