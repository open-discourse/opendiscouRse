library(ggplot2)

# academic theme ----------------------------------------------------------

theme_academia <- function() {
  
  # font <- "EB Garamond"
  # font <- "CMU Serif"
  # font <- "ComputerModern"
  font <- "Times"
  
  # base theme
  theme_linedraw() %+replace%
    
    theme(
      
      text = element_text(family = font),
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