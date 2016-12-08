require("ggplot2")

personalTheme <- theme(
  # major grid lines & bacground
  panel.grid.major = element_line(color = "#8E9090"),
  panel.background = element_rect(fill = "#CBCDCC"),
  
  # remove tickmarks, minor grid lines, and facet titles
  axis.ticks = element_blank(),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  
  # padding around the chart & extend background color
  plot.margin = unit(c(15,15,15,15), "point"),
  plot.background = element_rect(fill = "#CBCDCC"),
  
  # text: axis & title
  # I use a nonstandard font so comment out these 3 lines and uncomment the 3 below
  plot.title = element_text(hjust = 0, margin = margin(0,0,20,0), size = 20, family = "Input", face = "bold"),
  axis.text = element_text(color = "#000000", size = 8, family = "Input"),
  axis.title = element_text(size = 8, family = "Input", face = "bold"),
  strip.text.x = element_text(size = 8, family = "Input", color = "#000000"),
  # plot.title = element_text(hjust = 0, margin = margin(0,0,20,0), size = 20, face = "bold"),
  # axis.text = element_text(color = "#2E2E2E", size = 11),
  # axis.title = element_text(size = 11, face = "bold"),
  axis.title.x = element_text(margin = margin(15,0,0,0)),
  axis.title.y = element_text(margin = margin(15,20,0,0)),
  
  # legend, if necessary, uncomment the last few otherwise use single line below
  # legend.position = "none"
  legend.position = "bottom",
  legend.background = element_rect(fill = "#CBCDCC"),
  legend.key = element_rect(fill = "#CBCDCC", color = "#CBCDCC"),
  # again, use a nonstandard font so two alternatives
  legend.text = element_text(family = "Input", color = "#000000", size = 11),
  legend.title = element_text(family = "Input", color = "#000000", face = "bold", size = 14)
  # legend.text = element_text(color = "#2E2E2E", size = 11),
  # legend.title = element_text(color = "#2E2E2E", face = "bold", size = 14)
)