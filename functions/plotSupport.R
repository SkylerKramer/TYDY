# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

##### PLOT PROCESSING #####
## This function makes text in ggplot2 objects bold
boldPlots <- function(p = NULL){
  # adjust theme
  p <- p +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size=14, face= "bold", colour= "black" ),
      axis.title.x = ggplot2::element_text(size=14, face="bold", colour = "black"),    
      axis.title.y = ggplot2::element_text(size=14, face="bold", colour = "black"),    
      axis.text.x = ggplot2::element_text(size=12, face="bold", colour = "black"), 
      axis.text.y = ggplot2::element_text(size=12, face="bold", colour = "black"),
      strip.text.x = ggplot2::element_text(size = 10, face="bold", colour = "black" ),
      strip.text.y = ggplot2::element_text(size = 10, face="bold", colour = "black"),
      axis.line.x = ggplot2::element_line(color="black", size = 0.3),
      axis.line.y = ggplot2::element_line(color="black", size = 0.3),
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.3)
    )
  
  # return modified plot
  return(p)
}