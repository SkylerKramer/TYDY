# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## This function provides a visual assessment for similarity between plates
plateFeatures_ridgePlots <- function(bulkData = NULL){
  
  # make column for plate
  bulkData <- bulkData %>% dplyr::mutate(Plate = strsplit(Coordinate, "-") %>% lapply(function(x) x[1]) %>% unlist())
  
  # size plot
  p1 <- boldPlots(
    ggplot2::ggplot(bulkData, ggplot2::aes(x = Size, y = Plate, fill = stat(x))) +
      ggridges::geom_density_ridges_gradient() +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(x = "Size", y = "Plate", fill = "Size", title = "Size distribution per plate") +
      ggplot2::theme_bw()
  )
  
  # color plot
  p2 <- boldPlots(
    ggplot2::ggplot(bulkData, ggplot2::aes(x = Color, y = Plate, fill = stat(x))) +
      ggridges::geom_density_ridges_gradient() +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(x = "Color", y = "Plate", fill = "Color", title = "Color distribution per plate") +
      ggplot2::theme_bw()
  )
  
  # arrange plots in grid
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}