# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

##### INDIVIDUAL PLOTS #####
## This function plots detected blobs with associated nearest neighbor information
blobPlot_nn <- function(blobs_nn = NULL){
  # plot blob coordinates, color by area, size by nn distance, shape by nn distance category
  p <- ggplot2::ggplot(blobs_nn, ggplot2::aes(xcent, abs(max(ycent) - ycent), colour = area, size = nndist, shape = nndist_cat)) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_viridis_c() +
    ggplot2::scale_size_area(max_size = 4) +
    ggplot2::labs(
      x = "X-Centroid", y = "Y-Centroid",
      colour = "Area",
      size = "Nearest\nNeighbor\nDistance", shape = "Distance\nCategory",
      title = "Watershed-Detected Blobs"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_classic()
  
  # make bold plot
  p <- boldPlots(p = p)
  
  # return bold plot
  return(p)
}

## This function plots the density distribution of nearest neighbor distances
blobPlot_nndDist <- function(blobs_nn = NULL){
  # plot density of nearest neighbor distances
  p <- ggplot2::ggplot(blobs_nn, ggplot2::aes(nndist)) +
    ggplot2::geom_density() +
    ggplot2::xlim(0, NA) +
    ggplot2::labs(x = "Nearest-Neighbor Distance", y = "Density", title = "Nearest Neighbor Distances") +
    ggplot2::theme_classic()
  
  # make bold plot
  p <- boldPlots(p = p)
  
  # return bold plot
  return(p)
}

## This function plots the density distribution of blob areas
blobPlot_sizeDist <- function(blobs = NULL){
  # plot density of area
  p <- ggplot2::ggplot(blobs, ggplot2::aes(area)) +
    ggplot2::geom_density() +
    ggplot2::xlim(0, NA) +
    ggplot2::labs(x = "Area", y = "Density", title = "Size Distribution") +
    ggplot2::theme_classic()
  
  # make bold plot
  p <- boldPlots(p = p)
  
  # return bold plot
  return(p)
}

## This function plots x- and y-centroids after sorting them; isolated points are spatial outliers
blobPlot_centroids <- function(blobs = NULL){
  # plot sorted x-centroids
  p1 <- blobs %>% dplyr::arrange(xcent) %>% tibble::rowid_to_column() %>%
    ggplot2::ggplot(ggplot2::aes(rowid, xcent)) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::labs(x = "Rank", y = "X-Centroid", title = "Grouped X") +
    ggplot2::theme_classic()
  
  # plot sorted y-centroids
  p2 <- blobs %>% dplyr::arrange(ycent) %>% tibble::rowid_to_column() %>%
    ggplot2::ggplot(ggplot2::aes(rowid, ycent)) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::labs(x = "Rank", y = "Y-Centroid", title = "Grouped Y") +
    ggplot2::theme_classic()
  
  # make bold plots
  p1 <- boldPlots(p = p1)
  p2 <- boldPlots(p = p2)
  
  # arrange plots
  p <- gridExtra::grid.arrange(p1, p2, ncol = 2)
  
  # return grid
  return(p)
}

##### FINAL PLOT #####
## This function plots colonies and colours by pixel peaks (a measure of "not greenness")
blobPlot_color <- function(yeastColonies_color = NULL){
  # plot centroids, size by area, and fill by pixel peaks
  p <- ggplot2::ggplot(yeastColonies_color, ggplot2::aes(X, abs(max(Y) - Y))) +
    ggplot2::geom_point(ggplot2::aes(size = Size, fill = Color), pch = 21, colour = "black") +
    ggplot2::scale_size_area(max_size = 6) +
    ggplot2::scale_fill_gradient(low = "darkred", high = "white") +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = "X", y = "Y",
      size = "Area", fill = "Color",
      title = "Yeast Colonies with Color Detection"
    ) +
    ggplot2::theme_classic()
  
  # make bold plot
  p <- boldPlots(p = p)
  
  # return bold plot
  return(p)
}
