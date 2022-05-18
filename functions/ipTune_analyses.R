# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## This function uses watershed object detection after a series of image processing steps to detect "blobs" in the cropped image
blobDetection <- function(
    imgPath = NULL,
    compression = "1500", whichChannel = "gray",
    invert = FALSE,
    threshold_width = 27, threshold_height = 27, offset = 0.05,
    brush_size = 5,
    watershed_tolerance = 3, watershed_ext = 1
){
  # read image and convert to grayscale
  img <- magick::image_read(imgPath) %>%
    magick::image_scale(., compression) %>%
    magick::image_channel(., channel = "gray") %>%
    magick::as_EBImage()
  
  # optionally invert image
  if(invert) {
    img <- max(img) - img
  }
  
  # binarize image with adaptive linear thresholding
  ymaskt <- EBImage::thresh(img, w = threshold_width, h = threshold_height, offset = offset)
  
  # open (erode then dilate) image according to disc kernel; binarize image
  disc <- EBImage::makeBrush(brush_size, shape = "disc")
  ymaskf <- EBImage::opening(ymaskt, disc) %>% EBImage::bwlabel()
  
  # distance map transformation -> watershed transformation and object detection
  dmap <- EBImage::distmap(ymaskf)
  ymask <- EBImage::watershed(dmap, tolerance = watershed_tolerance, ext = watershed_ext)
  
  # blob statistics and coordinate correction
  blobs <- EBImage::computeFeatures(
    ymask, img,
    methods.ref = c("computeFeatures.moment", "computeFeatures.shape")
  ) %>%
    as.data.frame() %>%
    dplyr::rename(xcent = x.0.m.cx, ycent = x.0.m.cy, area = x.0.s.area, radius = x.0.s.radius.mean) %>%
    dplyr::select(xcent, ycent, area, radius)
  
  # return blob statistics
  return(blobs)
}

## This function adds nearest neighbor information to the detected blobs; used for post-processing after object detection
blobNeighbors <- function(blobs = NULL, minDist = 20){
  # distance matrix between all detected blobs
  blobsdist <- dist(blobs[,1:2]) %>% as.matrix()
  
  # nearest neighbor distances and identities
  blobsdist_nnDist <- apply(blobsdist, 2, function(x) min(x[x != 0]))
  
  # add nearest neighbor information to data.frame
  blobs_nn <- blobs %>%
    dplyr::mutate(
      nndist = blobsdist_nnDist,
      nndist_cat = dplyr::case_when(
        nndist <= minDist ~ "SMALL",
        nndist > minDist ~ "LARGE"
      )
    )
  
  # return blobs with nearest neighbor statistics
  return(blobs_nn)
}

## This function merges blobs that are below the minimum cutoff (nndist_cat == "SMALL")
mergeBlobs <- function(blobs_nn = NULL, minDist = 20){
  # isolate blobs where nearest neighbor category is SMALL
  closeblobs <- blobs_nn %>% dplyr::filter(nndist_cat == "SMALL")
  distantblobs <- blobs_nn %>% dplyr::filter(nndist_cat != "SMALL")
  
  # add counter for number of loops
  counter <- 0
  
  # keep merging until there are no more blobs with a small nearest neighbor distance
  while(nrow(closeblobs) != 0 & counter < 100){
    # make distance matrix and set 0 to NA
    closeblobs_dist <- dist(closeblobs[,1:2]) %>% as.matrix()
    
    # within 1st column of distance matrix, get names of all rows with distances < minDistToMerge
    neighbors <- closeblobs_dist[which(closeblobs_dist[,1] < minDist), 1] %>% names()
    
    # average centroids and sum areas for neighbors
    avgblob <- data.frame(
      xcent = closeblobs[neighbors,"xcent"] %>% mean(),
      ycent = closeblobs[neighbors,"ycent"] %>% mean(),
      area = closeblobs[neighbors,"area"] %>% sum(),
      radius = NA, nndist = NA, nndist_cat = NA
    ) %>% dplyr::mutate(radius = sqrt(area / pi))
    
    # bind merged blob to distantblobs data.frame
    distantblobs <- rbind(distantblobs, avgblob)
    
    # remove this set of neighbors from closeblobs data.frame
    closeblobs <- closeblobs[!(row.names(closeblobs) %in% neighbors),]
    
    # bump counter
    counter <- counter + 1
  }
  
  # return merged blobs
  return(distantblobs)
}

## This function removes blobs with sizes below a user-defined threshold
sizeFiltBlobs <- function(blobs = NULL, minSize = 100){
  # omit objects with small sizes
  blobs <- blobs %>% dplyr::filter(area > minSize)
  
  # return filtered data.frame
  return(blobs)
}

## This function groups centroids into columns / rows and marks noise as OUTLIERS
blobGroups <- function(centroidVector = NULL, minDist = 20, groupType = "column"){
  # make centroid vector into data.frame, give unique IDs, sort by centroid, and initialize category variable as NA
  centroidDF <- as.data.frame(centroidVector) %>%
    tibble::rowid_to_column() %>%
    dplyr::arrange(centroidVector) %>%
    dplyr::mutate(lineGroup = NA)
  
  # sort by centroid and get distance matrix
  sort_distMat <- dist(centroidDF$centroidVector) %>% as.matrix()
  
  # keep creating groups until you have 24
  i <- 1
  while(ncol(sort_distMat) != 0){
    # all blobs close to the one in column 1 (< minDist pixels away) are added to the same column
    centroidGroup <- sort_distMat[which(sort_distMat[,1] < minDist), 1] %>% names()
    
    # check if blob belongs to any column
    # if(length(centroidGroup) != 0){
    if(length(centroidGroup) > 3){
      # label the blobs with the column number
      centroidDF[centroidGroup, "lineGroup"] <- paste0(
        ifelse(groupType == "column", "c", "r"),
        stringr::str_pad(i, width = 2, side = "left", pad = "0")
      )
      
      # bump the iterator
      i <- i + 1
    } else{
      # mark outliers
      centroidDF[colnames(sort_distMat)[1], "lineGroup"] <- "OUTLIER"
    }
    
    # remove the first set of columns
    sort_distMat <- sort_distMat[,-c(1:length(centroidGroup))] %>% as.matrix()
  }
  
  # sort by original column ID and pull line grouping
  lineGroups <- centroidDF %>%
    dplyr::arrange(rowid) %>%
    dplyr::pull(lineGroup)
  
  # return groupings
  return(lineGroups)
}

## This function is a wrapper function for blobGroups(.); it also includes outlier removal and a small plot
blobSort <- function(blobs_nn_merge = NULL, minDist = 20){
  # get column and row groupings
  blobs_nn_merge$colGroups <- blobGroups(centroidVector = blobs_nn_merge$xcent, minDist = minDist, groupType = "column")
  blobs_nn_merge$rowGroups <- blobGroups(centroidVector = blobs_nn_merge$ycent, minDist = minDist, groupType = "row")
  
  # remove blobs marked as outliers
  blobs_nn_merge <- blobs_nn_merge %>% dplyr::filter(colGroups != "OUTLIER" & rowGroups != "OUTLIER")
  
  # return blobs with row / column groups and no outliers
  return(blobs_nn_merge)
}

## This function detects and fills holes
missingBlobs <- function(blobs_noFP_nn = NULL){
  # get all observed coordinates
  observedCoords <- blobs_noFP_nn %>%
    dplyr::mutate(position = paste(colGroups, rowGroups, sep = "-")) %>%
    dplyr::pull(position)
  
  # get number of columns and number of rows
  colCount <- blobs_noFP_nn$colGroups %>% substr(., 2, 3) %>% as.numeric() %>% max()
  rowCount <- blobs_noFP_nn$rowGroups %>% substr(., 2, 3) %>% as.numeric() %>% max()
  
  # make vector of all theoretical coordinates
  allCoords <- expand.grid(
    paste0("c", stringr::str_pad(1:colCount, width = 2, side = "left", pad = "0")),
    paste0("r", stringr::str_pad(1:rowCount, width = 2, side = "left", pad = "0"))
  ) %>%
    dplyr::mutate(position = paste(Var1, Var2, sep = "-")) %>%
    dplyr::pull(position)
  
  # see if there are any differences in all theoretical coordinates vs all observed coordinates
  if(length(setdiff(allCoords, observedCoords)) != 0){
    # find the coordinates that are missing
    missingCoords <- setdiff(allCoords, observedCoords) %>%
      stringr::str_split_fixed(., pattern = "-", n = 2) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      dplyr::rename(colGroups = V1, rowGroups = V2) %>%
      dplyr::mutate(xcent = NA, ycent = NA, area = 0, radius = 0, nndist = NA, nndist_cat = NA) %>%
      dplyr::relocate(c(colGroups, rowGroups), .after = nndist_cat)
    
    # iterate through data.frame of missing coordinates and estimate the coordinates
    for(i in 1:nrow(missingCoords)){
      # get average x-coordinate of column
      missingCoords[i,1] <- blobs_noFP_nn %>%
        dplyr::filter(colGroups == missingCoords$colGroups[i]) %>%
        dplyr::pull(xcent) %>%
        mean()
      
      # get average y-coordinate of column
      missingCoords[i,2] <- blobs_noFP_nn %>%
        dplyr::filter(rowGroups == missingCoords$rowGroups[i]) %>%
        dplyr::pull(ycent) %>%
        mean()
    }
    
    # bind missing coordinates to data.frame
    blobs_noFP_nn <- rbind(blobs_noFP_nn, missingCoords)
  }
  
  # return data.frame
  return(blobs_noFP_nn)
}

## This function detects the colors of yeast colonies
blobColors <- function(imgPath = NULL, compression = "1500", whichChannel = "green", yeastColonies = NULL){
  # read, compress, and extract "green" as magick object; convert to imager object
  img <- magick::image_read(imgPath) %>%
    magick::image_scale(., compression) %>%
    magick::image_channel(., channel = whichChannel) %>%
    magick::as_EBImage()
  
  # initialize vector for pixel values
  pixelPeaks <- c()
  
  # crop out each centroid +/- radius
  for(i in 1:nrow(yeastColonies)){
    # check if colony is marked as a hole
    if(yeastColonies$area[i] != 0 ){
      # get range of x-coordinates
      xmin <- round(yeastColonies$xcent[i] - yeastColonies$radius[i])
      xmax <- round(yeastColonies$xcent[i] + yeastColonies$radius[i])
      xrange <- xmin:xmax
      
      # get range of y-coordinates
      ymin <- round(yeastColonies$ycent[i] - yeastColonies$radius[i])
      ymax <- round(yeastColonies$ycent[i] + yeastColonies$radius[i])
      yrange <- ymin:ymax
      
      # crop colony
      img_crop <- img[xrange,yrange]
      img_crop[img_crop > EBImage::otsu(img_crop)] <- EBImage::otsu(img_crop)
      
      # get pixel value corresponding to the largest peak of the kernel density estimate
      d <- density(img_crop)
      pixelPeaks <- c(pixelPeaks, d$x[which.max(d$y)])
    } else{
      # fill with NA
      pixelPeaks <- c(pixelPeaks, NA)
    }
  }
  
  # add pixel peaks to yeast data.frame
  yeastColonies$color <- pixelPeaks %>% round(., 3)
  
  # return data.frame
  return(yeastColonies)
}

## This function post-processes the objects
blobProcessing <- function(yeastColonies = NULL, whichPlate = 1, whichImg = NULL){
  
  # rename columns, round centroid coordinates, make library coordinates, reorder columns and omit some of them, sort by coordinate
  yeastColonies <- yeastColonies %>%
    dplyr::rename(X = xcent, Y = ycent, Size = area, Color = color) %>%
    dplyr::mutate(
      X = round(X, digits = 2), Y = round(Y, digits = 2),
      Coordinate = paste0("p", stringr::str_pad(whichPlate, width = 2, side = "left", pad = "0"), "-", rowGroups, "-", colGroups),
      ImagePath = whichImg
    ) %>%
    dplyr::select(ImagePath, Coordinate, X, Y, Size, Color) %>%
    dplyr::arrange(Coordinate)
  
  # return post-processed data.frame
  return(yeastColonies)
}