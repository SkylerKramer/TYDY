# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## This is a wrapper function designed to streamline the process of processing images
bulkColonyDetection <- function(imgPaths = NULL, jsonPath = NULL, plateNames = NULL, imgNames = NULL){
  
  ## split string of plate names, assuming that each line has a comma-separated list of length 4 (e.g., "U01,U02,U03,U04\nU05,U06,U07,U08")
  plateNames <- strsplit(plateNames, "\n") %>% unlist()
  
  ## read and parse JSON file with parameters
  params <- jsonlite::read_json(jsonPath, simplifyVector = TRUE)
  
  # get start time
  startTime <- Sys.time()
  
  ## initialize data.frame
  yeastColonies <- data.frame()
  
  ## iterate through each provided path to a cropped plate image
  for(i in 1:length(imgPaths)){
    
    # blob-detecting pipeline
    tempColonies <- imgPaths[i] %>%
      blobDetection(
        imgPath = .,
        compression = params$compression, whichChannel = params$whichChannel,
        invert = params$invert,
        threshold_width = params$threshold_width, threshold_height = params$threshold_height, offset = params$offset,
        brush_size = params$brush_size,
        watershed_tolerance = params$watershed_tolerance, watershed_ext = params$watershed_ext
      ) %>%
      blobNeighbors(blobs = ., minDist = params$minDist) %>%
      mergeBlobs(blobs_nn = ., minDist = params$minDist) %>%
      sizeFiltBlobs(blobs = ., minSize = params$sizeFilt) %>%
      blobNeighbors(blobs = ., minDist = params$minDist) %>%
      blobSort(blobs_nn_merge = ., minDist = params$minDist) %>%
      blobNeighbors(blobs = ., minDist = params$minDist) %>%
      missingBlobs(blobs_noFP_nn = .) %>%
      blobColors(imgPath = imgPaths[i], compression = params$compression, whichChannel = params$whichChannel_colorDetection, yeastColonies = .) %>%
      blobProcessing(yeastColonies = ., whichPlate = plateNames[i], whichImg = imgNames[i])
    
    # bind data.frames
    yeastColonies <- rbind(yeastColonies, tempColonies)
    
    # message with time to finish each plate
    message(paste0(gsub("Time difference of", paste0("Elapsed time for image ", i, ": "), capture.output(round(Sys.time() - startTime, 2)))))
  }
  
  ## return data.frame
  return(yeastColonies)
}
