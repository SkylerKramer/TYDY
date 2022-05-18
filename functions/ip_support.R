# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## This is a support function that writes image processing parameters to a JSON file
ipTune2JSON <- function(
    compression = NULL,
    whichChannel = NULL,
    invert = NULL,
    threshold_width = NULL, threshold_height = NULL,
    offset = NULL,
    brush_size = NULL,
    watershed_tolerance = NULL, watershed_ext = NULL,
    minDist = NULL,
    sizeFilt = NULL,
    whichChannel_colorDetection = NULL
){
  # set all IP parameters into data.frame and convert to JSON; jsonlite::prettify looks good in R but bad elsewhere...
  ipParams <- data.frame(
    compression = compression %>% as.numeric(),
    whichChannel = whichChannel,
    invert = invert,
    threshold_width = threshold_width, threshold_height = threshold_height,
    offset = offset,
    brush_size = brush_size,
    watershed_tolerance = watershed_tolerance, watershed_ext = watershed_ext,
    minDist = minDist,
    sizeFilt = sizeFilt,
    whichChannel_colorDetection = whichChannel_colorDetection
  ) %>% jsonlite::unbox() %>% jsonlite::toJSON()
  
  # return prettified JSON object
  return(ipParams)
}

## This is a support function to validate the JSON file given to the bulk analysis tab against a predefined schema
validateIP <- function(schemaPath = NULL, jsonPath = NULL){
  # create validator object from schema
  v <- jsonvalidate::json_validator(schemaPath)
  
  # read JSON parameters
  j <- jsonlite::read_json(jsonPath, simplifyVector = FALSE) %>% toJSON(auto_unbox = TRUE)
  
  # return validation
  return(v(j))
}
