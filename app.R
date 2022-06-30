# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## imports
# frontend
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyBS)

# backend
library(magick)
library(EBImage)
library(imager)
library(jsonlite)
library(jsonvalidate)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(patchwork)
library(tibble)
library(topGO)
library(org.At.tair.db)

## source files for functions
# support function for all plots
source("functions/plotSupport.R")

# functions to tune/visualize image processing
source("functions/ipTune_analyses.R")
source("functions/ipTune_plots.R")

# functions to download and validate JSON files
source("functions/ip_support.R")

# function to process images in bulk
source("functions/ipBulk_wrappers.R")
source("functions/ipBulk_plots.R")

# functions for single-screen analyses
source("functions/ss_analyses.R")
source("functions/ss_plots.R")

# functions for dual-screen analyses
source("functions/ds_analyses.R")
source("functions/ds_plots.R")

# functions for downstream bioinformatics analyses
source("functions/bioinf_analyses.R")

## source files for tabs
source("uiTabs/ipTab.R")
source("uiTabs/singleScreenTab.R")
source("uiTabs/dualScreenTab.R")
source("uiTabs/bioinfTab.R")
source("uiTabs/aboutTab.R")

## change file limit for file sizes
options(shiny.maxRequestSize = 1000*1024^2)

## sanitize error messages
options(shiny.sanitize.errors = TRUE)

# define UI
ui <- navbarPage(

  ## application title:
  ## Current: TYDY - Toolkit for Yeast Data analYsis
  ## Previous: YDAP - Yeast Data Analytics Platform
  "TYDY",
  
  ## application theme
  theme = shinythemes::shinytheme("united"),
  
  ## change size of pop-up windows -> CAUSES WARNING ON STARTUP
  tags$head(tags$style(HTML(".modal-lg {width: 75%; height: 75%}"))),
  
  ## tabs
  ipTab,
  singleScreenTab,
  dualScreenTab,
  bioinfTab,
  aboutTab,
  
  ## favicon
  # tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  
  ## source HTML code for GA if file is found (only found if running through web app)
  if(file.exists("google-analytics.html")){
    tags$head(includeHTML(("google-analytics.html")))
  }
)

# define server
server <- function(input, output, session) {

  ### IP TUNE - PROCESSING
  ## detect blobs and compute their nearest neighbor distances (NNDs)
  ipTune_blobsDetected <- eventReactive(input$ipTune_startIP, {
    
    # confirm that there is a file
    if(is.null(isolate(input$ipTune_image))){
      showNotification("Must upload a cropped image!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # detect blobs in image; compute NNDs
      blobDetection(
        imgPath = isolate(input$ipTune_image$datapath),
        compression = isolate(input$ipTune_compression),
        whichChannel = isolate(input$ipTune_objectChannel),
        invert = ifelse(isolate(input$ipTune_inversion) == "Yes", TRUE, FALSE),
        threshold_width = isolate(input$ipTune_thresholdWidth),
        threshold_height = isolate(input$ipTune_thresholdHeight),
        offset = isolate(input$ipTune_offset),
        brush_size = isolate(input$ipTune_brushSize),
        watershed_tolerance = isolate(input$ipTune_watershedTolerance),
        watershed_ext = isolate(input$ipTune_watershedExt)
      ) %>%
        blobNeighbors(blobs = ., minDist = isolate(input$ipTune_minDist))
    }
  })
  
  ## fill holes; detect color; post-process objects
  ipTune_yeastColonies <- eventReactive(input$ipTune_startCleaning, {
    
    # confirm that there is a blob object ready to have holes filled and color detected
    if(is.null(ipTune_blobsDetected())){
      return(NULL)
    } else{
      
      # merge blobs; omit unmerged, small blobs; recompute NNDs
      mergedBlobs <- mergeBlobs(blobs_nn = ipTune_blobsDetected(), minDist = isolate(input$ipTune_minDist)) %>%
        sizeFiltBlobs(blobs = ., minSize = isolate(input$ipTune_sizeFilter)) %>%
        blobNeighbors(blobs = ., minDist = isolate(input$ipTune_minDist))
      
      # group by rows and columns (omits outliers); recompute NNDs
      groupedBlobs <- blobSort(blobs_nn_merge = mergedBlobs, minDist = isolate(input$ipTune_minDist)) %>%
        blobNeighbors(blobs = ., minDist = isolate(input$ipTune_minDist))
      
      # fill holes; detect colors
      missingBlobs(blobs_noFP_nn = groupedBlobs) %>%
        blobColors(
          imgPath = isolate(input$ipTune_image$datapath),
          compression = isolate(input$ipTune_compression),
          whichChannel = isolate(input$ipTune_colorChannel),
          yeastColonies = .
        ) %>%
        blobProcessing(yeastColonies = ., whichPlate = isolate(input$ipTune_plateSelection), whichImg = isolate(input$ipTune_image$name))
    }
  })
  
  ## display data.frame from IP-Tuning
  output$ipTune_colonyOutput <- DT::renderDataTable(DT::datatable({
    ipTune_yeastColonies()
  }, filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE
  ))
  
  ## download data.frame from IP-Tuning
  output$ipTune_dlTable <- downloadHandler(
    # set filename
    filename = function(){
      "colonyDetection.csv"
    },
    
    # set file content
    content = function(file){
      write.csv(ipTune_yeastColonies(), file, quote = TRUE, row.names = FALSE)
    }
  )
  
  ## download parameters from IP-Tuning
  output$ipTune_dlParams <- downloadHandler(
    # set filename
    filename = function(){
      "ipParameters.JSON"
    },
    
    # set file content
    content = function(file){
      
      ## make JSON file of parameters
      ipTune_parameters <- ipTune2JSON(
        compression = isolate(input$ipTune_compression),
        whichChannel = isolate(input$ipTune_objectChannel),
        invert = ifelse(isolate(input$ipTune_inversion) == "Yes", TRUE, FALSE),
        threshold_width = isolate(input$ipTune_thresholdWidth),
        threshold_height = isolate(input$ipTune_thresholdHeight),
        offset = isolate(input$ipTune_offset),
        brush_size = isolate(input$ipTune_brushSize),
        watershed_tolerance = isolate(input$ipTune_watershedTolerance),
        watershed_ext = isolate(input$ipTune_watershedExt),
        minDist = isolate(input$ipTune_minDist),
        sizeFilt = isolate(input$ipTune_sizeFilter),
        whichChannel_colorDetection = isolate(input$ipTune_colorChannel)
      )
      
      # write to file
      writeLines(ipTune_parameters, file)
    }
  )
  
  ### IP TUNE - VISUALIZATIONS
  ## original image
  ipTune_image <- eventReactive(input$ipTune_startIP, {
    if(is.null(isolate(input$ipTune_image))){
      return(NULL)
    } else{
      return(list(
        src = isolate(input$ipTune_image$datapath),
        width = "100%", height = "100%",
        alt = "Input image"
      ))
    }
  })
  
  output$ipTune_imageOutput <- renderImage({
    ipTune_image()
  }, deleteFile = FALSE)
  
  ## final image with color
  ipTune_ipFinal <- eventReactive(input$ipTune_startCleaning, {
    if(is.null(ipTune_yeastColonies())){
      return(NULL)
    } else{
      blobPlot_color(yeastColonies = ipTune_yeastColonies())
    }
  })
  
  output$ipTune_ipFinalOutput <- renderPlot({
    ipTune_ipFinal()
  })
  
  ## post-IP plots: NND point plot, NND density, size density
  ipTune_postIP <- eventReactive(input$ipTune_postIPPlot_start, {
    # set requirement
    # req(ipTune_blobsDetected())
    
    # create plots
    p1 <- blobPlot_nn(blobs_nn = ipTune_blobsDetected())
    p2 <- blobPlot_nndDist(blobs_nn = ipTune_blobsDetected())
    p3 <- blobPlot_sizeDist(blobs = ipTune_blobsDetected())
    p4 <- blobPlot_centroids(blobs = ipTune_blobsDetected())
    
    # arrange plots in grid
    p <- p1 + (p2 / p3) + p4
    
    # make plot
    p
  })
  
  output$ipTune_postIPPlot_output <- renderPlot({
    if(is.null(ipTune_postIP())){
      showNotification("Please start processing an image before proceeding!", type = "error", duration = NULL)
      return(NULL)
    } else{
      ipTune_postIP()
    }
  })
  
  ### IP BULK - PROCESSING
  ## update file selection
  observe({
    updateSelectizeInput(session = session, inputId = "ipBulk_imageOrder", choices = input$ipBulk_images$name)
  })
  
  ## validate supplied JSON file
  ipBulk_jsonValid <- eventReactive(input$ipBulk_start, {
    if(is.null(isolate(input$ipBulk_json))){
      return(NULL)
    } else{
      
      # validate JSON file; return TRUE if valid
      validateIP(schemaPath = "req/IPTuning_Schema.json", jsonPath = isolate(input$ipBulk_json$datapath))
    }
  })
  
  ## bulk processing
  ipBulk_bulk <- eventReactive(input$ipBulk_start, {
    if(is.null(isolate(input$ipBulk_images)) | is.null(isolate(input$ipBulk_json))){
      showNotification("Must upload at least one cropped image and JSON parameter file!", type = "error", duration = NULL)
      return(NULL)
    } else if(length(isolate(input$ipBulk_imageOrder)) != strsplit(isolate(input$ipBulk_plateNames), "\n") %>% unlist() %>% length()){
      showNotification("Must have one set of plate names per selected image!", type = "error", duration = NULL)
      return(NULL)
    } else if(length(isolate(input$ipBulk_imageOrder)) < 1){
      showNotification("Must select at least one image!", type = "error", duration = NULL)
      return(NULL)
    } else if(!ipBulk_jsonValid()){
      showNotification("JSON file not validated! Regenerate JSON file in IP Tuning tab!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # capture output
      withCallingHandlers({
        shinyjs::html("bulkIPTextOutput", "")
        
        ## bulk analysis
        bulkColonyDetection(
          imgPaths = isolate(input$ipBulk_images[match(input$ipBulk_imageOrder, input$ipBulk_images$name),]$datapath),
          jsonPath = isolate(input$ipBulk_json$datapath),
          plateNames = isolate(input$ipBulk_plateNames),
          imgNames = isolate(input$ipBulk_images[match(input$ipBulk_imageOrder, input$ipBulk_images$name),]$name)
        )
      },
      # redirect output to text in UI
      message = function(m){
        shinyjs::html(id = "bulkIPTextOutput", html = m$message, add = FALSE)
      })
    }
  })
  
  ## display bulk-processed image data
  output$ipBulk_bulkOutput <- DT::renderDataTable(DT::datatable({
    ipBulk_bulk()
  }, filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE
  ))
  
  ## download bulk-processed image data
  output$ipBulk_download <- downloadHandler(
    # set filename
    filename = function(){
      "colonyDetection_bulk.csv"
    },
    
    # set file content
    content = function(file){
      write.csv(ipBulk_bulk(), file, quote = T, row.names = F)
    }
  )
  
  ## visualize bulk-processed data
  ipBulk_qcPlots <- eventReactive(input$ipBulk_start, {
    
    # confirm that data has been processed
    if(is.null(ipBulk_bulk())){
      return(NULL)
    } else{
      
      # make grid of plots
      plateFeatures_ridgePlots(ipBulk_bulk())
    }
  })
  
  output$ipBulk_plotOutput <- renderPlot({
    ipBulk_qcPlots()
  })
  
  ### SINGLE-SCREEN ANALYSIS (HANDLES SINGLE OR QUADRANT COLONIES)
  ## sort by size and color
  singleScreen_sortDF <- eventReactive(input$ssStart, {
    
    # confirm file upload
    if(is.null(isolate(input$ssInput_screen)) | is.null(isolate(input$ssInput_library))){
      showNotification("Must upload files for screen and library!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # sort by size and color
      singleScreen_sort(
        dataPath = isolate(input$ssInput_screen$datapath),
        libraryPath = isolate(input$ssInput_library$datapath),
        voi = isolate(input$ss_featureSelection),
        topPerc = isolate(input$ss_topPercentile)
      )
    }
  })
  
  ## display data.frame from single-screen sorting
  output$ssOutput <- DT::renderDataTable(DT::datatable({
    singleScreen_sortDF()
  }, filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE
  ))
  
  ## download data.frame from single-screen sorting
  output$ssDownload <- downloadHandler(
    # set filename
    filename = function(){
      "singleScreen_sorted.csv"
    },
    
    # set file content
    content = function(file){
      write.csv(singleScreen_sortDF(), file, quote = TRUE, row.names = FALSE)
    }
  )
  
  ## rank plot
  singleScreen_ranked <- eventReactive(input$ssStart, {
    if(is.null(singleScreen_sortDF())){
      return(NULL)
    } else{
      
      # make plots with size and color vs rank
      singleScreen_rankPlot(ssDF = singleScreen_sortDF())
    }
  })
  
  output$ssPlotOutput_rank <- plotly::renderPlotly({
    singleScreen_ranked()
  })
  
  ## relationship plot
  singleScreen_relationship <- eventReactive(input$ssStart, {
    if(is.null(singleScreen_sortDF())){
      return(NULL)
    } else{
      
      # make plot of size vs color
      singleScreen_relationshipPlot(ssDF = singleScreen_sortDF())
    }
  })
  
  output$ssPlotOutput_relationship <- plotly::renderPlotly({
    singleScreen_relationship()
  })
  
  ## family enrichment plot
  ss_fep <- eventReactive(input$ss_fep_start, {
    if(is.null(singleScreen_sortDF())){
      showNotification("Finish analysis before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make plot
      singleScreen_familyEnrichment(libraryPath = isolate(input$ssInput_library$datapath), processedData = singleScreen_sortDF()) %>%
        singleScreen_familyEnrichment_format(processedData = singleScreen_sortDF(), familyEnrichData = .) %>%
        singleScreen_familyEnrichmentPlot(familyData = .)
    }
  })
  
  output$ssPlotOutput_fep <- plotly::renderPlotly({
    ss_fep()
  })
  
  ### DUAL-SCREEN ANALYSIS - WITHOUT REPLICATES
  ## merge and process data
  dualScreenSing_processDF <- eventReactive(input$dsSing_start, {
    
    # confirm file upload
    if(is.null(isolate(input$dsSing_controlInput)) | is.null(isolate(input$dsSing_testInput)) | is.null(isolate(input$dsSing_libraryInput))){
      showNotification("Must upload files for control, test, and library!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # merge and process data
      dualScreen_processData_sing(
        ctrlDataPath = isolate(input$dsSing_controlInput$datapath),
        testDataPath = isolate(input$dsSing_testInput$datapath),
        voi = isolate(input$dsSing_featureSelection),
        librDataPath = isolate(input$dsSing_libraryInput$datapath),
        featureRange = isolate(input$dsSing_featureRange)
      )
    }
  })
  
  ## display data.frame from dual-screen comparison
  output$dsSingOutput <- DT::renderDataTable(DT::datatable({
    dualScreenSing_processDF()
  }, filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE
  ))
  
  ## download data.frame from dual-screen comparison
  output$dsSing_download <- downloadHandler(
    
    # set filename
    filename = function(){
      "dualScreen_singleComparison.csv"
    },
    
    # set file content
    content = function(file){
      write.csv(dualScreenSing_processDF(), file, quote = TRUE, row.names = FALSE)
    }
  )
  
  ## sorted feature plot colored by absolute difference
  dualScreenSing_sfp_diff <- eventReactive(input$dsSing_sfp_diff_start, {
    if(is.null(dualScreenSing_processDF())){
      showNotification("Process data first!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make sorted feature plot
      dualScreenSing_sortedFeaturePlot_diff(processDF = dualScreenSing_processDF())
    }
  })
  
  output$dsSingPlotOutput_sfp_diff <- plotly::renderPlotly({
    dualScreenSing_sfp_diff()
  })
  
  ## sorted feature plot colored by family
  dualScreenSing_sfp_family <- eventReactive(input$dsSing_sfp_family_start, {
    if(is.null(dualScreenSing_processDF())){
      showNotification("Process data first!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make sorted feature plot
      dualScreenSing_sortedFeaturePlot_family(processDF = dualScreenSing_processDF())
    }
  })
  
  output$dsSingPlotOutput_sfp_family <- plotly::renderPlotly({
    dualScreenSing_sfp_family()
  })
  
  ## family enrichment plot
  dualScreenSing_fep <- eventReactive(input$dsSing_fep_start, {
    if(is.null(dualScreenSing_processDF())){
      showNotification("Finish comparisons before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make plot
      dualScreen_familyEnrichment(libraryPath = isolate(input$dsSing_libraryInput$datapath), processedData = dualScreenSing_processDF()) %>%
        dualScreen_familyEnrichment_format(processedData = dualScreenSing_processDF(), familyEnrichData = .) %>%
        dualScreen_familyEnrichmentPlot(familyData = .)
    }
  })
  
  output$dsSingPlotOutput_fep <- plotly::renderPlotly({
    dualScreenSing_fep()
  })
  
  ### DUAL-SCREEN ANALYSIS - WITH REPLICATES
  ## process control data
  dualScreenQuad_controlDF <- eventReactive(input$dsQuad_start, {
    
    # confirm file upload
    if(is.null(isolate(input$dsQuad_controlInput)) | is.null(isolate(input$dsQuad_testInput)) | is.null(isolate(input$dsQuad_libraryInput))){
      showNotification("Must upload files for control, test, and library!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # process control data for dual-screen quad analysis
      dualScreen_processData_quad(
        dataPath = isolate(input$dsQuad_controlInput$datapath),
        libraryPath = isolate(input$dsQuad_libraryInput$datapath),
        voi = isolate(input$dsQuad_featureSelection),
        experiment = "control"
      )
    }
  })
  
  ## process test data
  dualScreenQuad_testDF <- eventReactive(input$dsQuad_start, {
    
    # confirm file upload
    if(is.null(isolate(input$dsQuad_controlInput)) | is.null(isolate(input$dsQuad_testInput)) | is.null(isolate(input$dsQuad_libraryInput))){
      return(NULL)
    } else{
      
      # process test data for dual-screen quad analysis
      dualScreen_processData_quad(
        dataPath = isolate(input$dsQuad_testInput$datapath),
        libraryPath = isolate(input$dsQuad_libraryInput$datapath),
        voi = isolate(input$dsQuad_featureSelection),
        experiment = "test"
      )
    }
  })
  
  ## merge control and test data, perform comparative analyses
  dualScreenQuad_mergeDF <- eventReactive(input$dsQuad_start, {
    
    # confirm objects are not NULL
    if(is.null(dualScreenQuad_controlDF()) | is.null(dualScreenQuad_testDF())){
      return(NULL)
    } else{
      
      # merge CV-filtered data and perform comparative analyses
      dualScreen_mergeData_quad(
        ctrlDF = dualScreenQuad_controlDF() %>% dualScreen_cvFilter(cvFilter = isolate(input$dsQuad_controlSlider)),
        testDF = dualScreenQuad_testDF() %>% dualScreen_cvFilter(cvFilter = isolate(input$dsQuad_testSlider))
      ) %>% dualScreen_stattest()
    }
  })
  
  ## display data.frame from dual-screen comparison
  output$dsQuadOutput <- DT::renderDataTable(DT::datatable({
    dualScreenQuad_mergeDF()
  }, filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE
  ))
  
  ## download data.frame from dual-screen comparison
  output$dsQuad_download <- downloadHandler(
    
    # set filename
    filename = function(){
      "dualScreen_quadComparison.csv"
    },
    
    # set file content
    content = function(file){
      write.csv(dualScreenQuad_mergeDF(), file, quote = TRUE, row.names = FALSE)
    }
  )
  
  ## CV plots in the main panel
  dualScreenQuad_cvPlots <- eventReactive(input$dsQuad_start, {
    if(is.null(dualScreenQuad_controlDF()) | is.null(dualScreenQuad_testDF())){
      return(NULL)
    } else{
      
      # make plots showing the effects of the CV filter
      dualScreen_cvPlots(
        ctrlDF = dualScreenQuad_controlDF(), testDF = dualScreenQuad_testDF(),
        voi = isolate(input$dsQuad_featureSelection),
        ctrlFilter = isolate(input$dsQuad_controlSlider), testFilter = isolate(input$dsQuad_testSlider)
      )
    }
  })
  
  output$dsQuadPlotOutput_cv <- plotly::renderPlotly({
    dualScreenQuad_cvPlots()
  })
  
  ## volcano plot in the main panel
  dualScreenQuad_volcanoPlot <- eventReactive(input$dsQuad_start, {
    if(is.null(dualScreenQuad_mergeDF())){
      return(NULL)
    } else{
      
      # make volcano plot after performing comparative statistics
      dualScreen_volcanoPlot(pvalData = dualScreenQuad_mergeDF())
    }
  })
  
  output$dsQuadPlotOutput_volcano <- plotly::renderPlotly({
    dualScreenQuad_volcanoPlot()
  })
  
  ## sorted feature plot with p-value colors in pop-up plot
  dualScreenQuad_sfp_pv <- eventReactive(input$dsQuad_sfp_pv_start, {
    if(is.null(dualScreenQuad_mergeDF())){
      showNotification("Finish comparisons before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make plot
      dualScreen_sortedFeaturePlot_pval(pvalData = dualScreenQuad_mergeDF())
    }
  })
  
  output$dsQuadPlotOutput_sfp_pv <- plotly::renderPlotly({
    dualScreenQuad_sfp_pv()
  })
  
  ## sorted feature plot with family colors in pop-up plot
  dualScreenQuad_sfp_family <- eventReactive(input$dsQuad_sfp_family_start, {
    if(is.null(dualScreenQuad_mergeDF())){
      showNotification("Finish comparisons before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make plot
      dualScreen_sortedFeaturePlot_family(pvalData = dualScreenQuad_mergeDF())
    }
  })
  
  output$dsQuadPlotOutput_sfp_family <- plotly::renderPlotly({
    dualScreenQuad_sfp_family()
  })
  
  ## family enrichment plot
  dualScreenQuad_fep <- eventReactive(input$dsQuad_fep_start, {
    if(is.null(dualScreenQuad_mergeDF())){
      showNotification("Finish comparisons before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make plot
      dualScreen_familyEnrichment(libraryPath = isolate(input$dsQuad_libraryInput$datapath), processedData = dualScreenQuad_mergeDF()) %>%
        dualScreen_familyEnrichment_format(processedData = dualScreenQuad_mergeDF(), familyEnrichData = .) %>%
        dualScreen_familyEnrichmentPlot(familyData = .)
    }
  })
  
  output$dsQuadPlotOutput_fep <- plotly::renderPlotly({
    dualScreenQuad_fep()
  })
  
  ### BIOINFORMATICS ANALYSIS (GO AND KEGG)
  ## GO enrichment analysis
  goDF <- eventReactive(input$bioinf_start, {
    
    # confirm file upload
    if(is.null(isolate(input$bioinf_input))){
      showNotification("Must upload file!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # GO enrichment analysis
      goAnalysis(dataPath = isolate(input$bioinf_input$datapath), ontologySelection = "BP")
    }
  })
  
  ## display data.frame from single-screen sorting
  output$bioinf_GO <- DT::renderDataTable(DT::datatable({
    goDF()
  }, filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE
  ))
  
  ## download data.frame from single-screen sorting
  output$bioinf_download_GO <- downloadHandler(
    # set filename
    filename = function(){
      "goEnrichment.csv"
    },
    
    # set file content
    content = function(file){
      write.csv(goDF(), file, quote = TRUE, row.names = FALSE)
    }
  )
  
  ## KEGG enrichment analysis
  keggDF <- eventReactive(input$bioinf_start, {
    
    # confirm file upload
    if(is.null(isolate(input$bioinf_input))){
      showNotification("Must upload file!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # GO enrichment analysis
      keggAnalysis(
        dataPath = isolate(input$bioinf_input$datapath),
        pathwaysListPath = "req/ath_pathways-list.rds",
        genesByPathwayPath = "req/ath_genes-by-pathway.rds"
      )
    }
  })
  
  ## display data.frame from single-screen sorting
  output$bioinf_KEGG <- DT::renderDataTable(DT::datatable({
    keggDF()
  }, filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE
  ))
  
  ## download data.frame from single-screen sorting
  output$bioinf_download_KEGG <- downloadHandler(
    # set filename
    filename = function(){
      "keggEnrichment.csv"
    },
    
    # set file content
    content = function(file){
      write.csv(keggDF(), file, quote = TRUE, row.names = FALSE)
    }
  )
  
  ### CLOSE APP WHEN BROWSER CLOSES
  session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)
