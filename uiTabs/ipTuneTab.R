# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## style to use for buttons
buttonStyle <- "padding: 5px 14px 5px 14px; margin: 5px 5px 5px 5px;"

## define tab
ipTuningTab <- tabPanel(
  "IP-Tune",
  
  # need a sidebar and a main panel
  sidebarLayout(
    
    # define sidebar
    sidebarPanel(
      ## INPUT DATA
      fileInput(
        "ipTune_image", label = strong("Choose sample image:"),
        multiple = FALSE, placeholder = "JPEG or PNG",
        accept = c(".jpg", ".jpeg", ".JPG", ".JPEG", ".png", ".PNG")
      ),
      
      ## DEFAULT OR CUSTOM PARAMETERS
      radioButtons("ipTune_customParamters", label = strong("Use custom parameters for object detection?"), choices = c("Yes", "No"), selected = "No", inline = TRUE),
      shinyBS::bsTooltip("ipTune_customParamters", "Defaults are specific to sample data and will likely need to be adjusted."),
      
      ## CUSTOM PARAMETERS
      conditionalPanel(
        condition = "input.ipTune_customParamters == 'Yes'",
        
        ## OBJECT DETECTION
        # compression
        sliderInput("ipTune_compression", label = strong("Image compression:"), min = 1000, max = 3000, value = 1500, step = 100, ticks = FALSE),
        shinyBS::bsTooltip(
          "ipTune_compression",
          "
          To what value should the image width be scaled (while preserving the aspect ratio)?
          Smaller values decrease runtime but may result in information loss. Downstream parameters heavily depend on this selection.
          "
        ),
        
        # channel selection
        selectInput("ipTune_objectChannel", label = strong("Channel selection:"), choices = c("gray", "red", "green", "blue")),
        shinyBS::bsTooltip("ipTune_objectChannel", "To which color channel should the image be converted?"),
        
        # inversion selection
        radioButtons("ipTune_inversion", label = strong("Invert image?"), choices = c("Yes", "No"), selected = "No", inline = TRUE),
        shinyBS::bsTooltip(
          "ipTune_inversion", "Should the image be inverted? Not recommended when foreground is brighter than background."
        ),
        
        # threshold width / height, offset, brush size, and watershed tolerance
        sliderInput("ipTune_thresholdWidth", label = strong("Threshold width:"), min = 3, max = 151, value = 27, step = 2, ticks = FALSE),
        shinyBS::bsTooltip("ipTune_thresholdWidth", "Half width of moving window for adaptive thresholding."),
        
        sliderInput("ipTune_thresholdHeight", label = strong("Threshold height:"), min = 3, max = 151, value = 27, step = 2, ticks = FALSE),
        shinyBS::bsTooltip("ipTune_thresholdHeight", "Half height of moving window for adaptive thresholding."),
        
        sliderInput("ipTune_offset", label = strong("Offset:"), min = 0, max = 0.5, value = 0.05, step = 0.01, ticks = FALSE),
        shinyBS::bsTooltip("ipTune_offset", "Threshold offset from averaged value in moving window. Value helps binarize the image."),
        
        sliderInput("ipTune_brushSize", label = strong("Brush size:"), min = 3, max = 21, value = 5, step = 2, ticks = FALSE),
        shinyBS::bsTooltip("ipTune_brushSize", "Size of disc-shaped brush to mask image."),
        
        sliderInput("ipTune_watershedTolerance", label = strong("Watershed tolerance:"), min = 1, max = 21, value = 3, step = 1, ticks = FALSE),
        shinyBS::bsTooltip("ipTune_watershedTolerance", "Minimum object height."),
        
        sliderInput("ipTune_watershedExt", label = strong("Watershed radius:"), min = 1, max = 21, value = 3, step = 1, ticks = FALSE),
        shinyBS::bsTooltip("ipTune_watershedExt", "Pixel radius to detect neighboring objects.")
      ),
      
      ## IP START BUTTON FOR IMAGE PROCESSING
      fluidRow(
        align = "center",
        
        # button to start image processing
        actionButton("ipTune_startIP", label = strong("Start IP"), icon = icon("play"), style = buttonStyle),
        shinyBS::bsTooltip("ipTune_startIP", "Start Watershed object detection step."),
        
        # button for first set of plots: NND point plot, NND density, and size density
        actionButton("ipTune_postIPPlot_start", label = strong("Post-IP Plots"), icon = icon("th"), style = buttonStyle),
        shinyBS::bsTooltip("ipTune_postIPPlot_start", "Shows plots related NND and size to inform downstream parameter selection."),
        shinyBS::bsModal(
          "ipTune_postIPPlot_window", "Post-IP Plots", "ipTune_postIPPlot_start", size = "large",
          shinycssloaders::withSpinner(plotOutput("ipTune_postIPPlot_output"))
        )
      ),
      
      ## OBJECT CLEANING AND SORTING
      # threshold for nearest-neighbor distances
      sliderInput(
        "ipTune_minDist", label = strong("Max. distance for merging:"),
        min = 2, max = 100, value = 20, step = 2, ticks = FALSE
      ),
      shinyBS::bsTooltip(
        "ipTune_minDist",
        "Objects with nearest-neighbor distances below this threshold will be merged."
      ),
      
      # slider for size filter
      sliderInput("ipTune_sizeFilter", label = strong("Minimum size filter:"), min = 10, max = 1000, value = 100, step = 10, ticks = FALSE),
      shinyBS::bsTooltip("ipTune_sizeFilter", "Objects smaller than this size are likely contamination and will be omitted."),
      
      ## COLOR DETECTION
      selectInput("ipTune_colorChannel", label = strong("Channel for color detection:"), choices = c("gray", "red", "green", "blue"), selected = "green"),
      shinyBS::bsTooltip("ipTune_colorChannel", "Which color channel should be used to detect color? Green is recommended for a red/white screen."),
      
      ## PLATE SELECTION
      textInput("ipTune_plateSelection", label = strong("Plate name:"), placeholder = "1", value = "1"),
      shinyBS::bsTooltip("ipTune_plateSelection", "Which library plate is used in this experimental plate?"),
      
      ## IP FINISH BUTTONS
      fluidRow(
        align = "center",
        
        # start button to finish
        column(4, actionButton("ipTune_startCleaning", label = strong("Finish IP"), icon = icon("play"), style = buttonStyle)),
        
        # download button for data.table
        column(4, downloadButton("ipTune_dlTable", label = strong("Table"), style = buttonStyle)),
        
        # download button for IP parameters in JSON file
        column(4, downloadButton("ipTune_dlParams", label = strong("JSON"), style = buttonStyle))
      )
    ),
    
    # define main panel
    mainPanel(
      
      # row for input image and final image processing results
      fluidRow(
        column(6, shinycssloaders::withSpinner(imageOutput("ipTune_imageOutput"))),
        column(6, shinycssloaders::withSpinner(plotOutput("ipTune_ipFinalOutput")))
      ),
      
      # row for data table
      fluidRow(
        shinycssloaders::withSpinner(DT::dataTableOutput("ipTune_colonyOutput"))
      )
    )
  )
)