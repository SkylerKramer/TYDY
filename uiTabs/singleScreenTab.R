# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## style to use for buttons
buttonStyle <- "padding: 5px 14px 5px 14px; margin: 5px 5px 5px 5px;"

singleScreenTab <- tabPanel(
  "Single Screen",
  
  # need a sidebar and a main panel
  sidebarLayout(
    
    # define sidebar
    sidebarPanel(
      
      # screen upload
      fileInput(
        "ssInput_screen", label = strong("Upload single screen:"),
        multiple = FALSE, placeholder = "XLSX or CSV file",
        accept = c(".csv")
      ),
      
      # library upload
      fileInput(
        "ssInput_library", label = strong("Upload library:"),
        multiple = FALSE, placeholder = "XLSX or CSV file",
        accept = c(".csv")
      ),
      
      # feature selection dropdown
      selectInput("ss_featureSelection", label = strong("Analyze which feature?"), choices = c("Size", "Color"), selected = "Color"),
      shinyBS::bsTooltip("ss_featureSelection", "Should 'significance' be determined with size or color?"),
      
      # slider input to set range for feature difference
      sliderInput("ss_topPercentile", label = strong("Significance threshold:"), min = 5, max = 95, value = 15, step = 5, ticks = FALSE),
      shinyBS::bsTooltip("ss_topPercentile", "Percentiles less than or equal to this value are considered 'significant'."),
      
      # start and download buttons
      actionButton("ssStart", label = strong("Start"), icon = icon("play"), style = buttonStyle),
      downloadButton("ssDownload", label = strong("Download"), style = buttonStyle),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # popup plot
      actionButton("ss_fep_start", label = strong("FEP"), icon = icon("play"), style = buttonStyle),
      shinyBS::bsTooltip("ss_fep_start", "Shows a family enrichment plot."),
      shinyBS::bsModal(
        "fepSS_Window", "Family Enrichment Plot", "ss_fep_start",
        size = "large", shinycssloaders::withSpinner(plotly::plotlyOutput("ssPlotOutput_fep"))
      )
    ),
    
    # define main panel
    mainPanel(
      
      # row for plots
      fluidRow(
        column(6, shinycssloaders::withSpinner(plotly::plotlyOutput("ssPlotOutput_rank"))),
        column(6, shinycssloaders::withSpinner(plotly::plotlyOutput("ssPlotOutput_relationship")))
      ),
      
      # data.table with rankings
      shinycssloaders::withSpinner(DT::dataTableOutput("ssOutput"))
    )
  )
)