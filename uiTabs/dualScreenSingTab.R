# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## style to use for buttons
buttonStyle <- "padding: 5px 14px 5px 14px; margin: 5px 5px 5px 5px;"

dualScreenSingTab <- tabPanel(
  "DS-Singleton",
  
  # need a sidebar and a main panel
  sidebarLayout(
    
    # define sidebar
    sidebarPanel(
      
      # control data upload
      fileInput(
        "dsSing_controlInput", label = strong("Choose control data:"),
        multiple = FALSE, placeholder = "CSV file",
        accept = ".csv"
      ),
      
      # test data upload
      fileInput(
        "dsSing_testInput", label = strong("Choose test data:"),
        multiple = FALSE, placeholder = "CSV file",
        accept = ".csv"
      ),
      
      # library data upload
      fileInput(
        "dsSing_libraryInput", label = strong("Choose library data:"),
        multiple = FALSE, placeholder = "CSV file",
        accept = ".csv"
      ),
      
      # feature selection dropdown
      selectInput("dsSing_featureSelection", label = strong("Analyze which feature?"), choices = c("Size", "Color"), selected = "Color"),
      shinyBS::bsTooltip("dsSing_featureSelection", "Should size or color be analyzed?"),
      
      # slider input to set range for feature difference
      sliderInput("dsSing_featureRange", label = strong("Quantiles of interest:"), min = 0.05, max = 0.95, value = c(0.05, 0.95), step = 0.05, ticks = FALSE),
      shinyBS::bsTooltip("dsSing_featureRange", "Colonies with values outside of this range are considered significant."),
      
      # start/download buttons
      actionButton("dsSing_start", label = strong("Start"), icon = icon("play"), style = buttonStyle),
      downloadButton("dsSing_download", label = strong("Download"), style = buttonStyle),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # popup plots
      actionButton("dsSing_sfp_diff_start", label = strong("SFP-Diff"), icon = icon("play"), style = buttonStyle),
      shinyBS::bsTooltip("dsSing_sfp_diff_start", "Shows a sorted feature plot colored by absolute difference."),
      shinyBS::bsModal(
        "sfpSing_diff_Window", "Sorted Feature Plot - Absolute Difference", "dsSing_sfp_diff_start",
        size = "large", shinycssloaders::withSpinner(plotly::plotlyOutput("dsSingPlotOutput_sfp_diff"))
      ),
      
      actionButton("dsSing_sfp_family_start", label = strong("SFP-Family"), icon = icon("play"), style = buttonStyle),
      shinyBS::bsTooltip("dsSing_sfp_family_start", "Shows a sorted feature plot colored by family."),
      shinyBS::bsModal(
        "sfpSing_family_Window", "Sorted Feature Plot - Family", "dsSing_sfp_family_start",
        size = "large", shinycssloaders::withSpinner(plotly::plotlyOutput("dsSingPlotOutput_sfp_family"))
      ),
      
      actionButton("dsSing_fep_start", label = strong("FEP"), icon = icon("play"), style = buttonStyle),
      shinyBS::bsTooltip("dsSing_fep_start", "Shows a family enrichment plot."),
      shinyBS::bsModal(
        "fepSing_Window", "Family Enrichment Plot", "dsSing_fep_start",
        size = "large", shinycssloaders::withSpinner(plotly::plotlyOutput("dsSingPlotOutput_fep"))
      )
    ),
    
    # define main panel
    mainPanel(
      
      # data.table with rankings
      shinycssloaders::withSpinner(DT::dataTableOutput("dsSingOutput"))
    )
  )
)