# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## style to use for buttons
buttonStyle <- "padding: 5px 14px 5px 14px; margin: 5px 5px 5px 5px;"

dualScreenQuadTab <- tabPanel(
  "DS-Replicates",
  
  # need a sidebar and a main panel
  sidebarLayout(
    
    # define sidebar
    sidebarPanel(
      
      # control data upload
      fileInput(
        "dsQuad_controlInput", label = strong("Choose control data:"),
        multiple = FALSE, placeholder = "CSV file",
        accept = ".csv"
      ),
      
      # test data upload
      fileInput(
        "dsQuad_testInput", label = strong("Choose test data:"),
        multiple = FALSE, placeholder = "CSV file",
        accept = ".csv"
      ),
      
      # library data upload
      fileInput(
        "dsQuad_libraryInput", label = strong("Choose library data:"),
        multiple = FALSE, placeholder = "CSV file",
        accept = ".csv"
      ),
      
      # feature selection dropdown
      selectInput("dsQuad_featureSelection", label = strong("Analyze which feature?"), choices = c("Size", "Color"), selected = "Size"),
      shinyBS::bsTooltip("dsQuad_featureSelection", "Should size or color be analyzed?"),
      
      # max CV for control
      sliderInput(
        "dsQuad_controlSlider", label = strong("Max. CV threshold for control data:"),
        min = 0, max = 1, value = 0.5, step = 0.01, ticks = FALSE
      ),
      shinyBS::bsTooltip("sliderControlData", "What is the largest coefficient of variation (CV) to consider?"),
      
      # max CV for test
      sliderInput(
        "dsQuad_testSlider", label = strong("Max. CV threshold for test data:"),
        min = 0, max = 1, value = 0.5, step = 0.01, ticks = FALSE
      ),
      shinyBS::bsTooltip("sliderTestData", "What is the largest coefficient of variation (CV) to consider?"),
      
      # start/download buttons
      actionButton("dsQuad_start", label = strong("Start"), icon = icon("play"), style = buttonStyle),
      downloadButton("dsQuad_download", label = strong("Download"), style = buttonStyle),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # buttons for popup plots
      actionButton("dsQuad_sfp_pv_start", label = strong("SFP-PV"), icon = icon("play"), style = buttonStyle),
      shinyBS::bsTooltip("dsQuad_sfp_pv_start", "Shows a sorted feature plot colored by p-value."),
      shinyBS::bsModal(
        "sfp_pv_Window", "Sorted Feature Plot - PV", "dsQuad_sfp_pv_start",
        size = "large", shinycssloaders::withSpinner(plotly::plotlyOutput("dsQuadPlotOutput_sfp_pv"))
      ),
      
      actionButton("dsQuad_sfp_family_start", label = strong("SFP-Family"), icon = icon("play"), style = buttonStyle),
      shinyBS::bsTooltip("dsQuad_sfp_family_start", "Shows a sorted feature plot colored by family."),
      shinyBS::bsModal(
        "sfp_family_Window", "Sorted Feature Plot - Family", "dsQuad_sfp_family_start",
        size = "large", shinycssloaders::withSpinner(plotly::plotlyOutput("dsQuadPlotOutput_sfp_family"))
      ),
      
      actionButton("dsQuad_fep_start", label = strong("FEP"), icon = icon("play"), style = buttonStyle),
      shinyBS::bsTooltip("dsQuad_fep_start", "Shows a family enrichment plot."),
      shinyBS::bsModal(
        "fepQuad_Window", "Family Enrichment Plot", "dsQuad_fep_start",
        size = "large", shinycssloaders::withSpinner(plotly::plotlyOutput("dsQuadPlotOutput_fep"))
      )
    ),
    
    # define main panel
    mainPanel(
      # row for plots
      fluidRow(
        column(8, shinycssloaders::withSpinner(plotly::plotlyOutput("dsQuadPlotOutput_cv"))),
        column(4, shinycssloaders::withSpinner(plotly::plotlyOutput("dsQuadPlotOutput_volcano")))
      ),
      
      # data.table with rankings
      shinycssloaders::withSpinner(DT::dataTableOutput("dsQuadOutput"))
    )
  )
)