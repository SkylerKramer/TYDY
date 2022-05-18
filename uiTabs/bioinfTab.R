# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## style to use for buttons
buttonStyle <- "padding: 5px 14px 5px 14px; margin: 5px 5px 5px 5px;"

## define tab
bioinfTab <- tabPanel(
  "Bioinformatics",
  
  # need a sidebar and a main panel
  sidebarLayout(
    
    # define sidebar
    sidebarPanel(
      
      # file input
      fileInput(
        "bioinf_input", label = strong("Choose analyzed data:"),
        multiple = TRUE, placeholder = "CSV file",
        accept = ".csv"
      ),
      
      # start/download buttons
      actionButton("bioinf_start", label = strong("Start"), icon = icon("play"), style = buttonStyle),
      downloadButton("bioinf_download_GO", label = strong("Download GO"), style = buttonStyle),
      downloadButton("bioinf_download_KEGG", label = strong("Download KEGG"), style = buttonStyle),
      
      # add space
      tags$br(),
      tags$br(),
      
      # analysis notes
      strong(HTML("*GO (BP) and KEGG analyses currently only support <em>A. thaliana</em>."))
    ),
    
    # define main panel
    mainPanel(
      shinycssloaders::withSpinner(DT::dataTableOutput("bioinf_GO")),
      shinycssloaders::withSpinner(DT::dataTableOutput("bioinf_KEGG"))
    )
  )
)