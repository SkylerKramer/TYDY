# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## style to use for buttons
buttonStyle <- "padding: 5px 14px 5px 14px; margin: 5px 5px 5px 5px;"

## define tab
ipBulkTab <- tabPanel(
  "IP-Bulk",
  
  # need a sidebar and a main panel
  sidebarLayout(
    
    # define sidebar
    sidebarPanel(
      ## INPUT IMAGES
      fileInput(
        "ipBulk_images", label = strong("Choose images:"),
        multiple = TRUE, placeholder = "JPEG or PNG",
        accept = c(".jpg", ".jpeg", ".JPG", ".JPEG", ".png", ".PNG")
      ),
      
      ## INPUT JSON
      fileInput(
        "ipBulk_json", label = strong("Choose JSON file with IP parameters:"),
        multiple = FALSE, placeholder = "JSON",
        accept = c(".JSON")
      ),
      
      ## REORDER INPUT FILES
      selectizeInput("ipBulk_imageOrder", label = strong("Select file order."), choices = "*", multiple = TRUE),
      
      ## INPUT FILE NAMES
      textAreaInput("ipBulk_plateNames", label = strong("Enter plate names (one per line):"), placeholder = "1\n2\nEtc.", rows = 6),
      
      ## START BUTTON FOR BULK IP
      fluidRow(
        align = "center",
        
        # start button
        column(6, actionButton("ipBulk_start", label = strong("Start"), icon = icon("play"), style = buttonStyle)),
        
        # download button
        column(6, downloadButton("ipBulk_download", label = strong("Download"), style = buttonStyle))
      ),
      
      # show console output
      shinyjs::useShinyjs(),
      fluidRow(align = "center", strong(textOutput("bulkIPTextOutput")))
    ),
    
    # define main panel
    mainPanel(
      shinycssloaders::withSpinner(plotOutput("ipBulk_plotOutput", height = "600px")),
      shinycssloaders::withSpinner(DT::dataTableOutput("ipBulk_bulkOutput"))
    )
  )
)