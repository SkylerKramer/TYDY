# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

aboutTab <- tabPanel(
  "About",
  
  # title
  h1("About TYDY"),
  
  # general description
  p(
    "
    TYDY - Toolkit for Yeast Data analYsis - is a web tool designed to facilitate image processing, single-screen analysis, 
    dual-screen comparisons, and downstream bioinformatics tasks for plates of gridded yeast colonies.
    Image processing is based on Watershed object detection and returns both size and color. Single-screen analyses are based
    on colony rankings, and dual-screen analyses are based on comparisons between test screens and reference screens.
    Downstream bioinformatics analyses include GO/KEGG/REAC enrichment and queries to STRING in the case of assays specific to
    protein-protein interaction detection.
    "
  ),
  
  # inputs and outputs
  p(HTML(paste0(
    "To get started, supply a tightly cropped image to the Image Processing - Tuning module. Sample images can be downloaded ",
    a(href = "https://github.com/SkylerKramer/", "here", .noWS = "outside", target = "_blank"),
    "."
  ))),
  
  # availability section
  h3("Availability"),
  
  # list of accession points
  tags$ul(
    tags$li(HTML(paste0(
      "Website (direct access): ",
      a(href = "https://github.com/SkylerKramer/", "tydy.missouri.edu", target = "blank")
    ))),
    
    tags$li(HTML(paste0(
      "Docker (local download of platform): ",
      a(href = "https://hub.docker.com/repository/docker/skylerkramer", "skylerkramer/tydy", target = "blank")
    ))),
    
    tags$li(HTML(paste0(
      "Github (code download): ",
      a(href = "https://github.com/SkylerKramer/ShinyY2H/", "SkylerKramer/TYDY", target = "blank")
    ))),
    
    tags$li(HTML(paste0(
      "User guide: ",
      a(href = "https://github.com/SkylerKramer/", "UserGuide.pdf", target = "_blank")
    )))
  ),
  
  # citation
  h3("Citation"),
  p("If you use or modify YDAP, please cite: https://tydy.missouri.edu"),
  
  # contact section
  h3("Contact"),
  p(
    "TYDY is maintained by Skyler T. Kramer and David Mendoza-Cozatl. To report bugs or request features, please use one of the following: "
  ),
  
  # list of contacts
  tags$ul(
    tags$li(HTML(paste0(
      "Github: ",
      a(href = "https://github.com/SkylerKramer/TYDY/issues", "SkylerKramer/TYDY", target = "blank")
    ))),
    
    tags$li("Email: stk7c9@umsystem.edu")
  ),
  
  # update section
  h3("The most recent version of YDAP is from Apr. 25, 2022.")
)