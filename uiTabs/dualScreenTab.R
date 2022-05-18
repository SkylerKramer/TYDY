# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

# source the sub-tabs
source("uiTabs/dualScreenSingTab.R")
source("uiTabs/dualScreenQuadTab.R")

# merge the sub-tabs
dualScreenTab <- tabPanel(
  "Dual Screen",
  tabsetPanel(
    dualScreenSingTab,
    dualScreenQuadTab
  )
)