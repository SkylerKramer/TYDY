# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

# source the sub-tabs
source("uiTabs/ipTuneTab.R")
source("uiTabs/ipBulkTab.R")

# merge the sub-tabs
ipTab <- tabPanel(
  "Image Processing",
  tabsetPanel(
    ipTuningTab,
    ipBulkTab
  )
)