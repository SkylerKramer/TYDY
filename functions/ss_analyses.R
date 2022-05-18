# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## THIS FUNCTION SORTS DATA BY SIZE AND COLOR FROM A SINGLE SCREEN ANALYSIS
singleScreen_sort <- function(dataPath = NULL, libraryPath = NULL, voi = NULL, topPerc = NULL){
  # read library file
  libraryDF <- read.csv(libraryPath)
  
  # read CSV and omit NAs (missing points have Size = 0 and Color = NA)
  yeastDF <- read.csv(dataPath) %>% na.omit()
  
  # divide Size and Color by each plate's median
  yeastDF <- yeastDF %>%
    dplyr::mutate(Plate = strsplit(Coordinate, split = "-") %>% lapply(function(x) x[1]) %>% unlist()) %>%
    dplyr::group_by(Plate) %>%
    dplyr::mutate(Size = Size / median(Size), Color = Color / median(Color)) %>%
    dplyr::ungroup() %>%
    dplyr::select(!Plate)
  
  # merge with library file and get averages if there are replicates
  yeastDF <- yeastDF %>%
    dplyr::left_join(libraryDF, by = "Coordinate") %>%
    dplyr::group_by(Identifier) %>%
    dplyr::mutate(Size = mean(Size), Color = mean(Color), Coordinate = paste(Coordinate, collapse = " : ")) %>%
    dplyr::select(!c(X, Y)) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()
  
  # get rank/percentile for size and color; round values; select values of interest
  yeastDF <- yeastDF %>%
    
    dplyr::arrange(dplyr::desc(Size)) %>%
    tibble::rowid_to_column("Size_Rank") %>%
    dplyr::mutate(Size_Perc = round(Size_Rank / nrow(yeastDF) * 100, digits = 3)) %>%
    
    dplyr::arrange(dplyr::desc(Color)) %>%
    tibble::rowid_to_column("Color_Rank") %>%
    dplyr::mutate(Color_Perc = round(Color_Rank / nrow(yeastDF) * 100, digits = 3)) %>%
    
    dplyr::mutate(Size = round(Size, 3), Color = round(Color, 3)) %>%
    
    dplyr::select(Identifier, Common_Name, Family, Size, Size_Rank, Size_Perc, Color, Color_Rank, Color_Perc, ImagePath, Coordinate) %>%
    dplyr::arrange(Coordinate)
  
  # apply significance threshold to variable of interest
  yeastDF <- yeastDF %>%
    dplyr::mutate(
      TF.type = dplyr::case_when(
        !!as.symbol(glue::glue("{voi}_Perc")) <= topPerc ~ "Sig.",
        TRUE ~ "Not Sig."
      )
    )
  
  # return merged file
  return(yeastDF)
}

## THIS FUNCTION PERFORMS THE FISHER'S EXACT TEST FOR FAMILY ENRICHMENT IN A USER-DEFINED THRESHOLD
singleScreen_familyEnrichment <- function(libraryPath = NULL, processedData = NULL){
  # read library file
  libraryDF <- read.csv(libraryPath)
  
  # get number of unique TFs
  uniqueTFs <- length(unique(libraryDF$Identifier))
  
  # count family members in original library
  familyCounts <- libraryDF %>%
    dplyr::select(Identifier, Family) %>%
    dplyr::distinct() %>%
    dplyr::group_by(Family) %>%
    dplyr::count()
  
  # omit any families in processed data that do not exist in library data
  processedData <- processedData[which(processedData$Family %in% familyCounts$Family),]
  
  # get number of TFs whose values are significantly up or down relative to the reference screen
  numSig <- processedData %>% dplyr::filter(TF.type == "Sig.") %>% nrow()
  
  # count number of significant TFs per family; initialize extra columns; add total family counts
  familySummary <- processedData %>%
    dplyr::filter(TF.type != "Not Sig.") %>%
    dplyr::group_by(Family) %>%
    dplyr::summarise(TFs_Sig = sum(TF.type == "Sig.")) %>%
    dplyr::left_join(familyCounts, by = "Family") %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::relocate(n, .before = TFs_Sig) %>%
    dplyr::mutate(
      Expected_Sig = round(n * numSig / uniqueTFs, 2),
      Sig_pvalue = NA
    ) %>%
    dplyr::ungroup()
  
  # perform Fisher's exact test for each family / tf.type
  for(i in 1:nrow(familySummary)){
    
    #'matrix[1,1] = number of activators or repressors in family
    #'matrix[2,1] = number of activators or repressors not in family
    #'matrix[1,2] = number of family members that are not activators or repressors
    #'matrix[2,2] = number of non-family members that are not activators or repressors
    
    # FET p-value for activators in ith family
    familySummary$Sig_pvalue[i] <- fisher.test(
      matrix(c(
        familySummary$TFs_Sig[i],
        numSig - familySummary$TFs_Sig[i],
        familySummary$n[i] - familySummary$TFs_Sig[i],
        uniqueTFs - numSig - familySummary$n[i] + familySummary$TFs_Sig[i]
      ), nr = 2),
      alternative = "greater"
    )$p.value %>% round(., 5)
  }
  
  # return family summary
  return(familySummary)
}

## This function formats the family enrichment data.frame for subsequent plotting
singleScreen_familyEnrichment_format <- function(processedData = NULL, familyEnrichData = NULL){
  
  # remove insignificant TFs
  processedData <- processedData %>% dplyr::filter(TF.type != "Not Sig.")
  
  # data.frame for sig. up/down TFs per family; merge with enrichment data
  familyData <- table(processedData$Family, processedData$TF.type) %>%
    as.data.frame() %>%
    dplyr::filter(Freq != 0) %>%
    dplyr::rename(Family = Var1, TF.type = Var2, TypeCount = Freq) %>%
    # dplyr::mutate(TypeCount = ifelse(TF.type == "Sig. Up", TypeCount, -1*TypeCount)) %>%
    tidyr::pivot_wider(names_from = TF.type, values_from = TypeCount) %>%
    dplyr::left_join(familyEnrichData, by = "Family")
  
  # sort data.frame by family size and name; reorder Family as factor; add columns to denote significance
  familyData <- familyData %>%
    dplyr::arrange(n, Family) %>%
    dplyr::mutate(
      Family = factor(Family, levels = unique(Family)),
      Sig = ifelse(Sig_pvalue < 0.05, "Yes", "No")
    )
  
  # return formatted data.frame
  return(familyData)
}
