# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

### TO ANALYZE TWO SCREENS WITHOUT REPLICATES
## This function merges and processes control, test, and library data for dual screen comparisons without replicates
dualScreen_processData_sing <- function(ctrlDataPath = NULL, testDataPath = NULL, voi = NULL, librDataPath = NULL, featureRange = NULL){
  
  # small function to process control and test data
  dataPrep <- function(dataPath = NULL, voi = NULL){
    
    # read file and select/rename variable of interest, group by plate, divide feature by median of its plate
    read.csv(dataPath) %>%
      dplyr::select(Coordinate, !!as.symbol(voi)) %>%
      dplyr::rename(Feature = !!as.symbol(voi)) %>%
      tidyr::drop_na() %>%
      
      dplyr::mutate(Plate = strsplit(Coordinate, split = "-") %>% lapply(function(x) x[1]) %>% unlist()) %>%
      dplyr::group_by(Plate) %>%
      dplyr::mutate(NormFeature = Feature / median(Feature)) %>%
      
      dplyr::ungroup() %>%
      dplyr::select(!Plate)
  }
  
  # read and format control, test, and library data
  ctrlData <- dataPrep(ctrlDataPath, voi)
  testData <- dataPrep(testDataPath, voi)
  librData <- read.csv(librDataPath)
  
  # merge data
  joinData <- dplyr::inner_join(ctrlData, testData, by = "Coordinate") %>% dplyr::inner_join(librData, by = "Coordinate")
  
  # scale features; subtract and classify coordinates based on feature difference; round and relocate
  normData <- joinData %>%
    dplyr::mutate(
      NormFeature.Diff = NormFeature.y - NormFeature.x,
      TF.type = dplyr::case_when(
        NormFeature.Diff >= quantile(NormFeature.Diff, probs = max(featureRange)) ~ "Sig. Up",
        NormFeature.Diff <= quantile(NormFeature.Diff, probs = min(featureRange)) ~ "Sig. Down",
        TRUE ~ "Not Sig."
      )
    ) %>%
    dplyr::mutate(
      NormFeature.x = round(NormFeature.x, 3),
      NormFeature.y = round(NormFeature.y, 3),
      NormFeature.Diff = round(NormFeature.Diff, 3)
    ) %>%
    dplyr::relocate(c(NormFeature.Diff, TF.type), .before = Identifier)
  
  # return merged/processed data.frame
  return(normData)
}

### TO ANALYZE TWO SCREENS WITH REPLICATES
## This function processes merges quad data with a library, pivots wider, and computes summary statistics for each TF
dualScreen_processData_quad <- function(dataPath = NULL, libraryPath = NULL, voi = NULL, experiment = NULL){
  
  # read one screen and merge with library
  screenDF <- read.csv(dataPath) %>% tidyr::drop_na() %>% dplyr::inner_join(read.csv(libraryPath), by = "Coordinate")
  
  # omit unused columns and sort by Identifier (voi = variable of interest (Size or Color); divide voi by plate-specific median
  screenDF <- screenDF %>%
    dplyr::select(!c(ImagePath, X, Y, !!as.symbol(ifelse(voi == "Size", "Color", "Size")))) %>%
    dplyr::arrange(Identifier) %>%
    dplyr::rename(Feature = !!as.symbol(voi)) %>%
    dplyr::mutate(Plate = strsplit(Coordinate, split = "-") %>% lapply(function(x) x[1]) %>% unlist()) %>%
    dplyr::group_by(Plate) %>%
    dplyr::mutate(Feature = Feature / median(Feature)) %>%
    dplyr::ungroup() %>%
    dplyr::select(!c(Plate, Coordinate))
  
  # if the test set, omit any non-essential library columns to avoid duplications after subsequent merging
  if(experiment == "test"){
    screenDF <- screenDF %>% dplyr::select(Identifier, Feature)
  }
  
  # dynamically enumerate replicates in each group and compute summary stats
  screenDF <- screenDF %>%
    dplyr::group_by(Identifier) %>%
    dplyr::mutate(whichRep = glue::glue("Rep{dplyr::row_number()}_{experiment}")) %>%
    tidyr::pivot_wider(names_from = whichRep, values_from = Feature) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Avg = matrixStats::rowMeans2(dplyr::select(., dplyr::starts_with("Rep")) %>% as.matrix(), na.rm = TRUE),
      SD = matrixStats::rowSds(dplyr::select(., dplyr::starts_with("Rep")) %>% as.matrix(), na.rm = TRUE),
      CV = SD / Avg
    ) %>%
    dplyr::mutate(Avg = round(Avg, 2), SD = round(SD, 2), CV = round(CV, 2)) %>%
    dplyr::filter(!is.na(SD))
  
  # return data.frame
  return(screenDF)
}

## This function filters quadrants by their CV values
dualScreen_cvFilter <- function(processedDF = NULL, cvFilter = NULL){
  
  # filter by CV value
  processedDF <- processedDF %>% dplyr::filter(CV < cvFilter)
  
  # return filtered data.frame
  return(processedDF)
}

## This function merges the control and test data sets
dualScreen_mergeData_quad <- function(ctrlDF = NULL, testDF = NULL){
  
  # merge data
  mergeDF <- dplyr::inner_join(ctrlDF, testDF, by = "Identifier")
  
  # return merged data
  return(mergeDF)
}

## This function iteratively performs Brown-Forsythe tests
dualScreen_brownForsythe <- function(mergeDF = NULL){
  
  # return p-values from iterative Brown-Forsythe test for homogeneity of variances across groups
  pvals <- sapply(
    1:nrow(mergeDF),
    function(i){
      data.frame(
        y = c(
          mergeDF[i,] %>% dplyr::select(dplyr::matches("^Rep._control")) %>% unlist() %>% unname() %>% na.omit() %>% as.vector(),
          mergeDF[i,] %>% dplyr::select(dplyr::matches("^Rep._test")) %>% unlist() %>% unname() %>% na.omit() %>% as.vector()
        ),
        g = c(
          rep("control", mergeDF[i,] %>% dplyr::select(dplyr::matches("^Rep._control")) %>% unlist() %>% unname() %>% na.omit() %>% as.vector() %>% length()),
          rep("test", mergeDF[i,] %>% dplyr::select(dplyr::matches("^Rep._test")) %>% unlist() %>% unname() %>% na.omit() %>% as.vector() %>% length())
        )
      ) %>% car::leveneTest(data = ., y ~ g) %>% dplyr::pull(`Pr(>F)`) %>% min(., na.rm = TRUE)
    } 
  ) %>% suppressWarnings()
  
  # correct and round p-values
  pvals <- p.adjust(pvals, method = "BH") %>% round(., digits = 5)
  
  # return p-values
  return(pvals)
}

## This function iteratively performs t-tests
dualScreen_ttest <- function(mergeDF = NULL){
  
  # return p-values from iterative t-test for differences in means across groups
  pvals <- sapply(
    1:nrow(mergeDF),
    function(i){
      t.test(
        x = mergeDF %>%
          dplyr::ungroup() %>%
          dplyr::slice(i) %>%
          dplyr::select(dplyr::matches("^Rep._control")) %>%
          unlist(use.names = FALSE),
        
        y = mergeDF %>%
          dplyr::ungroup() %>%
          dplyr::slice(i) %>%
          dplyr::select(dplyr::matches("^Rep._test")) %>%
          unlist(use.names = FALSE),
        
        alternative = "two.sided"
      )$p.value
    }
  )
  
  # correct and round p-values
  pvals <- p.adjust(pvals, method = "BH") %>% round(., digits = 5)
  
  # return p-values
  return(pvals)
}

# non-parametric effect size estimator; % chance that rand. selected obs. of control is greater than rand. selected obs. of test
dualScreen_Aw <- function(mergeDF = NULL){
  
  # calculate non-parametric estimator for common-language (CL) effect size: Aw
  Aw <- sapply(
    1:nrow(mergeDF),
    function(i) {
      xvec <- mergeDF[i,] %>% dplyr::select(dplyr::matches("^Rep._control")) %>% unlist() %>% unname() %>% na.omit() %>% as.vector()
      yvec <- mergeDF[i,] %>% dplyr::select(dplyr::matches("^Rep._test")) %>% unlist() %>% unname() %>% na.omit() %>% as.vector()
      
      sum(sapply(xvec, function(x) sum(x > yvec) + 0.5*sum(x == yvec))) / (length(xvec) * length(yvec))
    }
  )
  
  # return Aw
  return(Aw)
}

## This function compares control and test feature vectors
dualScreen_stattest <- function(joinData = NULL){
  
  # BH-correct p-values; classify as repressor or activator; add absolute difference
  joinData <- joinData %>%
    dplyr::mutate(
      bf_pv = dualScreen_brownForsythe(mergeDF = joinData),
      ttest_pv = dualScreen_ttest(mergeDF = joinData),
      Aw = dualScreen_Aw(mergeDF = joinData),
      Abs_Diff = round(abs(Avg.x - Avg.y), 2),
      TF.type = dplyr::case_when(
        ttest_pv < 0.05 & Avg.y < Avg.x ~ "Sig. Down",
        ttest_pv < 0.05 & Avg.y > Avg.x ~ "Sig. Up",
        TRUE ~ "Not Sig."
      )
    )
  
  # message with number of significant differences
  message(paste0("Significant TFs: ", nrow(joinData[which(joinData$ttest_pv < 0.05),])))
  
  # reorder columns
  joinData <- joinData %>%
    dplyr::relocate(c(Avg.x, SD.x, Avg.y, SD.y, bf_pv, ttest_pv, Aw, Abs_Diff, TF.type), .before = Family) %>%
    dplyr::select(!dplyr::matches("^Rep")) %>%
    dplyr::select(!dplyr::matches("CV\\."))
  
  # return analyzed data
  return(joinData)
}

### TO ANALYZE BOTH SCREENS
## This function computes family enrichment scores
dualScreen_familyEnrichment <- function(libraryPath = NULL, processedData = NULL){
  
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
  numUp <- processedData %>% dplyr::filter(TF.type == "Sig. Up") %>% nrow()
  numDown <- processedData %>% dplyr::filter(TF.type == "Sig. Down") %>% nrow()
  
  # count number of significant TFs per family; initialize extra columns; add total family counts
  familySummary <- processedData %>%
    dplyr::filter(TF.type != "Not Sig.") %>%
    dplyr::group_by(Family) %>%
    dplyr::summarise(
      TFs_Up = sum(TF.type == "Sig. Up"),
      TFs_Down = sum(TF.type == "Sig. Down")
    ) %>%
    dplyr::left_join(familyCounts, by = "Family") %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::relocate(n, .before = TFs_Up) %>%
    dplyr::mutate(
      Expected_Up = round(n * numUp / uniqueTFs, 2),
      Up_pvalue = NA,
      Expected_Down = round(n * numDown / uniqueTFs, 2),
      Down_pvalue = NA
    ) %>%
    dplyr::ungroup()
  
  # perform Fisher's exact test for each family / tf.type
  for(i in 1:nrow(familySummary)){
    
    #'matrix[1,1] = number of activators or repressors in family
    #'matrix[2,1] = number of activators or repressors not in family
    #'matrix[1,2] = number of family members that are not activators or repressors
    #'matrix[2,2] = number of non-family members that are not activators or repressors
    
    # FET p-value for activators in ith family
    familySummary$Up_pvalue[i] <- fisher.test(
      matrix(c(
        familySummary$TFs_Up[i],
        numUp - familySummary$TFs_Up[i],
        familySummary$n[i] - familySummary$TFs_Up[i],
        uniqueTFs - numUp - familySummary$n[i] + familySummary$TFs_Up[i]
      ), nr = 2),
      alternative = "greater"
    )$p.value %>% round(., 5)
    
    # FET p-value for repressors in ith family
    familySummary$Down_pvalue[i] <- fisher.test(
      matrix(c(
        familySummary$TFs_Down[i],
        numDown - familySummary$TFs_Down[i],
        familySummary$n[i] - familySummary$TFs_Down[i],
        uniqueTFs - numDown - familySummary$n[i] + familySummary$TFs_Down[i]
      ), nr = 2),
      alternative = "greater"
    )$p.value %>% round(., 5)
  }
  
  # return family summary
  return(familySummary)
}

## This function formats the family enrichment data.frame for subsequent plotting
dualScreen_familyEnrichment_format <- function(processedData = NULL, familyEnrichData = NULL){
  
  # remove insignificant TFs
  processedData <- processedData %>% dplyr::filter(TF.type != "Not Sig.")
  
  # data.frame for sig. up/down TFs per family; merge with enrichment data
  familyData <- table(processedData$Family, processedData$TF.type) %>%
    as.data.frame() %>%
    dplyr::filter(Freq != 0) %>%
    dplyr::rename(Family = Var1, TF.type = Var2, TypeCount = Freq) %>%
    dplyr::mutate(TypeCount = ifelse(TF.type == "Sig. Up", TypeCount, -1*TypeCount)) %>%
    tidyr::pivot_wider(names_from = TF.type, values_from = TypeCount) %>%
    dplyr::left_join(familyEnrichData, by = "Family")
  
  # sort data.frame by family size and name; reorder Family as factor; add columns to denote significance
  familyData <- familyData %>% dplyr::arrange(n, Family) %>%
    dplyr::mutate(
      Family = factor(Family, levels = unique(Family)),
      Up_Sig = ifelse(Up_pvalue < 0.05, "Yes", "No"),
      Down_Sig = ifelse(Down_pvalue < 0.05, "Yes", "No")
    )
  
  # return formatted data.frame
  return(familyData)
}
