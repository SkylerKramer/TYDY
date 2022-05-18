# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

# function to perform GO enrichment analysis
goAnalysis <- function(dataPath = NULL, ontologySelection = NULL){
  
  # read data
  tfDF <- read.csv(dataPath)
  
  # make gene list with either p-values from t-test (dual-screen replicate analysis) or 0/1 if no t-testing
  if("ttest_pv" %in% colnames(tfDF)){
    geneList <- tfDF$ttest_pv
  } else{
    geneList <- ifelse(tfDF$TF.type == "Not Sig.", 1, 0)
  }
  
  # name gene list
  names(geneList) <- tfDF$Identifier
  
  # make GO object
  GOdata <- new(
    "topGOdata",
    ontology = ontologySelection,
    allGenes = geneList,
    geneSelectionFun = function(x) x == 0,
    annot = annFUN.org,
    mapping = "org.At.tair.db"
  )
  
  # use KS test for GO enrichment and summarize the results
  resultKS <- topGO::runTest(GOdata, algorithm = "weight01", statistic = "ks")
  tab <- topGO::GenTable(GOdata, raw.p.value = resultKS, topNodes = length(resultKS@score), numChar = 120)
  
  # return GO results
  return(tab)
}

# function to perform KEGG pathway enrichment analysis
keggAnalysis <- function(dataPath = NULL, pathwaysListPath = "req/ath_pathways-list.rds", genesByPathwayPath = "req/ath_genes-by-pathway.rds"){
  
  # read data
  tfDF <- read.csv(dataPath)
  
  # read required objects for subsequent KEGG enrichment analysis
  pathways.list <- readRDS(pathwaysListPath)
  genes.by.pathway <- readRDS(genesByPathwayPath)
  
  # make gene list with either p-values from t-test (dual-screen replicate analysis) or 0/1 if no t-testing
  if("ttest_pv" %in% colnames(tfDF)){
    geneList <- tfDF$ttest_pv
  } else{
    geneList <- ifelse(tfDF$TF.type == "Not Sig.", 1, 0)
  }
  
  # name gene list
  names(geneList) <- tfDF$Identifier
  
  # Wilcoxon test for each pathway
  pVals.by.pathway <- t(
    sapply(
      names(genes.by.pathway),
      function(pathway) {
        
        # extract all genes in pathway
        pathway.genes <- genes.by.pathway[[pathway]]
        
        # get intersection and set difference between gene list and pathway genes
        list.genes.in.pathway <- intersect(names(geneList), pathway.genes)
        list.genes.not.in.pathway <- setdiff(names(geneList), list.genes.in.pathway)
        
        # get scores (0s and 1s) for genes in and not in the pathway
        scores.in.pathway <- geneList[list.genes.in.pathway]
        scores.not.in.pathway <- geneList[list.genes.not.in.pathway]
        
        # if there are genes in the pathway, compute Wilcox test
        if (length(scores.in.pathway) > 0){
          p.value <- wilcox.test(scores.in.pathway, scores.not.in.pathway, alternative = "less")$p.value
        } else{
          p.value <- NA
        }
        
        # return pvalues and pathways
        return(c(p.value = p.value, Annotated = length(list.genes.in.pathway)))
      }
    )
  )
  
  # format table
  tab <- data.frame(pathway.code = rownames(pVals.by.pathway)) %>%
    dplyr::mutate(
      pathway.name = pathways.list[paste0("path:", pathway.code)],
      p.value = pVals.by.pathway[,"p.value"],
      Annotated = pVals.by.pathway[,"Annotated"]
    ) %>%
    dplyr::arrange(p.value) %>%
    na.omit()
  
  # return table
  return(tab)
}
