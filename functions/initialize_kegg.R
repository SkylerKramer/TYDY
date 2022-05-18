# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

#' This script is intended to pull organism-specific pathways from the RESTful KEGG API.
#' To use other organisms, the 'orgCode' should be changed.

library(KEGGREST)

# specify organism; "ath" is A. thaliana
orgCode <- "ath"

# pull A.t. pathways
pathways.list <- KEGGREST::keggList("pathway", orgCode)

# get genes for each pathway
pathway.codes <- sub("path:", "", names(pathways.list))
genes.by.pathway <- sapply(
  pathway.codes,
  function(pwid){
    pw <- KEGGREST::keggGet(pwid)
    
    if (is.null(pw[[1]]$GENE)) return(NA)
    
    pw2 <- pw[[1]]$GENE[c(TRUE,FALSE)]
    pw2 <- unlist(lapply(strsplit(pw2, split = ";", fixed = T), function(x)x[1]))
    
    return(pw2)
  }
)

# write R objects
saveRDS(pathways.list, file = "req/ath_pathways-list.rds")
saveRDS(genes.by.pathway, file = "req/ath_genes-by-pathway.rds")
