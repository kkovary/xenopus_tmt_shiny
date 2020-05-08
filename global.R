
# Check for installed packages, install packages that are not installed, and load all

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  options(repos=structure(c(CRAN="http://cloud.r-project.org/")))
  options(repos = BiocManager::repositories(version="3.10"))
  
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
}

packages<-c("shiny", "tidyverse", "BiocManager", "DT", "ComplexHeatmap", "shinythemes", "circlize","RColorBrewer","org.Xl.eg.db")
check.packages(packages)


### Data ###
bet_mod_cor2 <- readRDS("data/bet_mod_cor_2.RDS")
cor <- readRDS("data/cor.RDS")

### Functions ###
mod_cor_heatmap <- function(num, bet_mod_data = bet_mod_cor2){
  # Takes in a table and row position and returns a 
  # heatmap of correlations between two modules
  
  accession_a <- bet_mod_data$name_a[num]
  accession_b <- bet_mod_data$name_b[num]
  
  uniprot_a <- bet_mod_data$uniprot_a[num] %>% unlist()
  uniprot_b <- bet_mod_data$uniprot_b[num] %>% unlist()
  
  color_groups <- tibble(accession = c(rep(accession_a, length(uniprot_a)),
                                       rep(accession_b, length(uniprot_b))),
                         uniprot = c(uniprot_a, uniprot_b)) %>% 
    mutate(accession = ifelse(duplicated(uniprot),
                              "both",
                              accession)) %>% 
    filter(!duplicated(uniprot,fromLast = TRUE))
  
  color_code <- c("#762a83","#1b7837","#ffffbf")
  names(color_code) <- c(accession_a, accession_b, "both")
  
  column_ha <- HeatmapAnnotation(
    Module = color_groups$accession,
    col = list(Module = color_code)
  )
  
  mod_cor <- cor[unique(c(uniprot_a, uniprot_b)), unique(c(uniprot_a, uniprot_b))]
  
  mod_cor_names <- mapIds(org.Xl.eg.db, 
                          keys = unique(c(uniprot_a, uniprot_b)), 
                          keytype = 'UNIPROT', 
                          column = 'SYMBOL')
  
  mod_cor_names <- sapply(mod_cor_names, function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
  })
  
  rownames(mod_cor) <- mod_cor_names
  colnames(mod_cor) <- mod_cor_names
  
  ht_list <- Heatmap(
    mod_cor,
    name = "R",
    col = colorRamp2(seq(-1,1,0.01), rev(colorRampPalette(brewer.pal(11, 'RdBu'))(201))),
    top_annotation = column_ha,
    cluster_columns = TRUE,
    cluster_rows = TRUE,
    row_names_gp = gpar(fontsize = 7),
    column_names_gp = gpar(fontsize = 7)
  )
  
  draw(ht_list, annotation_legend_side = "top")
}
