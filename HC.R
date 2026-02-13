# Hierarchical clustering based on agnes

file<-"ever_Q1"  ## rds file name for symptoms for each analysis group

data<-readRDS(paste0(file,".rds")) ## read in symptoms
data<-na.omit(data) ## remove individuals with missing data

set.seed(12345) ## principle seed

library(cluster)
library(purrr)
library(RColorBrewer)
library(lattice)
library(latticeExtra)

d <- dist(data, method="binary") ## set distance matrix as binary

m <- c( "average", "single", "complete", "ward")  ## test various methods
names(m) <- c( "average", "single", "complete", "ward")
 function to compute coefficient
ac <- function(x) {
  agnes(d, diss = TRUE, method = x)$ac
}
map_dbl(m, ac)   ### check which method has the highest score

agn2 <- agnes(d, diss = TRUE, method = method)  ## Conduct hierarchical clustering

saveRDS(agn2,paste0(file,".agnesHC.rds"))


clusters<-n  ## Set this value as the number of clusters identified by Benoulli method for same analysis group
agout <- cutree(agn2, k = clusters)   ## indentify which individuals cluster together

saveRDS(agout,paste0(file,".agnesHC.rds"))  ## save output of individual clustering

