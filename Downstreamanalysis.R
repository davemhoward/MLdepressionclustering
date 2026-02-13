## Regression analyses


file<-"ever_Q1"  ## rds file name for symptoms for each analysis group

data<-readRDS(paste0(file,".bernoulli.rds")) ## read in results from Bernoulli mixture model

clust<-as.data.frame(data[[3]][[3]]) ## obtain probability scores for each individual for each cluster
clust$f.eid<-row.names(clust)  ## save individual ids in a column rather than row name


demog<-read.table("demographics.txt",header=T,sep=" ") ## read in independent factors

stats<-merge(clust,demog,by="f.eid",all.x=T)   ## merge probability scores with factors

nclust<-ncol(data[[3]][[3]])  ## set number of clusters to nclust

## CURRENTLY SET FOR MHQ
stats[which(stats$Income == "DontKnowRefuse"),c("Income")]<-NA
stats[which(stats$Smoker == "PreferNotToAnswer"),c("Smoker")]<-NA
stats$Ethnicity<-as.factor(stats$Ethnicity)
stats$Ethnicity<-relevel(stats$Ethnicity, ref=6)
options(width=200)
sink(paste0(file,".regression.txt"))
for (k in 1:nclust) {

  stats$cluster<-stats[,c(k+1)]
  fit <- lm(cluster ~ Age.At.MHQ + as.factor(Sex) + Ethnicity + as.factor(BirthPlace) +
    TDI + as.factor(Smoker) + BMI,
    data = stats)

  print(k)
  print(summary(fit))

  fit2 <- lm(cluster ~ Age.At.MHQ + as.factor(Sex) + MHQ_recurrent,
    data = stats)
  print(summary(fit2))

  fit3 <- lm(cluster ~ Age.At.WBQ + as.factor(Sex) + trd,
    data = stats)
  print(summary(fit3))

  health <- lm(cluster ~ Age.At.MHQ + as.factor(Sex) + as.factor(mi) + as.factor(stroke) +
    as.factor(asthma) + as.factor(copd) + as.factor(dementia) + as.factor(renal) + as.factor(mnd) + as.factor(parkinsons),
    data = stats)

## Extention of Parkinson's to pre and post MHQ
#  health <- lm(cluster ~ Age.At.MHQ + as.factor(Sex) + as.factor(mi) + as.factor(stroke) +
#    as.factor(asthma) + as.factor(copd) + as.factor(dementia) + as.factor(renal) + as.factor(mnd) + as.factor(parkinsonspreMHQ),
#    data = stats)
#  health <- lm(cluster ~ Age.At.MHQ + as.factor(Sex) + as.factor(mi) + as.factor(stroke) +
#    as.factor(asthma) + as.factor(copd) + as.factor(dementia) + as.factor(renal) + as.factor(mnd) + as.factor(parkinsonspostMHQ),
#    data = stats)

  print(summary(health))

}
sink()


## PRS analysis

prs_input<-read.table("PRS.txt",header=T,sep=" ")  ## read in polygenic scores

prs<-merge(clust,prs_input,by="f.eid") ## merge probability scores with polygenic scores

options(width=200)
sink(paste0(file,".PRS.txt"))
for (k in 1:nclust) {

  prs$cluster<-prs[,c(k+1)]

  print(k)
  fitbp <- lm(cluster ~ Age.At.MHQ + as.factor(Sex) + as.factor(ancestry) + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + BIP_phi_auto,
    data = prs)
  print(summary(fitbp))
  fitscz <- lm(cluster ~ Age.At.MHQ + as.factor(Sex) + as.factor(ancestry) + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + SCZ_phi_auto,
    data = prs)
  print(summary(fitscz))
  fitadhd <- lm(cluster ~ Age.At.MHQ + as.factor(Sex) + as.factor(ancestry) + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + ADHD_phi_auto,
    data = prs)
  print(summary(fitadhd))

}

sink()

