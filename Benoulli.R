## Run Benoulli mixture models and create heatmaps of symptoms clusters

library(comato)

file<-"ever_Q1"  ## rds file name for symptoms for each analysis group

data<-readRDS(paste0(file,".rds"))  ## read in symptoms
data<-na.omit(data)  ## remove individuals with missing data

set.seed(12345) ## principle seed

res <- MBM.cluster(data, 1, 14)  ## run Bernoulli mixture model

saveRDS(res,paste0(file,".bernoulli.rds")) ## save output of clusters and individual probabilites

## Generate heatmaps as in Figure 1 and 2

library(RColorBrewer)
library(lattice)
library(latticeExtra)

symptomscores<-res[[3]][[2]]

myTheme <- custom.theme(region=brewer.pal(n=3, 'RdBu'))

## use the line below for numbers and the scale line in level plot
#y.scale <- list(at=seq(0,length(res[[3]][[1]]),1))

lp<-levelplot(t(symptomscores),
          aspect = "fill", xlab='', ylab='',
          scales = list(x = list(rot = 45), tck = c(1,0)),
          colorkey = list(space = "right"),
          par.settings=myTheme,
          border='black', border.lwd=.6,
                         panel=function(...) {
                 arg <- list(...)
                 panel.levelplot(...)
                 panel.text(lp$panel.args.common$x, lp$panel.args.common$y, round(lp$panel.args.common$z,2))}
)

lp$par.settings$layout.widths$left.padding<-8

## For ever depressed
trellis.device(pdf, file=paste0(file,"_bernoulli.pdf"), paper="a4r", height=7.5, width=10.6)
lp
dev.off()


## For currently depressed
trellis.device(pdf, file=paste0(file,"_bernoulli.pdf"), paper="a4r", height=7, width=9.9)
lp
dev.off()

