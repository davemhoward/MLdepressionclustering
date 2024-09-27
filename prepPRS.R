library(dplyr)

### Load data

MHQ_Full<-readRDS("davis_fields.rds")

colnames(MHQ_Full) <- paste('f.', colnames(MHQ_Full), sep = '')
colnames(MHQ_Full) <- gsub("-", ".", colnames(MHQ_Full))

### Calculate age at MHQ as age at baseline + (diff in days between baseline and MHQ divided by 365.25)

MHQ_Full$Age.At.MHQ<-as.numeric(with(MHQ_Full, ifelse(is.na(f.20400.0.0),f.21003.0.0 + ceiling(difftime(median(na.omit(as.Date(MHQ_Full$f.20400.0.0))), as.Date(f.53.0.0), units="days")/365.25),f.21003.0.0 + ceiling(difftime(as.Date(f.20400.0.0), as.Date(f.53.0.0), units="days")/365.25))))
MHQ_Full$Age.At.WBQ<-as.numeric(with(MHQ_Full, ifelse(is.na(f.29197.0.0),f.21003.0.0 + ceiling(difftime(median(na.omit(as.Date(MHQ_Full$f.29197.0.0))), as.Date(f.53.0.0), units="days")/365.25),f.21003.0.0 + ceiling(difftime(as.Date(f.29197.0.0), as.Date(f.53.0.0), units="days")/365.25))))
MHQ_Full$Sex<-MHQ_Full$f.31.0.0

PCs<-read.table("ukb82087_40PCs_ID_from_sqc_v2.txt",sep="",header=FALSE)

PGS<-merge(MHQ_Full[,c("f.eid","Age.At.MHQ","Age.At.WBQ","Sex")],PCs[,1:11],by.x="f.eid",by.y="V1")

PGS$ancestry<-NA
anc<-c("AFR","AMR","CSA","EAS","EUR","MID")
pheno<-c("BIP","SCZ","ADHD")

for (i in 1:6) {
  for (j in 1:3) {
    filename<-paste0("pgs/",anc[i],"/prscs/",pheno[j],"/UKB-",pheno[j],"-",anc[i],".profiles")
    scores<-read.table(filename,header=T,sep="")
    if (j == 1) {
      PGS[which(PGS$f.eid %in% scores$IID),"ancestry"]<-anc[i]
      PGS[,ncol(PGS)+1]<-merge(PGS,scores[,c("IID","BIP_phi_auto")],by.x="f.eid",by.y="IID",all.x=TRUE)["BIP_phi_auto"]
      colnames(PGS)[ncol(PGS)] <- paste(anc[i], "_", colnames(PGS)[ncol(PGS)], sep = '')
    } else if (j == 2) {
      PGS[,ncol(PGS)+1]<-merge(PGS,scores[,c("IID","SCZ_phi_auto")],by.x="f.eid",by.y="IID",all.x=TRUE)["SCZ_phi_auto"]
      colnames(PGS)[ncol(PGS)] <- paste(anc[i], "_", colnames(PGS)[ncol(PGS)], sep = '')
    } else {
      PGS[,ncol(PGS)+1]<-merge(PGS,scores[,c("IID","ADHD_phi_auto")],by.x="f.eid",by.y="IID",all.x=TRUE)["ADHD_phi_auto"]
      colnames(PGS)[ncol(PGS)] <- paste(anc[i], "_", colnames(PGS)[ncol(PGS)], sep = '')
    }
  }
}

PGS<- PGS %>% mutate(BIP_phi_auto = coalesce(AFR_BIP_phi_auto,AMR_BIP_phi_auto,CSA_BIP_phi_auto,EAS_BIP_phi_auto,EUR_BIP_phi_auto,MID_BIP_phi_auto))
PGS<- PGS %>% mutate(SCZ_phi_auto = coalesce(AFR_SCZ_phi_auto,AMR_SCZ_phi_auto,CSA_SCZ_phi_auto,EAS_SCZ_phi_auto,EUR_SCZ_phi_auto,MID_SCZ_phi_auto))
PGS<- PGS %>% mutate(ADHD_phi_auto = coalesce(AFR_ADHD_phi_auto,AMR_ADHD_phi_auto,CSA_ADHD_phi_auto,EAS_ADHD_phi_auto,EUR_ADHD_phi_auto,MID_ADHD_phi_auto))

write.table(PGS[,c("f.eid","Age.At.MHQ","Age.At.WBQ","Sex","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","BIP_phi_auto","SCZ_phi_auto","ADHD_phi_auto")], "PRS.txt", row.names=F, col.name=T, quote=F)
