### Load dependencies

library(data.table)
library(Hmisc)
library(stringi)
library(e1071)

### Load data

data<-readRDS("davis_fields.rds")

colnames(data) <- paste('f.', colnames(data), sep = '')
colnames(data) <- gsub("-", ".", colnames(data))

#####Demographics#####

### Calculate age at Mental Health Questionnair (MHQ) as age at baseline + (diff in days between baseline and MHQ divided by 365.25)
### Calculate age at Well being Questionnaire (WBQ) as age at baseline + (diff in days between baseline and MHQ divided by 365.25)
data$Age.At.MHQ<-as.numeric(with(data, ifelse(is.na(f.20400.0.0),f.21003.0.0 + ceiling(difftime(median(na.omit(as.Date(data$f.20400.0.0))), as.Date(f.53.0.0), units="days")/365.25),f.21003.0.0 + ceiling(difftime(as.Date(f.20400.0.0), as.Date(f.53.0.0), units="days")/365.25))))
data$Age.At.WBQ<-as.numeric(with(data, ifelse(is.na(f.29197.0.0),f.21003.0.0 + ceiling(difftime(median(na.omit(as.Date(data$f.29197.0.0))), as.Date(f.53.0.0), units="days")/365.25),f.21003.0.0 + ceiling(difftime(as.Date(f.29197.0.0), as.Date(f.53.0.0), units="days")/365.25))))

### Sex field

data$Sex<-with(data, ifelse(is.na(f.31.0.0) | f.31.0.0 < 0, NA,
		      		ifelse(!is.na(f.31.0.0), f.31.0.0, NA))) 

### Define Ethnicity and Migrant status (0 = born in Britain, 1 = born elsewhere) fields

data$Ethnicity<-ordered(with(data, ifelse(is.na(f.21000.0.0) | f.21000.0.0 < 0, NA,
              		 	 	   ifelse(!is.na(f.21000.0.0) & f.21000.0.0 > 1000 & f.21000.0.0 < 1999, "White",
              		 	 	   ifelse(!is.na(f.21000.0.0) & f.21000.0.0 > 2000 & f.21000.0.0 < 2999, "Mixed",
              		 	 	   ifelse(!is.na(f.21000.0.0) & f.21000.0.0 > 3000 & f.21000.0.0 < 3999, "Asian",
              		 	 	   ifelse(!is.na(f.21000.0.0) & f.21000.0.0 > 4000 & f.21000.0.0 < 4999, "Black",
              		 	 	   ifelse(!is.na(f.21000.0.0) & f.21000.0.0 == 5, "Chinese",
              		 	 	   ifelse(!is.na(f.21000.0.0) & f.21000.0.0 == 6, "Other",NA)))))))),
			 	 	   levels=c("White","Black","Asian","Chinese","Mixed","Other"))

data$BirthPlace<-with(data, ifelse(is.na(f.1647.0.0) | f.1647.0.0 < 0, NA,
			      		ifelse(f.1647.0.0 == 5 | f.1647.0.0 == 6, 1, 0)))


### Smoker status (rename as factor)

data$Smoker<-ordered(with(data, ifelse(!is.na(f.20116.0.0) & f.20116.0.0 == 2, "Current",
			      ifelse(!is.na(f.20116.0.0) & f.20116.0.0 == 1, "Former",
			      ifelse(!is.na(f.20116.0.0) & f.20116.0.0 == 0, "Never",
			      ifelse(!is.na(f.20116.0.0) & f.20116.0.0 == -3, "PreferNotToAnswer",NA))))),
			      levels=c("Current", "Former", "Never", "PreferNotToAnswer"))

## BMI

lowerfemalebmi<-quantile(data$f.21001.0.0[which(data$Sex == 0)], 0.025, na.rm=T) ## 2.5% lower bound for women: BMI 19.65666
upperfemalebmi<-quantile(data$f.21001.0.0[which(data$Sex == 0)], 0.975, na.rm=T) ## 2.5% upper bound for women BMI 39.89475
lowermalebmi<-quantile(data$f.21001.0.0[which(data$Sex == 1)], 0.025, na.rm=T) ## 2.5% lower bound for men BMI 21.0381
uppermalebmi<-quantile(data$f.21001.0.0[which(data$Sex == 1)], 0.975, na.rm=T) ## 2.5% upper bound for men 37.8122

data$BMI<-data$f.21001.0.0
data$rawBMI<-data$f.21001.0.0
## Remove indiviudals outside of the lower and upper bounds for each sex
data$BMI[which(data$Sex == 0 & (data$BMI <= lowerfemalebmi | data$BMI >= upperfemalebmi))]<-NA
data$BMI[which(data$Sex == 1 & (data$BMI <= lowermalebmi | data$BMI >= uppermalebmi))]<-NA
## take the reciprical and multiply by -1 to keep direction of effect and the scale to have a mean of 0 and sd of 1
data$BMI[which(data$Sex == 0)]<-scale(-1*1/(data$BMI[which(data$Sex == 0)]))
data$BMI[which(data$Sex == 1)]<-scale(-1*1/(data$BMI[which(data$Sex == 1)]))

skewness(data$BMI[which(data$Sex == 0)],na.rm=T)
skewness(data$BMI[which(data$Sex == 1)],na.rm=T)

#hist(data$BMI[which(data$Sex == 0)])


## Number of Depressive episodes reported at each questionnaire. 1 is given for recurrent depression, 0 for single episode

## Set -818 and NA to missing, 1 as 0 and everything else as 1
data$MHQ_recurrent<-with(data, ifelse(is.na(f.20442.0.0) | f.20442.0.0 == -818, NA,
			      		ifelse(f.20442.0.0 == 1, 0, 1)))
## Set -1, -3 and NA to missing, 1 as 0 and everything else as 1
data$WBQ_recurrent<-with(data, ifelse(is.na(f.29033.0.0) | f.29033.0.0 == -1 | f.29033.0.0 == -3, NA,
			      		ifelse(f.29033.0.0 == 1, 0, 1)))

## ADO Health phenotypes

ado<-readRDS("ado.rds")

colnames(ado) <- paste('f.', colnames(ado), sep = '')
colnames(ado) <- gsub("-", ".", colnames(ado))

#length(!is.na(ado$f.42000.0.0_ukb50682))

data<-merge(data,ado,by="f.eid")

## MI

data$mi[which(is.na(data$f.42000.0.0_ukb675453))]<-0 ## controls
data$mi[which(!is.na(data$f.42000.0.0_ukb675453))]<-1  ## cases

## for pre and post questionnaire controls only use actual controls not not yet diagnosed cases

data$mipreMHQ[which(is.na(data$f.42000.0.0_ukb675453))]<-0 ## controls
data$mipreMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42000.0.0_ukb675453) > 0)]<-1  ## MI cases after MHQ
data$mipreMHQ[which(data$f.42000.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date for MI

data$mipostMHQ[which(is.na(data$f.42000.0.0_ukb675453))]<-0 ## controls
data$mipostMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42000.0.0_ukb675453) < 0)]<-1  ## MI cases after MHQ
data$mipostMHQ[which(data$f.42000.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date for MI

data$mipreWBQ[which(is.na(data$f.42000.0.0_ukb675453))]<-0 ## controls
data$mipreWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42000.0.0_ukb675453) > 0)]<-1  ## MI cases after WBQ
data$mipreWBQ[which(data$f.42000.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date for MI

data$mipostWBQ[which(is.na(data$f.42000.0.0_ukb675453))]<-0 ## controls
data$mipostWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42000.0.0_ukb675453) < 0)]<-1  ## MI cases after WBQ
data$mipostWBQ[which(data$f.42000.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date for MI

## stroke

data$stroke[which(is.na(data$f.42006.0.0_ukb675453))]<-0 ## controls
data$stroke[which(!is.na(data$f.42006.0.0_ukb675453))]<-1  ## cases

## for pre and post questionnaire controls only use actual controls not not yet diagnosed cases

data$strokepreMHQ[which(is.na(data$f.42006.0.0_ukb675453))]<-0 ## controls
data$strokepreMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42006.0.0_ukb675453) > 0)]<-1  ## cases after MHQ
data$strokepreMHQ[which(data$f.42006.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$strokepostMHQ[which(is.na(data$f.42006.0.0_ukb675453))]<-0 ## controls
data$strokepostMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42006.0.0_ukb675453) < 0)]<-1  ## cases after MHQ
data$strokepostMHQ[which(data$f.42006.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$strokepreWBQ[which(is.na(data$f.42006.0.0_ukb675453))]<-0 ## controls
data$strokepreWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42006.0.0_ukb675453) > 0)]<-1  ## cases after WBQ
data$strokepreWBQ[which(data$f.42006.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$strokepostWBQ[which(is.na(data$f.42006.0.0_ukb675453))]<-0 ## controls
data$strokepostWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42006.0.0_ukb675453) < 0)]<-1  ## cases after WBQ
data$strokepostWBQ[which(data$f.42006.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

## asthma

data$asthma[which(is.na(data$f.42014.0.0_ukb675453))]<-0 ## controls
data$asthma[which(!is.na(data$f.42014.0.0_ukb675453))]<-1  ## cases

## for pre and post questionnaire controls only use actual controls not not yet diagnosed cases

data$asthmapreMHQ[which(is.na(data$f.42014.0.0_ukb675453))]<-0 ## controls
data$asthmapreMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42014.0.0_ukb675453) > 0)]<-1  ## cases after MHQ
data$asthmapreMHQ[which(data$f.42014.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$asthmapostMHQ[which(is.na(data$f.42014.0.0_ukb675453))]<-0 ## controls
data$asthmapostMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42014.0.0_ukb675453) < 0)]<-1  ## cases after MHQ
data$asthmapostMHQ[which(data$f.42014.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$asthmapreWBQ[which(is.na(data$f.42014.0.0_ukb675453))]<-0 ## controls
data$asthmapreWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42014.0.0_ukb675453) > 0)]<-1  ## cases after WBQ
data$asthmapreWBQ[which(data$f.42014.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$asthmapostWBQ[which(is.na(data$f.42014.0.0_ukb675453))]<-0 ## controls
data$asthmapostWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42014.0.0_ukb675453) < 0)]<-1  ## cases after WBQ
data$asthmapostWBQ[which(data$f.42014.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

## copd

data$copd[which(is.na(data$f.42016.0.0_ukb675453))]<-0 ## controls
data$copd[which(!is.na(data$f.42016.0.0_ukb675453))]<-1  ## cases

## for pre and post questionnaire controls only use actual controls not not yet diagnosed cases

data$copdpreMHQ[which(is.na(data$f.42016.0.0_ukb675453))]<-0 ## controls
data$copdpreMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42016.0.0_ukb675453) > 0)]<-1  ## cases after MHQ
data$copdpreMHQ[which(data$f.42016.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$copdpostMHQ[which(is.na(data$f.42016.0.0_ukb675453))]<-0 ## controls
data$copdpostMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42016.0.0_ukb675453) < 0)]<-1  ## cases after MHQ
data$copdpostMHQ[which(data$f.42016.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$copdpreWBQ[which(is.na(data$f.42016.0.0_ukb675453))]<-0 ## controls
data$copdpreWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42016.0.0_ukb675453) > 0)]<-1  ## cases after WBQ
data$copdpreWBQ[which(data$f.42016.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$copdpostWBQ[which(is.na(data$f.42016.0.0_ukb675453))]<-0 ## controls
data$copdpostWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42016.0.0_ukb675453) < 0)]<-1  ## cases after WBQ
data$copdpostWBQ[which(data$f.42016.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

## dementia

data$dementia[which(is.na(data$f.42018.0.0_ukb675453))]<-0 ## controls
data$dementia[which(!is.na(data$f.42018.0.0_ukb675453))]<-1  ## cases

## for pre and post questionnaire controls only use actual controls not not yet diagnosed cases

data$dementiapreMHQ[which(is.na(data$f.42018.0.0_ukb675453))]<-0 ## controls
data$dementiapreMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42018.0.0_ukb675453) > 0)]<-1  ## cases after MHQ
data$dementiapreMHQ[which(data$f.42018.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$dementiapostMHQ[which(is.na(data$f.42018.0.0_ukb675453))]<-0 ## controls
data$dementiapostMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42018.0.0_ukb675453) < 0)]<-1  ## cases after MHQ
data$dementiapostMHQ[which(data$f.42018.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$dementiapreWBQ[which(is.na(data$f.42018.0.0_ukb675453))]<-0 ## controls
data$dementiapreWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42018.0.0_ukb675453) > 0)]<-1  ## cases after WBQ
data$dementiapreWBQ[which(data$f.42018.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$dementiapostWBQ[which(is.na(data$f.42018.0.0_ukb675453))]<-0 ## controls
data$dementiapostWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42018.0.0_ukb675453) < 0)]<-1  ## cases after WBQ
data$dementiapostWBQ[which(data$f.42018.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

## renal

data$renal[which(is.na(data$f.42026.0.0_ukb675453))]<-0 ## controls
data$renal[which(!is.na(data$f.42026.0.0_ukb675453))]<-1  ## cases

## for pre and post questionnaire controls only use actual controls not not yet diagnosed cases

data$renalpreMHQ[which(is.na(data$f.42026.0.0_ukb675453))]<-0 ## controls
data$renalpreMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42026.0.0_ukb675453) > 0)]<-1  ## cases before MHQ
data$renalpreMHQ[which(data$f.42026.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$renalpostMHQ[which(is.na(data$f.42026.0.0_ukb675453))]<-0 ## controls
data$renalpostMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42026.0.0_ukb675453) < 0)]<-1  ## cases after MHQ
data$renalpostMHQ[which(data$f.42026.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$renalpreWBQ[which(is.na(data$f.42026.0.0_ukb675453))]<-0 ## controls
data$renalpreWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42026.0.0_ukb675453) > 0)]<-1  ## cases before WBQ
data$renalpreWBQ[which(data$f.42026.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$renalpostWBQ[which(is.na(data$f.42026.0.0_ukb675453))]<-0 ## controls
data$renalpostWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42026.0.0_ukb675453) < 0)]<-1  ## cases after WBQ
data$renalpostWBQ[which(data$f.42026.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

## mnd

data$mnd[which(is.na(data$f.42028.0.0_ukb675453))]<-0 ## controls
data$mnd[which(!is.na(data$f.42028.0.0_ukb675453))]<-1  ## cases

## for pre and post questionnaire controls only use actual controls not not yet diagnosed cases

data$mndpreMHQ[which(is.na(data$f.42028.0.0_ukb675453))]<-0 ## controls
data$mndpreMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42028.0.0_ukb675453) > 0)]<-1  ## cases before MHQ
data$mndpreMHQ[which(data$f.42028.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$mndpostMHQ[which(is.na(data$f.42028.0.0_ukb675453))]<-0 ## controls
data$mndpostMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42028.0.0_ukb675453) < 0)]<-1  ## cases after MHQ
data$mndpostMHQ[which(data$f.42028.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$mndpreWBQ[which(is.na(data$f.42028.0.0_ukb675453))]<-0 ## controls
data$mndpreWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42028.0.0_ukb675453) > 0)]<-1  ## cases before WBQ
data$mndpreWBQ[which(data$f.42028.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$mndpostWBQ[which(is.na(data$f.42028.0.0_ukb675453))]<-0 ## controls
data$mndpostWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42028.0.0_ukb675453) < 0)]<-1  ## cases after WBQ
data$mndpostWBQ[which(data$f.42028.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

## parkinsons

data$parkinsons[which(is.na(data$f.42030.0.0_ukb675453))]<-0 ## controls
data$parkinsons[which(!is.na(data$f.42030.0.0_ukb675453))]<-1  ## cases

## for pre and post questionnaire controls only use actual controls not not yet diagnosed cases

data$parkinsonspreMHQ[which(is.na(data$f.42030.0.0_ukb675453))]<-0 ## controls
data$parkinsonspreMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42030.0.0_ukb675453) > 0)]<-1  ## cases before MHQ
data$parkinsonspreMHQ[which(data$f.42030.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$parkinsonspostMHQ[which(is.na(data$f.42030.0.0_ukb675453))]<-0 ## controls
data$parkinsonspostMHQ[which(as.Date(data$f.20400.0.0) - as.Date(data$f.42030.0.0_ukb675453) < 0)]<-1  ## cases after MHQ
data$parkinsonspostMHQ[which(data$f.42030.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$parkinsonspreWBQ[which(is.na(data$f.42030.0.0_ukb675453))]<-0 ## controls
data$parkinsonspreWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42030.0.0_ukb675453) > 0)]<-1  ## cases before WBQ
data$parkinsonspreWBQ[which(data$f.42030.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

data$parkinsonspostWBQ[which(is.na(data$f.42030.0.0_ukb675453))]<-0 ## controls
data$parkinsonspostWBQ[which(as.Date(data$f.29197.0.0) - as.Date(data$f.42030.0.0_ukb675453) < 0)]<-1  ## cases after WBQ
data$parkinsonspostWBQ[which(data$f.42030.0.0_ukb675453 == "1900-01-01")]<-NA ## NA for unknown date

table(data$parkinsonspostMHQ, useNA="always")

## output demographics
write.table(data,"demographics.txt",quote=F,row.names=F)


