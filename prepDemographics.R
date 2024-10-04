### Load dependencies

library(data.table)
library(Hmisc)
library(stringi)

### Load data

data<-readRDS("fields.rds")

colnames(data) <- paste('f.', colnames(data), sep = '')
colnames(data) <- gsub("-", ".", colnames(data))

#####Demographics#####

### Calculate age at Mental Health Questionnair (MHQ) as age at baseline + (diff in days between baseline and MHQ divided by 365.25)
### Calculate age at Well being Questionnaire (WBQ) as age at baseline + (diff in days between baseline and MHQ divided by 365.25)
data$Age.At.MHQ<-as.numeric(with(data, ifelse(is.na(f.20400.0.0),f.21003.0.0 + ceiling(difftime(median(na.omit(as.Date(data$f.20400.0.0))), as.Date(f.53.0.0), units="days")/365.25),f.21003.0.0 + ceiling(difftime(as.Date(f.20400.0.0), as.Date(f.53.0.0), units="days")/365.25))))
data$Age.At.WBQ<-as.numeric(with(data, ifelse(is.na(f.29197.0.0),f.21003.0.0 + ceiling(difftime(median(na.omit(as.Date(data$f.29197.0.0))), as.Date(f.53.0.0), units="days")/365.25),f.21003.0.0 + ceiling(difftime(as.Date(f.29197.0.0), as.Date(f.53.0.0), units="days")/365.25))))

### Define Gender field

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

data$Migrant.Status<-with(data, ifelse(is.na(f.1647.0.0) | f.1647.0.0 < 0, NA,
			      		ifelse(f.1647.0.0 == 5 | f.1647.0.0 == 6, 1, 0)))

### Group TDI into tertiles

data$TDI.Tertiles<-ordered(with(data, ifelse(is.na(f.189.0.0), NA,
				    	      ifelse(f.189.0.0 >= 2, "Most",
				    	      ifelse(f.189.0.0 < 2 & f.189.0.0 > -2, "Average",
				    	      ifelse(f.189.0.0 <= -2, "Least", NA))))),
				    	      levels=c("Most","Average","Least"))

data$TDI<-data$f.189.0.0

### Average Household Income
data$Income<-ordered(with(data, ifelse(!is.na(f.738.0.0) & f.738.0.0 == 1, "LessThan18K",
		       	      		 ifelse(!is.na(f.738.0.0) & f.738.0.0 == 2, "18Kto30K",
			      		 ifelse(!is.na(f.738.0.0) & f.738.0.0 == 3, "30Kto52K",
			      		 ifelse(!is.na(f.738.0.0) & f.738.0.0 == 4, "52Kto100K",
			      		 ifelse(!is.na(f.738.0.0) & f.738.0.0 == 5, "MoreThan100K",
			      		 ifelse(!is.na(f.738.0.0) & f.738.0.0 < 0, "DontKnowRefuse", NA))))))),
			      		 levels=c("LessThan18K", "18Kto30K", "30Kto52K", "52Kto100K", "MoreThan100K", "DontKnowRefuse"))

### Smoker status (rename as factor)

data$Smoker<-ordered(with(data, ifelse(!is.na(f.20116.0.0) & f.20116.0.0 == 2, "Current",
			      ifelse(!is.na(f.20116.0.0) & f.20116.0.0 == 1, "Former",
			      ifelse(!is.na(f.20116.0.0) & f.20116.0.0 == 0, "Never",
			      ifelse(!is.na(f.20116.0.0) & f.20116.0.0 == -3, "PreferNotToAnswer",NA))))),
			      levels=c("Current", "Former", "Never", "PreferNotToAnswer"))

### Physical activity daily

data$Moderate.Physical.Activity<-with(data, ifelse(is.na(f.884.0.0) | f.884.0.0 < 0, NA,
					  	    ifelse(!is.na(f.884.0.0) & f.884.0.0 > 2, 0, 1)))

### Longstanding illness

data$Longstanding.Illness<-with(data, ifelse(is.na(f.2188.0.0) | f.2188.0.0 < 0, NA, f.2188.0.0))

### Diabetes

data$Diabetes<-with(data, ifelse(is.na(f.2443.0.0) | f.2443.0.0 < 0, NA, f.2443.0.0))

### Cancer

data$Cancer<-with(data, ifelse(is.na(f.2453.0.0) | f.2453.0.0 < 0, NA, f.2453.0.0))

### CVD

data$CVD<-with(data, ifelse(!is.na(f.6150.0.0) & f.6150.0.0 == -7, 0,
		   	     ifelse(!is.na(f.6150.0.0) & f.6150.0.0 > 0, 1, NA)))

## BMI

data$BMI<-data$f.21001.0.0

## Number of Depressive episodes reported at each questionnaire. 1 is given for recurrent depression, 0 for single episode

## Set -818 and NA to missing, 1 as 0 and everything else as 1
data$MHQ_recurrent<-with(data, ifelse(is.na(f.20442.0.0) | f.20442.0.0 == -818, NA,
			      		ifelse(f.20442.0.0 == 1, 0, 1)))
## Set -1, -3 and NA to missing, 1 as 0 and everything else as 1
data$WBQ_recurrent<-with(data, ifelse(is.na(f.29033.0.0) | f.29033.0.0 == -1 | f.29033.0.0 == -3, NA,
			      		ifelse(f.29033.0.0 == 1, 0, 1)))

## Treatment Resitant Depression based on Chris Lo defintion

trd_case<-read.table("TRD_AD_eids_ukb82087.txt",header=T)
trd_assessed<-read.table("depression_single.txt",header=T)

data$trd<-NA
data$trd[which(data$f.eid %in% trd_assessed$eid)]<-0  ## Any individuals assessed for TRD set as 0
data$trd[which(data$f.eid %in% trd_case$eid)]<-1  ## The set any TRD case individuals as 1

## output demographics
write.table(data[,c(1,899:922)],"MHQ_demographics.txt",quote=F,row.names=F)
