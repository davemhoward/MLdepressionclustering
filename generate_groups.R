## generate groups for analysis
## based on code available here: https://data.mendeley.com/datasets/kv677c2th4/3 to define UK Biobank depression from this paper: doi:10.1192/bjo.2019.100

library(data.table)
library(Hmisc)
library(stringi)

data<-readRDS("depressionsymptoms.rds")

colnames(data) <- paste('f.', colnames(data), sep = '')
colnames(data) <- gsub("-", ".", colnames(data))

exclude<-read.table("sczbpcases.txt",header=F) 
nrow(exclude)

## remove schizophrenia and bipolar cases
data<-data[which(!(data$f.eid %in% exclude$V1)),]

## MHQ PHQ9

data$PHQ9.No.Info<-with(data,ifelse((is.na(f.20514.0.0) | f.20514.0.0 < 0) &
                                  (is.na(f.20510.0.0) | f.20510.0.0 < 0),1,0))

data$PHQ9.Screen<-with(data,ifelse(((!is.na(f.20514.0.0) & f.20514.0.0 >= 3) |
				  (!is.na(f.20510.0.0) & f.20510.0.0 >= 3)) &
				 (!is.na(PHQ9.No.Info) & PHQ9.No.Info == 0),1,0))
data$PHQ9.Items<-0

data$PHQ9.Items<-with(data, ifelse(!is.na(f.20514.0.0) & f.20514.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
data$PHQ9.Items<-with(data, ifelse(!is.na(f.20507.0.0) & f.20507.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
data$PHQ9.Items<-with(data, ifelse(!is.na(f.20510.0.0) & f.20510.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
data$PHQ9.Items<-with(data, ifelse(!is.na(f.20508.0.0) & f.20508.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
data$PHQ9.Items<-with(data, ifelse(!is.na(f.20517.0.0) & f.20517.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
data$PHQ9.Items<-with(data, ifelse(!is.na(f.20518.0.0) & f.20518.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
data$PHQ9.Items<-with(data, ifelse(!is.na(f.20519.0.0) & f.20519.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
data$PHQ9.Items<-with(data, ifelse(!is.na(f.20511.0.0) & f.20511.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
data$PHQ9.Items<-with(data, ifelse(!is.na(f.20513.0.0) & f.20513.0.0 >= 2, PHQ9.Items + 1, PHQ9.Items))

table(data$PHQ9.No.Info)
table(data$PHQ9.Screen)
nrow(data[which(data$PHQ9.Screen==1 & data$PHQ9.Items > 4),])

data$PHQ9.Case<-0
data$PHQ9.Case[which(data$PHQ9.Screen==1 & data$PHQ9.Items > 4)]<-1

## Well-being PHQ9 - NOTE different scoring of variables compared to MHQ

data$wbPHQ9.No.Info<-with(data,ifelse((is.na(f.29002.0.0) | f.29002.0.0 < 0) &
                                  (is.na(f.29003.0.0) | f.29003.0.0 < 0),1,0))

data$wbPHQ9.Screen<-with(data,ifelse(((!is.na(f.29002.0.0) & f.29002.0.0 >= 2) |
				  (!is.na(f.29003.0.0) & f.29003.0.0 >= 2)) &
				 (!is.na(wbPHQ9.No.Info) & wbPHQ9.No.Info == 0),1,0))
data$wbPHQ9.Items<-0

data$wbPHQ9.Items<-with(data, ifelse(!is.na(f.29002.0.0) & f.29002.0.0 >= 2, wbPHQ9.Items + 1, wbPHQ9.Items))
data$wbPHQ9.Items<-with(data, ifelse(!is.na(f.29007.0.0) & f.29007.0.0 >= 2, wbPHQ9.Items + 1, wbPHQ9.Items))
data$wbPHQ9.Items<-with(data, ifelse(!is.na(f.29003.0.0) & f.29003.0.0 >= 2, wbPHQ9.Items + 1, wbPHQ9.Items))
data$wbPHQ9.Items<-with(data, ifelse(!is.na(f.29008.0.0) & f.29008.0.0 >= 2, wbPHQ9.Items + 1, wbPHQ9.Items))
data$wbPHQ9.Items<-with(data, ifelse(!is.na(f.29004.0.0) & f.29004.0.0 >= 2, wbPHQ9.Items + 1, wbPHQ9.Items))
data$wbPHQ9.Items<-with(data, ifelse(!is.na(f.29009.0.0) & f.29009.0.0 >= 2, wbPHQ9.Items + 1, wbPHQ9.Items))
data$wbPHQ9.Items<-with(data, ifelse(!is.na(f.29005.0.0) & f.29005.0.0 >= 2, wbPHQ9.Items + 1, wbPHQ9.Items))
data$wbPHQ9.Items<-with(data, ifelse(!is.na(f.29006.0.0) & f.29006.0.0 >= 2, wbPHQ9.Items + 1, wbPHQ9.Items))
data$wbPHQ9.Items<-with(data, ifelse(!is.na(f.29010.0.0) & f.29010.0.0 >= 1, wbPHQ9.Items + 1, wbPHQ9.Items))

table(data$wbPHQ9.No.Info)
table(data$wbPHQ9.Screen)
nrow(data[which(data$wbPHQ9.Screen==1 & data$wbPHQ9.Items > 4),])

data$wbPHQ9.Case<-0
data$wbPHQ9.Case[which(data$wbPHQ9.Screen==1 & data$wbPHQ9.Items > 4)]<-1

###  MHQ CIDI

data$CIDI.MDD.No.Info<-with(data,ifelse(((is.na(f.20446.0.0) | f.20446.0.0 < 0) &
				       (is.na(f.20441.0.0) | f.20441.0.0 < 0 )), 1, 0))

data$CIDI.MDD.Screen<-with(data,ifelse(((!is.na(f.20446.0.0) & f.20446.0.0 == 1) |
			              (!is.na(f.20441.0.0) & f.20441.0.0 == 1)) &
				     (!is.na(f.20436.0.0) & f.20436.0.0 > 2) &  ## Fraction of day affected: Most of the day or all day long
				     (!is.na(f.20439.0.0) & f.20439.0.0 > 1) &  ## Freq of depressed days: Almost everyday or everyday
				     (!is.na(f.20440.0.0) & f.20440.0.0 > 1), 1, 0)) ## Impact of depression: Somewhat or a lot

data$CIDI.MDD.Response<-0

data$CIDI.MDD.Response<-with(data, ifelse(!is.na(f.20446.0.0) & f.20446.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
data$CIDI.MDD.Response<-with(data, ifelse(!is.na(f.20441.0.0) & f.20441.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
data$CIDI.MDD.Response<-with(data, ifelse(!is.na(f.20449.0.0) & f.20449.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
data$CIDI.MDD.Response<-with(data, ifelse(!is.na(f.20536.0.0) & f.20536.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
data$CIDI.MDD.Response<-with(data, ifelse(!is.na(f.20532.0.0) & f.20532.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
data$CIDI.MDD.Response<-with(data, ifelse(!is.na(f.20435.0.0) & f.20435.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
data$CIDI.MDD.Response<-with(data, ifelse(!is.na(f.20450.0.0) & f.20450.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
data$CIDI.MDD.Response<-with(data, ifelse(!is.na(f.20437.0.0) & f.20437.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))


table(data$CIDI.MDD.No.Info)
table(data$CIDI.MDD.Screen)
nrow(data[which(data$CIDI.MDD.Screen==1 & data$CIDI.MDD.Response > 4),])

data$CIDI.MDD.Case<-0
data$CIDI.MDD.Case[which(data$CIDI.MDD.Screen==1 & data$CIDI.MDD.Response > 4)]<-1


###  Well-being CIDI

data$wbCIDI.MDD.No.Info<-with(data,ifelse(((is.na(f.29011.0.0) | f.29011.0.0 < 0) &
				       (is.na(f.29012.0.0) | f.29012.0.0 < 0 )), 1, 0))


data$wbCIDI.MDD.Screen<-with(data,ifelse(((!is.na(f.29011.0.0) & f.29011.0.0 == 1) |
			              (!is.na(f.29012.0.0) & f.29012.0.0 == 1)) &
				     (!is.na(f.29014.0.0) & (f.29014.0.0 == 1 | f.29014.0.0 == 0)) &   ## 29014 scored as fraction of day as all of the day (0) or most of the day (1)
				     (!is.na(f.29015.0.0) & (f.29015.0.0 == 1 | f.29015.0.0 == 0)) &   ## 29015 scored as freq as everyday (0) or almost every day (1)
				     (!is.na(f.29031.0.0) & (f.29031.0.0 == 1 | f.29031.0.0 == 0)), 1, 0)) ## 20931 scoreas as impact as somewhat (1) of a lot (0)


data$wbCIDI.MDD.Response<-0

data$wbCIDI.MDD.Response<-with(data, ifelse(!is.na(f.29011.0.0) & f.29011.0.0 > 0, wbCIDI.MDD.Response + 1, wbCIDI.MDD.Response))
data$wbCIDI.MDD.Response<-with(data, ifelse(!is.na(f.29012.0.0) & f.29012.0.0 > 0, wbCIDI.MDD.Response + 1, wbCIDI.MDD.Response))
data$wbCIDI.MDD.Response<-with(data, ifelse(!is.na(f.29018.0.0) & f.29018.0.0 > 0, wbCIDI.MDD.Response + 1, wbCIDI.MDD.Response))  ## tiredness. yes (1). all other values < 1
data$wbCIDI.MDD.Response<-with(data, ifelse(!is.na(f.29021.0.0) & (f.29021.0.0 == 0 | f.29021.0.0 == 1 | f.29021.0.0 == 2), wbCIDI.MDD.Response + 1, wbCIDI.MDD.Response)) ## weight change: gained (0), lost (1) and both (2)
data$wbCIDI.MDD.Response<-with(data, ifelse(!is.na(f.29022.0.0) & f.29022.0.0 > 0, wbCIDI.MDD.Response + 1, wbCIDI.MDD.Response))  ## sleep change: yes (1). all other values < 1
data$wbCIDI.MDD.Response<-with(data, ifelse(!is.na(f.29026.0.0) & f.29026.0.0 > 0, wbCIDI.MDD.Response + 1, wbCIDI.MDD.Response))  ## concentration: yes (1). all other values < 1
data$wbCIDI.MDD.Response<-with(data, ifelse(!is.na(f.29027.0.0) & f.29027.0.0 > 0, wbCIDI.MDD.Response + 1, wbCIDI.MDD.Response))  ## worthlessness: yes (1). all other values < 1
data$wbCIDI.MDD.Response<-with(data, ifelse(!is.na(f.29029.0.0) & f.29029.0.0 > 0, wbCIDI.MDD.Response + 1, wbCIDI.MDD.Response))  ## thoughts of death: yes (1). all other values < 1

table(data$wbCIDI.MDD.No.Info)
table(data$wbCIDI.MDD.Screen)
nrow(data[which(data$wbCIDI.MDD.Screen==1 & data$wbCIDI.MDD.Response > 4),])

data$wbCIDI.MDD.Case<-0
data$wbCIDI.MDD.Case[which(data$wbCIDI.MDD.Screen==1 & data$wbCIDI.MDD.Response > 4)]<-1

##

data$any<-with(data, ifelse((!is.na(PHQ9.Case) & PHQ9.Case == 1) |
						(!is.na(wbCIDI.MDD.Case) & wbCIDI.MDD.Case == 1) |
						(!is.na(CIDI.MDD.Case) & CIDI.MDD.Case == 1) |
					    (!is.na(wbPHQ9.Case) & wbPHQ9.Case == 1), 1, 0))


## Group A is currently depressed in Q1
groupA<-data[which(data$PHQ9.Case == 1),]
groupB<-data[which(data$PHQ9.Case == 1),]
cidigroup<-data[which(data$PHQ9.Case == 0 & (data$CIDI.MDD.Case == 1 | data$wbCIDI.MDD.Case == 1) & data$wbPHQ9.Case == 0),]
set.seed(1234)
cidigroup$random<-round(runif(nrow(cidigroup)))
groupC<-cidigroup[which((cidigroup$CIDI.MDD.Case == 1 & cidigroup$wbCIDI.MDD.Case == 0) | (cidigroup$CIDI.MDD.Case == 1 & cidigroup$random == 0)),]
## Group D is ever depressed in Q1
groupD<-cidigroup[which((cidigroup$CIDI.MDD.Case == 1 & cidigroup$wbCIDI.MDD.Case == 0) | (cidigroup$CIDI.MDD.Case == 1 & cidigroup$random == 0)),]
groupG<-cidigroup[which((cidigroup$CIDI.MDD.Case == 0 & cidigroup$wbCIDI.MDD.Case == 1) | (cidigroup$wbCIDI.MDD.Case == 1 & cidigroup$random == 1)),]
groupH<-cidigroup[which((cidigroup$CIDI.MDD.Case == 0 & cidigroup$wbCIDI.MDD.Case == 1) | (cidigroup$wbCIDI.MDD.Case == 1 & cidigroup$random == 1)),]
## Group E is currently depressed in Q2
groupE<-data[which(data$wbPHQ9.Case == 1 & data$PHQ9.Case == 0),]
groupF<-data[which(data$wbPHQ9.Case == 1 & data$PHQ9.Case == 0),]

## Group H is ever depressed in Q2

nrow(groupA)
table(groupA$f.31.0.0)
nrow(groupB)
table(groupB$f.31.0.0)
nrow(groupC)
table(groupC$f.31.0.0)
nrow(groupD)
table(groupD$f.31.0.0)
nrow(groupE)
table(groupE$f.31.0.0)
nrow(groupF)
table(groupF$f.31.0.0)
nrow(groupG)
table(groupG$f.31.0.0)
nrow(groupH)
table(groupH$f.31.0.0)


## GroupA MHQ PHQ Binary

groupA$f.20514.0.0[which(groupA$f.20514.0.0 < 0)]<-NA
groupA$f.20514.0.0[which(groupA$f.20514.0.0 < 3)]<-0
groupA$f.20514.0.0[which(groupA$f.20514.0.0 >= 3)]<-1
groupA$f.20507.0.0[which(groupA$f.20507.0.0 < 0)]<-NA
groupA$f.20507.0.0[which(groupA$f.20507.0.0 < 3)]<-0
groupA$f.20507.0.0[which(groupA$f.20507.0.0 >= 3)]<-1
groupA$f.20510.0.0[which(groupA$f.20510.0.0 < 0)]<-NA
groupA$f.20510.0.0[which(groupA$f.20510.0.0 < 3)]<-0
groupA$f.20510.0.0[which(groupA$f.20510.0.0 >= 3)]<-1
groupA$f.20508.0.0[which(groupA$f.20508.0.0 < 0)]<-NA
groupA$f.20508.0.0[which(groupA$f.20508.0.0 < 3)]<-0
groupA$f.20508.0.0[which(groupA$f.20508.0.0 >= 3)]<-1
groupA$f.20517.0.0[which(groupA$f.20517.0.0 < 0)]<-NA
groupA$f.20517.0.0[which(groupA$f.20517.0.0 < 3)]<-0
groupA$f.20517.0.0[which(groupA$f.20517.0.0 >= 3)]<-1
groupA$f.20518.0.0[which(groupA$f.20518.0.0 < 0)]<-NA
groupA$f.20518.0.0[which(groupA$f.20518.0.0 < 3)]<-0
groupA$f.20518.0.0[which(groupA$f.20518.0.0 >= 3)]<-1
groupA$f.20519.0.0[which(groupA$f.20519.0.0 < 0)]<-NA
groupA$f.20519.0.0[which(groupA$f.20519.0.0 < 3)]<-0
groupA$f.20519.0.0[which(groupA$f.20519.0.0 >= 3)]<-1
groupA$f.20511.0.0[which(groupA$f.20511.0.0 < 0)]<-NA
groupA$f.20511.0.0[which(groupA$f.20511.0.0 < 3)]<-0
groupA$f.20511.0.0[which(groupA$f.20511.0.0 >= 3)]<-1
groupA$f.20513.0.0[which(groupA$f.20513.0.0 < 0)]<-NA
groupA$f.20513.0.0[which(groupA$f.20513.0.0 < 2)]<-0
groupA$f.20513.0.0[which(groupA$f.20513.0.0 >= 2)]<-1

row.names(groupA)<-groupA$f.eid

colnames(groupA)[which(colnames(groupA) == "f.20514.0.0")]<-"Lack of interest or pleasure"
colnames(groupA)[which(colnames(groupA) == "f.20510.0.0")]<-"Feelings of depression"
colnames(groupA)[which(colnames(groupA) == "f.20519.0.0")]<-"Tiredness or low energy"
colnames(groupA)[which(colnames(groupA) == "f.20517.0.0")]<-"Over or under sleeping"
colnames(groupA)[which(colnames(groupA) == "f.20507.0.0")]<-"Feelings of inadequacy"
colnames(groupA)[which(colnames(groupA) == "f.20508.0.0")]<-"Trouble concentrating"
colnames(groupA)[which(colnames(groupA) == "f.20511.0.0")]<-"Changes in appetite"
colnames(groupA)[which(colnames(groupA) == "f.20513.0.0")]<-"Suicidal thoughts or self-harm"
colnames(groupA)[which(colnames(groupA) == "f.20518.0.0")]<-"Psychomotor changes"

saveRDS(groupA[,c("Lack of interest or pleasure","Feelings of depression","Tiredness or low energy","Over or under sleeping","Feelings of inadequacy","Trouble concentrating","Changes in appetite","Suicidal thoughts or self-harm","Psychomotor changes")],"groupA.rds")
saveRDS(groupA[which(groupA$f.31.0.0 == 1),c("Lack of interest or pleasure","Feelings of depression","Tiredness or low energy","Over or under sleeping","Feelings of inadequacy","Trouble concentrating","Changes in appetite","Suicidal thoughts or self-harm","Psychomotor changes")],"groupAmales.rds")
saveRDS(groupA[which(groupA$f.31.0.0 == 0),c("Lack of interest or pleasure","Feelings of depression","Tiredness or low energy","Over or under sleeping","Feelings of inadequacy","Trouble concentrating","Changes in appetite","Suicidal thoughts or self-harm","Psychomotor changes")],"groupAfemales.rds")

## GroupB MHQ PHQ Ordinal

groupB$f.20514.0.0[which(groupB$f.20514.0.0 < 0)]<-NA
groupB$f.20507.0.0[which(groupB$f.20507.0.0 < 0)]<-NA
groupB$f.20510.0.0[which(groupB$f.20510.0.0 < 0)]<-NA
groupB$f.20508.0.0[which(groupB$f.20508.0.0 < 0)]<-NA
groupB$f.20517.0.0[which(groupB$f.20517.0.0 < 0)]<-NA
groupB$f.20518.0.0[which(groupB$f.20518.0.0 < 0)]<-NA
groupB$f.20519.0.0[which(groupB$f.20519.0.0 < 0)]<-NA
groupB$f.20511.0.0[which(groupB$f.20511.0.0 < 0)]<-NA
groupB$f.20513.0.0[which(groupB$f.20513.0.0 < 0)]<-NA

row.names(groupB)<-groupB$f.eid

saveRDS(groupB[,c("f.20514.0.0","f.20507.0.0","f.20510.0.0","f.20508.0.0","f.20517.0.0","f.20518.0.0","f.20519.0.0","f.20511.0.0","f.20513.0.0")],"groupB.rds")
saveRDS(groupB[which(groupB$f.31.0.0 == 1),c("f.20514.0.0","f.20507.0.0","f.20510.0.0","f.20508.0.0","f.20517.0.0","f.20518.0.0","f.20519.0.0","f.20511.0.0","f.20513.0.0")],"groupBmales.rds")
saveRDS(groupB[which(groupB$f.31.0.0 == 0),c("f.20514.0.0","f.20507.0.0","f.20510.0.0","f.20508.0.0","f.20517.0.0","f.20518.0.0","f.20519.0.0","f.20511.0.0","f.20513.0.0")],"groupBfemales.rds")

## GroupC MHQ CIDI 8 questions

groupC$f.20446.0.0[which(groupC$f.20446.0.0 < 0)]<-NA
groupC$f.20441.0.0[which(groupC$f.20441.0.0 < 0)]<-NA
groupC$f.20449.0.0[which(groupC$f.20449.0.0 < 0)]<-NA
groupC$f.20536.0.0[which(groupC$f.20536.0.0 < 0)]<-NA
groupC$f.20536.0.0[which(groupC$f.20536.0.0 >= 1)]<-1
groupC$f.20532.0.0[which(groupC$f.20532.0.0 < 0)]<-NA
groupC$f.20435.0.0[which(groupC$f.20435.0.0 < 0)]<-NA
groupC$f.20450.0.0[which(groupC$f.20450.0.0 < 0)]<-NA
groupC$f.20437.0.0[which(groupC$f.20437.0.0 < 0)]<-NA

colnames(groupC)[which(colnames(groupC) == "f.20441.0.0")]<-"Loss of interest in normal activities"
colnames(groupC)[which(colnames(groupC) == "f.20446.0.0")]<-"Feelings of depression"
colnames(groupC)[which(colnames(groupC) == "f.20449.0.0")]<-"Feelings of tiredness"
colnames(groupC)[which(colnames(groupC) == "f.20532.0.0")]<-"Sleep changes"
colnames(groupC)[which(colnames(groupC) == "f.20450.0.0")]<-"Feelings of worthlessness"
colnames(groupC)[which(colnames(groupC) == "f.20435.0.0")]<-"Difficulty concentrating"
colnames(groupC)[which(colnames(groupC) == "f.20536.0.0")]<-"Weight changes"
colnames(groupC)[which(colnames(groupC) == "f.20437.0.0")]<-"Thoughts of death"

row.names(groupC)<-groupC$f.eid

saveRDS(groupC[,c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Sleep changes","Feelings of worthlessness","Difficulty concentrating","Weight changes","Thoughts of death")],"groupC.rds")
saveRDS(groupC[which(groupC$f.31.0.0 == 1),c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Sleep changes","Feelings of worthlessness","Difficulty concentrating","Weight changes","Thoughts of death")],"groupCmales.rds")
saveRDS(groupC[which(groupC$f.31.0.0 == 0),c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Sleep changes","Feelings of worthlessness","Difficulty concentrating","Weight changes","Thoughts of death")],"groupCfemales.rds")

## GroupD MHQ CIDI 11 questions

groupD$f.20446.0.0[which(groupD$f.20446.0.0 < 0)]<-NA
groupD$f.20441.0.0[which(groupD$f.20441.0.0 < 0)]<-NA
groupD$f.20449.0.0[which(groupD$f.20449.0.0 < 0)]<-NA
groupD$f.20536.gain<-NA
groupD$f.20536.gain[which(groupD$f.20536.0.0 == 0)]<-0
groupD$f.20536.gain[which(groupD$f.20536.0.0 == 1)]<-1
groupD$f.20536.gain[which(groupD$f.20536.0.0 == 2)]<-0
groupD$f.20536.loss<-NA
groupD$f.20536.loss[which(groupD$f.20536.0.0 == 0)]<-0
groupD$f.20536.loss[which(groupD$f.20536.0.0 == 1)]<-0
groupD$f.20536.loss[which(groupD$f.20536.0.0 == 2)]<-1
groupD$f.20435.0.0[which(groupD$f.20435.0.0 < 0)]<-NA
groupD$f.20450.0.0[which(groupD$f.20450.0.0 < 0)]<-NA
groupD$f.20437.0.0[which(groupD$f.20437.0.0 < 0)]<-NA


colnames(groupD)[which(colnames(groupD) == "f.20446.0.0")]<-"Feelings of depression"
colnames(groupD)[which(colnames(groupD) == "f.20441.0.0")]<-"Loss of interest in normal activities"
colnames(groupD)[which(colnames(groupD) == "f.20449.0.0")]<-"Feelings of tiredness"
colnames(groupD)[which(colnames(groupD) == "f.20536.gain")]<-"Gained weight"
colnames(groupD)[which(colnames(groupD) == "f.20536.loss")]<-"Lost weight"
colnames(groupD)[which(colnames(groupD) == "f.20533.0.0")]<-"Trouble falling asleep"
colnames(groupD)[which(colnames(groupD) == "f.20534.0.0")]<-"Sleeping too much"
colnames(groupD)[which(colnames(groupD) == "f.20535.0.0")]<-"Waking too early"
colnames(groupD)[which(colnames(groupD) == "f.20435.0.0")]<-"Difficulty concentrating"
colnames(groupD)[which(colnames(groupD) == "f.20450.0.0")]<-"Feelings of worthlessness"
colnames(groupD)[which(colnames(groupD) == "f.20437.0.0")]<-"Thoughts of death"

row.names(groupD)<-groupD$f.eid

saveRDS(groupD[,c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Trouble falling asleep","Sleeping too much","Waking too early","Feelings of worthlessness","Difficulty concentrating","Gained weight","Lost weight","Thoughts of death")],"groupD.rds")
saveRDS(groupD[which(groupD$f.31.0.0 == 1),c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Trouble falling asleep","Sleeping too much","Waking too early","Feelings of worthlessness","Difficulty concentrating","Gained weight","Lost weight","Thoughts of death")],"groupDmales.rds")
saveRDS(groupD[which(groupD$f.31.0.0 == 0),c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Trouble falling asleep","Sleeping too much","Waking too early","Feelings of worthlessness","Difficulty concentrating","Gained weight","Lost weight","Thoughts of death")],"groupDfemales.rds")

## GroupE Well-being PHQ Binary

groupE$f.29002.0.0[which(groupE$f.29002.0.0 < 0)]<-NA
groupE$f.29002.0.0[which(groupE$f.29002.0.0 < 2)]<-0
groupE$f.29002.0.0[which(groupE$f.29002.0.0 >= 2)]<-1
groupE$f.29007.0.0[which(groupE$f.29007.0.0 < 0)]<-NA
groupE$f.29007.0.0[which(groupE$f.29007.0.0 < 2)]<-0
groupE$f.29007.0.0[which(groupE$f.29007.0.0 >= 2)]<-1
groupE$f.29003.0.0[which(groupE$f.29003.0.0 < 0)]<-NA
groupE$f.29003.0.0[which(groupE$f.29003.0.0 < 2)]<-0
groupE$f.29003.0.0[which(groupE$f.29003.0.0 >= 2)]<-1
groupE$f.29008.0.0[which(groupE$f.29008.0.0 < 0)]<-NA
groupE$f.29008.0.0[which(groupE$f.29008.0.0 < 2)]<-0
groupE$f.29008.0.0[which(groupE$f.29008.0.0 >= 2)]<-1
groupE$f.29004.0.0[which(groupE$f.29004.0.0 < 0)]<-NA
groupE$f.29004.0.0[which(groupE$f.29004.0.0 < 2)]<-0
groupE$f.29004.0.0[which(groupE$f.29004.0.0 >= 2)]<-1
groupE$f.29009.0.0[which(groupE$f.29009.0.0 < 0)]<-NA
groupE$f.29009.0.0[which(groupE$f.29009.0.0 < 2)]<-0
groupE$f.29009.0.0[which(groupE$f.29009.0.0 >= 2)]<-1
groupE$f.29005.0.0[which(groupE$f.29005.0.0 < 0)]<-NA
groupE$f.29005.0.0[which(groupE$f.29005.0.0 < 2)]<-0
groupE$f.29005.0.0[which(groupE$f.29005.0.0 >= 2)]<-1
groupE$f.29006.0.0[which(groupE$f.29006.0.0 < 0)]<-NA
groupE$f.29006.0.0[which(groupE$f.29006.0.0 < 2)]<-0
groupE$f.29006.0.0[which(groupE$f.29006.0.0 >= 2)]<-1
groupE$f.29010.0.0[which(groupE$f.29010.0.0 < 0)]<-NA
groupE$f.29010.0.0[which(groupE$f.29010.0.0 >= 1)]<-1

colnames(groupE)[which(colnames(groupE) == "f.29002.0.0")]<-"Lack of interest or pleasure"
colnames(groupE)[which(colnames(groupE) == "f.29003.0.0")]<-"Feelings of depression"
colnames(groupE)[which(colnames(groupE) == "f.29005.0.0")]<-"Tiredness or low energy"
colnames(groupE)[which(colnames(groupE) == "f.29004.0.0")]<-"Over or under sleeping"
colnames(groupE)[which(colnames(groupE) == "f.29007.0.0")]<-"Feelings of inadequacy"
colnames(groupE)[which(colnames(groupE) == "f.29008.0.0")]<-"Trouble concentrating"
colnames(groupE)[which(colnames(groupE) == "f.29006.0.0")]<-"Changes in appetite"
colnames(groupE)[which(colnames(groupE) == "f.29010.0.0")]<-"Suicidal thoughts or self-harm"
colnames(groupE)[which(colnames(groupE) == "f.29009.0.0")]<-"Psychomotor changes"

row.names(groupE)<-groupE$f.eid

saveRDS(groupE[,c("Lack of interest or pleasure","Feelings of depression","Tiredness or low energy","Over or under sleeping","Feelings of inadequacy","Trouble concentrating","Changes in appetite","Suicidal thoughts or self-harm","Psychomotor changes")],"groupE.rds")
saveRDS(groupE[which(groupE$f.31.0.0 == 1),c("Lack of interest or pleasure","Feelings of depression","Tiredness or low energy","Over or under sleeping","Feelings of inadequacy","Trouble concentrating","Changes in appetite","Suicidal thoughts or self-harm","Psychomotor changes")],"groupEmales.rds")
saveRDS(groupE[which(groupE$f.31.0.0 == 0),c("Lack of interest or pleasure","Feelings of depression","Tiredness or low energy","Over or under sleeping","Feelings of inadequacy","Trouble concentrating","Changes in appetite","Suicidal thoughts or self-harm","Psychomotor changes")],"groupEfemales.rds")


## GroupF Well-being PHQ Ordinal

groupF$f.29002.0.0[which(groupF$f.29002.0.0 < 0)]<-NA
groupF$f.29002.0.0<-groupF$f.29002.0.0+1
groupF$f.29007.0.0[which(groupF$f.29007.0.0 < 0)]<-NA
groupF$f.29007.0.0<-groupF$f.29007.0.0+1
groupF$f.29003.0.0[which(groupF$f.29003.0.0 < 0)]<-NA
groupF$f.29003.0.0<-groupF$f.29003.0.0+1
groupF$f.29008.0.0[which(groupF$f.29008.0.0 < 0)]<-NA
groupF$f.29008.0.0<-groupF$f.29008.0.0+1
groupF$f.29004.0.0[which(groupF$f.29004.0.0 < 0)]<-NA
groupF$f.29004.0.0<-groupF$f.29004.0.0+1
groupF$f.29009.0.0[which(groupF$f.29009.0.0 < 0)]<-NA
groupF$f.29009.0.0<-groupF$f.29009.0.0+1
groupF$f.29005.0.0[which(groupF$f.29005.0.0 < 0)]<-NA
groupF$f.29005.0.0<-groupF$f.29005.0.0+1
groupF$f.29006.0.0[which(groupF$f.29006.0.0 < 0)]<-NA
groupF$f.29006.0.0<-groupF$f.29006.0.0+1
groupF$f.29010.0.0[which(groupF$f.29010.0.0 < 0)]<-NA
groupF$f.29010.0.0<-groupF$f.29010.0.0+1

row.names(groupF)<-groupF$f.eid

saveRDS(groupF[,c("f.29002.0.0","f.29007.0.0","f.29003.0.0","f.29008.0.0","f.29004.0.0","f.29009.0.0","f.29005.0.0","f.29006.0.0","f.29010.0.0")],"groupF.rds")
saveRDS(groupF[which(groupF$f.31.0.0 == 1),c("f.29002.0.0","f.29007.0.0","f.29003.0.0","f.29008.0.0","f.29004.0.0","f.29009.0.0","f.29005.0.0","f.29006.0.0","f.29010.0.0")],"groupFmales.rds")
saveRDS(groupF[which(groupF$f.31.0.0 == 0),c("f.29002.0.0","f.29007.0.0","f.29003.0.0","f.29008.0.0","f.29004.0.0","f.29009.0.0","f.29005.0.0","f.29006.0.0","f.29010.0.0")],"groupFfemales.rds")


## GroupG Well-being CIDI 8 questions

groupG$f.29011.0.0[which(groupG$f.29011.0.0 < 0)]<-NA
groupG$f.29012.0.0[which(groupG$f.29012.0.0 < 0)]<-NA
groupG$f.29018.0.0[which(groupG$f.29018.0.0 < 0)]<-NA
groupG$f.29021.0.0[which(groupG$f.29021.0.0 < 0)]<-NA
groupG$f.29021.0.0[which(groupG$f.29021.0.0 < 3)]<-1
groupG$f.29021.0.0[which(groupG$f.29021.0.0 == 3)]<-0
groupG$f.29022.0.0[which(groupG$f.29022.0.0 < 0)]<-NA
groupG$f.29026.0.0[which(groupG$f.29026.0.0 < 0)]<-NA
groupG$f.29027.0.0[which(groupG$f.29027.0.0 < 0)]<-NA
groupG$f.29029.0.0[which(groupG$f.29029.0.0 < 0)]<-NA


colnames(groupG)[which(colnames(groupG) == "f.29012.0.0")]<-"Loss of interest in normal activities"
colnames(groupG)[which(colnames(groupG) == "f.29011.0.0")]<-"Feelings of depression"
colnames(groupG)[which(colnames(groupG) == "f.29018.0.0")]<-"Feelings of tiredness"
colnames(groupG)[which(colnames(groupG) == "f.29022.0.0")]<-"Sleep changes"
colnames(groupG)[which(colnames(groupG) == "f.29027.0.0")]<-"Feelings of worthlessness"
colnames(groupG)[which(colnames(groupG) == "f.29026.0.0")]<-"Difficulty concentrating"
colnames(groupG)[which(colnames(groupG) == "f.29021.0.0")]<-"Weight changes"
colnames(groupG)[which(colnames(groupG) == "f.29029.0.0")]<-"Thoughts of death"


row.names(groupG)<-groupG$f.eid

saveRDS(groupG[,c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Sleep changes","Feelings of worthlessness","Difficulty concentrating","Weight changes","Thoughts of death")],"groupG.rds")
saveRDS(groupG[which(groupG$f.31.0.0 == 1),c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Sleep changes","Feelings of worthlessness","Difficulty concentrating","Weight changes","Thoughts of death")],"groupGmales.rds")
saveRDS(groupG[which(groupG$f.31.0.0 == 0),c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Sleep changes","Feelings of worthlessness","Difficulty concentrating","Weight changes","Thoughts of death")],"groupGfemales.rds")


## GroupH Well-being CIDI 11 questions


groupH$f.29011.0.0[which(groupH$f.29011.0.0 < 0)]<-NA
groupH$f.29012.0.0[which(groupH$f.29012.0.0 < 0)]<-NA
groupH$f.29018.0.0[which(groupH$f.29018.0.0 < 0)]<-NA
groupH$f.29021.gain<-NA
groupH$f.29021.gain[which(groupH$f.29021.0.0 == 0)]<-1
groupH$f.29021.gain[which(groupH$f.29021.0.0 == 1)]<-0
groupH$f.29021.gain[which(groupH$f.29021.0.0 == 3)]<-0
groupH$f.29021.loss<-NA
groupH$f.29021.loss[which(groupH$f.29021.0.0 == 0)]<-0
groupH$f.29021.loss[which(groupH$f.29021.0.0 == 1)]<-1
groupH$f.29021.loss[which(groupH$f.29021.0.0 == 3)]<-0
groupH$f.29026.0.0[which(groupH$f.29026.0.0 < 0)]<-NA
groupH$f.29027.0.0[which(groupH$f.29027.0.0 < 0)]<-NA
groupH$f.29029.0.0[which(groupH$f.29029.0.0 < 0)]<-NA

colnames(groupH)[which(colnames(groupH) == "f.29011.0.0")]<-"Feelings of depression"
colnames(groupH)[which(colnames(groupH) == "f.29012.0.0")]<-"Loss of interest in normal activities"
colnames(groupH)[which(colnames(groupH) == "f.29018.0.0")]<-"Feelings of tiredness"
colnames(groupH)[which(colnames(groupH) == "f.29021.gain")]<-"Gained weight"
colnames(groupH)[which(colnames(groupH) == "f.29021.loss")]<-"Lost weight"
colnames(groupH)[which(colnames(groupH) == "f.29023.0.0")]<-"Trouble falling asleep"
colnames(groupH)[which(colnames(groupH) == "f.29025.0.0")]<-"Sleeping too much"
colnames(groupH)[which(colnames(groupH) == "f.29024.0.0")]<-"Waking too early"
colnames(groupH)[which(colnames(groupH) == "f.29026.0.0")]<-"Difficulty concentrating"
colnames(groupH)[which(colnames(groupH) == "f.29027.0.0")]<-"Feelings of worthlessness"
colnames(groupH)[which(colnames(groupH) == "f.29029.0.0")]<-"Thoughts of death"

row.names(groupH)<-groupH$f.eid

saveRDS(groupH[,c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Trouble falling asleep","Sleeping too much","Waking too early","Feelings of worthlessness","Difficulty concentrating","Gained weight","Lost weight","Thoughts of death")],"groupH.rds")
saveRDS(groupH[which(groupH$f.31.0.0 == 1),c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Trouble falling asleep","Sleeping too much","Waking too early","Feelings of worthlessness","Difficulty concentrating","Gained weight","Lost weight","Thoughts of death")],"groupHmales.rds")
saveRDS(groupH[which(groupH$f.31.0.0 == 0),c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Trouble falling asleep","Sleeping too much","Waking too early","Feelings of worthlessness","Difficulty concentrating","Gained weight","Lost weight","Thoughts of death")],"groupHfemales.rds")

##

library(VennDiagram)

## https://stackoverflow.com/questions/74869486/how-to-make-a-venn-diagram-with-venndiagram-for-4-sets-in-r 

## 1 is MHQ PHQ
## 2 is well-being PHQ
## 3 is MHQ CIDI
## 4 is well-being CIDI

area1<-nrow(data[which(data$PHQ9.No.Info == 0),])
area2<-nrow(data[which(data$wbPHQ9.No.Info == 0),])
area3<-nrow(data[which(data$CIDI.MDD.No.Info==0),])
area4<-nrow(data[which(data$wbCIDI.MDD.No.Info==0),])
n12<-nrow(data[which(data$PHQ9.No.Info == 0 & data$wbPHQ9.No.Info == 0),])
n13<-nrow(data[which(data$PHQ9.No.Info == 0 & data$CIDI.MDD.No.Info==0),])
n14<-nrow(data[which(data$PHQ9.No.Info == 0 & data$wbCIDI.MDD.No.Info==0),])
n23<-nrow(data[which(data$wbPHQ9.No.Info == 0 & data$CIDI.MDD.No.Info==0),])
n24<-nrow(data[which(data$wbPHQ9.No.Info == 0 & data$wbCIDI.MDD.No.Info==0),])
n34<-nrow(data[which(data$CIDI.MDD.No.Info==0 & data$wbCIDI.MDD.No.Info==0),])
n123<-nrow(data[which(data$PHQ9.No.Info == 0 & data$wbPHQ9.No.Info == 0 & data$CIDI.MDD.No.Info==0),])
n124<-nrow(data[which(data$PHQ9.No.Info == 0 & data$wbPHQ9.No.Info == 0 & data$wbCIDI.MDD.No.Info==0),])
n134<-nrow(data[which(data$PHQ9.No.Info == 0 & data$CIDI.MDD.No.Info==0 & data$wbCIDI.MDD.No.Info==0),])
n234<-nrow(data[which(data$wbPHQ9.No.Info == 0 & data$CIDI.MDD.No.Info==0 & data$wbCIDI.MDD.No.Info==0),])
n1234<-nrow(data[which(data$PHQ9.No.Info == 0 & data$wbPHQ9.No.Info == 0 & data$CIDI.MDD.No.Info==0 & data$wbCIDI.MDD.No.Info==0),])

#nrow(data[which(data$PHQ9.No.Info == 0 & data$wbPHQ9.No.Info == 0 & data$CIDI.MDD.No.Info==0 & data$wbCIDI.MDD.No.Info==0),])

draw.quad.venn(area1, area2, area3, area4, n12, n13, n14, n23, n24, n34, n123, n124, n134, n234, n1234,
  category = c("MHQ PHQ9", "Well-being PHQ", "MHQ CIDI-SF", "Well-being CIDI-SF"),
  fill = c("orange", "red", "green", "blue"),
  cex = 2,
  cat.cex = 2,
  cat.col = c("orange", "red", "green", "blue")
)



area1<-nrow(data[which(data$PHQ9.Case == 1),])
area2<-nrow(data[which(data$wbPHQ9.Case == 1),])
area3<-nrow(data[which(data$CIDI.MDD.Case==1),])
area4<-nrow(data[which(data$wbCIDI.MDD.Case==1),])
n12<-nrow(data[which(data$PHQ9.Case == 1 & data$wbPHQ9.Case == 1),])
n13<-nrow(data[which(data$PHQ9.Case == 1 & data$CIDI.MDD.Case==1),])
n14<-nrow(data[which(data$PHQ9.Case == 1 & data$wbCIDI.MDD.Case==1),])
n23<-nrow(data[which(data$wbPHQ9.Case == 1 & data$CIDI.MDD.Case==1),])
n24<-nrow(data[which(data$wbPHQ9.Case == 1 & data$wbCIDI.MDD.Case==1),])
n34<-nrow(data[which(data$CIDI.MDD.Case==1 & data$wbCIDI.MDD.Case==1),])
n123<-nrow(data[which(data$PHQ9.Case == 1 & data$wbPHQ9.Case == 1 & data$CIDI.MDD.Case==1),])
n124<-nrow(data[which(data$PHQ9.Case == 1 & data$wbPHQ9.Case == 1 & data$wbCIDI.MDD.Case==1),])
n134<-nrow(data[which(data$PHQ9.Case == 1 & data$CIDI.MDD.Case==1 & data$wbCIDI.MDD.Case==1),])
n234<-nrow(data[which(data$wbPHQ9.Case == 1 & data$CIDI.MDD.Case==1 & data$wbCIDI.MDD.Case==1),])
n1234<-nrow(data[which(data$PHQ9.Case == 1 & data$wbPHQ9.Case == 1 & data$CIDI.MDD.Case==1 & data$wbCIDI.MDD.Case==1),])

draw.quad.venn(area1, area2, area3, area4, n12, n13, n14, n23, n24, n34, n123, n124, n134, n234, n1234,
  category = c("MHQ PHQ9", "Well-being PHQ", "MHQ CIDI-SF", "Well-being CIDI-SF"),
  fill = c("orange", "red", "green", "blue"),
  cex = 2,
  cat.cex = 2,
  cat.col = c("orange", "red", "green", "blue")
)

