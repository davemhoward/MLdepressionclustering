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


## Currently depressed in Q1

currentQ1<-data[which(data$PHQ9.Case == 1),]
currentQ1$f.20514.0.0[which(currentQ1$f.20514.0.0 < 0)]<-NA
currentQ1$f.20514.0.0[which(currentQ1$f.20514.0.0 < 3)]<-0
currentQ1$f.20514.0.0[which(currentQ1$f.20514.0.0 >= 3)]<-1
currentQ1$f.20507.0.0[which(currentQ1$f.20507.0.0 < 0)]<-NA
currentQ1$f.20507.0.0[which(currentQ1$f.20507.0.0 < 3)]<-0
currentQ1$f.20507.0.0[which(currentQ1$f.20507.0.0 >= 3)]<-1
currentQ1$f.20510.0.0[which(currentQ1$f.20510.0.0 < 0)]<-NA
currentQ1$f.20510.0.0[which(currentQ1$f.20510.0.0 < 3)]<-0
currentQ1$f.20510.0.0[which(currentQ1$f.20510.0.0 >= 3)]<-1
currentQ1$f.20508.0.0[which(currentQ1$f.20508.0.0 < 0)]<-NA
currentQ1$f.20508.0.0[which(currentQ1$f.20508.0.0 < 3)]<-0
currentQ1$f.20508.0.0[which(currentQ1$f.20508.0.0 >= 3)]<-1
currentQ1$f.20517.0.0[which(currentQ1$f.20517.0.0 < 0)]<-NA
currentQ1$f.20517.0.0[which(currentQ1$f.20517.0.0 < 3)]<-0
currentQ1$f.20517.0.0[which(currentQ1$f.20517.0.0 >= 3)]<-1
currentQ1$f.20518.0.0[which(currentQ1$f.20518.0.0 < 0)]<-NA
currentQ1$f.20518.0.0[which(currentQ1$f.20518.0.0 < 3)]<-0
currentQ1$f.20518.0.0[which(currentQ1$f.20518.0.0 >= 3)]<-1
currentQ1$f.20519.0.0[which(currentQ1$f.20519.0.0 < 0)]<-NA
currentQ1$f.20519.0.0[which(currentQ1$f.20519.0.0 < 3)]<-0
currentQ1$f.20519.0.0[which(currentQ1$f.20519.0.0 >= 3)]<-1
currentQ1$f.20511.0.0[which(currentQ1$f.20511.0.0 < 0)]<-NA
currentQ1$f.20511.0.0[which(currentQ1$f.20511.0.0 < 3)]<-0
currentQ1$f.20511.0.0[which(currentQ1$f.20511.0.0 >= 3)]<-1
currentQ1$f.20513.0.0[which(currentQ1$f.20513.0.0 < 0)]<-NA
currentQ1$f.20513.0.0[which(currentQ1$f.20513.0.0 < 2)]<-0
currentQ1$f.20513.0.0[which(currentQ1$f.20513.0.0 >= 2)]<-1

row.names(currentQ1)<-currentQ1$f.eid

colnames(currentQ1)[which(colnames(currentQ1) == "f.20514.0.0")]<-"Lack of interest or pleasure"
colnames(currentQ1)[which(colnames(currentQ1) == "f.20510.0.0")]<-"Feelings of depression"
colnames(currentQ1)[which(colnames(currentQ1) == "f.20519.0.0")]<-"Tiredness or low energy"
colnames(currentQ1)[which(colnames(currentQ1) == "f.20517.0.0")]<-"Over or under sleeping"
colnames(currentQ1)[which(colnames(currentQ1) == "f.20507.0.0")]<-"Feelings of inadequacy"
colnames(currentQ1)[which(colnames(currentQ1) == "f.20508.0.0")]<-"Trouble concentrating"
colnames(currentQ1)[which(colnames(currentQ1) == "f.20511.0.0")]<-"Changes in appetite"
colnames(currentQ1)[which(colnames(currentQ1) == "f.20513.0.0")]<-"Suicidal thoughts or self-harm"
colnames(currentQ1)[which(colnames(currentQ1) == "f.20518.0.0")]<-"Psychomotor changes"

saveRDS(currentQ1[,c("Lack of interest or pleasure","Feelings of depression","Tiredness or low energy","Over or under sleeping","Feelings of inadequacy","Trouble concentrating","Changes in appetite","Suicidal thoughts or self-harm","Psychomotor changes")],"currentQ1.rds")


## Currently depressed in Q2

currentQ2<-data[which(data$wbPHQ9.Case == 1 & data$PHQ9.Case == 0),]
currentQ2$f.29002.0.0[which(currentQ2$f.29002.0.0 < 0)]<-NA
currentQ2$f.29002.0.0[which(currentQ2$f.29002.0.0 < 2)]<-0
currentQ2$f.29002.0.0[which(currentQ2$f.29002.0.0 >= 2)]<-1
currentQ2$f.29007.0.0[which(currentQ2$f.29007.0.0 < 0)]<-NA
currentQ2$f.29007.0.0[which(currentQ2$f.29007.0.0 < 2)]<-0
currentQ2$f.29007.0.0[which(currentQ2$f.29007.0.0 >= 2)]<-1
currentQ2$f.29003.0.0[which(currentQ2$f.29003.0.0 < 0)]<-NA
currentQ2$f.29003.0.0[which(currentQ2$f.29003.0.0 < 2)]<-0
currentQ2$f.29003.0.0[which(currentQ2$f.29003.0.0 >= 2)]<-1
currentQ2$f.29008.0.0[which(currentQ2$f.29008.0.0 < 0)]<-NA
currentQ2$f.29008.0.0[which(currentQ2$f.29008.0.0 < 2)]<-0
currentQ2$f.29008.0.0[which(currentQ2$f.29008.0.0 >= 2)]<-1
currentQ2$f.29004.0.0[which(currentQ2$f.29004.0.0 < 0)]<-NA
currentQ2$f.29004.0.0[which(currentQ2$f.29004.0.0 < 2)]<-0
currentQ2$f.29004.0.0[which(currentQ2$f.29004.0.0 >= 2)]<-1
currentQ2$f.29009.0.0[which(currentQ2$f.29009.0.0 < 0)]<-NA
currentQ2$f.29009.0.0[which(currentQ2$f.29009.0.0 < 2)]<-0
currentQ2$f.29009.0.0[which(currentQ2$f.29009.0.0 >= 2)]<-1
currentQ2$f.29005.0.0[which(currentQ2$f.29005.0.0 < 0)]<-NA
currentQ2$f.29005.0.0[which(currentQ2$f.29005.0.0 < 2)]<-0
currentQ2$f.29005.0.0[which(currentQ2$f.29005.0.0 >= 2)]<-1
currentQ2$f.29006.0.0[which(currentQ2$f.29006.0.0 < 0)]<-NA
currentQ2$f.29006.0.0[which(currentQ2$f.29006.0.0 < 2)]<-0
currentQ2$f.29006.0.0[which(currentQ2$f.29006.0.0 >= 2)]<-1
currentQ2$f.29010.0.0[which(currentQ2$f.29010.0.0 < 0)]<-NA
currentQ2$f.29010.0.0[which(currentQ2$f.29010.0.0 >= 1)]<-1

colnames(currentQ2)[which(colnames(currentQ2) == "f.29002.0.0")]<-"Lack of interest or pleasure"
colnames(currentQ2)[which(colnames(currentQ2) == "f.29003.0.0")]<-"Feelings of depression"
colnames(currentQ2)[which(colnames(currentQ2) == "f.29005.0.0")]<-"Tiredness or low energy"
colnames(currentQ2)[which(colnames(currentQ2) == "f.29004.0.0")]<-"Over or under sleeping"
colnames(currentQ2)[which(colnames(currentQ2) == "f.29007.0.0")]<-"Feelings of inadequacy"
colnames(currentQ2)[which(colnames(currentQ2) == "f.29008.0.0")]<-"Trouble concentrating"
colnames(currentQ2)[which(colnames(currentQ2) == "f.29006.0.0")]<-"Changes in appetite"
colnames(currentQ2)[which(colnames(currentQ2) == "f.29010.0.0")]<-"Suicidal thoughts or self-harm"
colnames(currentQ2)[which(colnames(currentQ2) == "f.29009.0.0")]<-"Psychomotor changes"

row.names(currentQ2)<-currentQ2$f.eid

saveRDS(currentQ2[,c("Lack of interest or pleasure","Feelings of depression","Tiredness or low energy","Over or under sleeping","Feelings of inadequacy","Trouble concentrating","Changes in appetite","Suicidal thoughts or self-harm","Psychomotor changes")],"currentQ2.rds")


cidigroup<-data[which(data$PHQ9.Case == 0 & (data$CIDI.MDD.Case == 1 | data$wbCIDI.MDD.Case == 1) & data$wbPHQ9.Case == 0),]
set.seed(1234)
cidigroup$random<-round(runif(nrow(cidigroup)))

## Ever depressed in Q1

everQ1<-cidigroup[which((cidigroup$CIDI.MDD.Case == 1 & cidigroup$wbCIDI.MDD.Case == 0) | (cidigroup$CIDI.MDD.Case == 1 & cidigroup$random == 0)),]

everQ1$f.20446.0.0[which(everQ1$f.20446.0.0 < 0)]<-NA
everQ1$f.20441.0.0[which(everQ1$f.20441.0.0 < 0)]<-NA
everQ1$f.20449.0.0[which(everQ1$f.20449.0.0 < 0)]<-NA
everQ1$f.20536.gain<-NA
everQ1$f.20536.gain[which(everQ1$f.20536.0.0 == 0)]<-0
everQ1$f.20536.gain[which(everQ1$f.20536.0.0 == 1)]<-1
everQ1$f.20536.gain[which(everQ1$f.20536.0.0 == 2)]<-0
everQ1$f.20536.loss<-NA
everQ1$f.20536.loss[which(everQ1$f.20536.0.0 == 0)]<-0
everQ1$f.20536.loss[which(everQ1$f.20536.0.0 == 1)]<-0
everQ1$f.20536.loss[which(everQ1$f.20536.0.0 == 2)]<-1
everQ1$f.20435.0.0[which(everQ1$f.20435.0.0 < 0)]<-NA
everQ1$f.20450.0.0[which(everQ1$f.20450.0.0 < 0)]<-NA
everQ1$f.20437.0.0[which(everQ1$f.20437.0.0 < 0)]<-NA


colnames(everQ1)[which(colnames(everQ1) == "f.20446.0.0")]<-"Feelings of depression"
colnames(everQ1)[which(colnames(everQ1) == "f.20441.0.0")]<-"Loss of interest in normal activities"
colnames(everQ1)[which(colnames(everQ1) == "f.20449.0.0")]<-"Feelings of tiredness"
colnames(everQ1)[which(colnames(everQ1) == "f.20536.gain")]<-"Gained weight"
colnames(everQ1)[which(colnames(everQ1) == "f.20536.loss")]<-"Lost weight"
colnames(everQ1)[which(colnames(everQ1) == "f.20533.0.0")]<-"Trouble falling asleep"
colnames(everQ1)[which(colnames(everQ1) == "f.20534.0.0")]<-"Sleeping too much"
colnames(everQ1)[which(colnames(everQ1) == "f.20535.0.0")]<-"Waking too early"
colnames(everQ1)[which(colnames(everQ1) == "f.20435.0.0")]<-"Difficulty concentrating"
colnames(everQ1)[which(colnames(everQ1) == "f.20450.0.0")]<-"Feelings of worthlessness"
colnames(everQ1)[which(colnames(everQ1) == "f.20437.0.0")]<-"Thoughts of death"

row.names(everQ1)<-everQ1$f.eid

saveRDS(everQ1[,c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Trouble falling asleep","Sleeping too much","Waking too early","Feelings of worthlessness","Difficulty concentrating","Gained weight","Lost weight","Thoughts of death")],"everQ1.rds")


## Ever depressed in Q2

everQ2<-cidigroup[which((cidigroup$CIDI.MDD.Case == 0 & cidigroup$wbCIDI.MDD.Case == 1) | (cidigroup$wbCIDI.MDD.Case == 1 & cidigroup$random == 1)),]

everQ2$f.29011.0.0[which(everQ2$f.29011.0.0 < 0)]<-NA
everQ2$f.29012.0.0[which(everQ2$f.29012.0.0 < 0)]<-NA
everQ2$f.29018.0.0[which(everQ2$f.29018.0.0 < 0)]<-NA
everQ2$f.29021.gain<-NA
everQ2$f.29021.gain[which(everQ2$f.29021.0.0 == 0)]<-1
everQ2$f.29021.gain[which(everQ2$f.29021.0.0 == 1)]<-0
everQ2$f.29021.gain[which(everQ2$f.29021.0.0 == 3)]<-0
everQ2$f.29021.loss<-NA
everQ2$f.29021.loss[which(everQ2$f.29021.0.0 == 0)]<-0
everQ2$f.29021.loss[which(everQ2$f.29021.0.0 == 1)]<-1
everQ2$f.29021.loss[which(everQ2$f.29021.0.0 == 3)]<-0
everQ2$f.29026.0.0[which(everQ2$f.29026.0.0 < 0)]<-NA
everQ2$f.29027.0.0[which(everQ2$f.29027.0.0 < 0)]<-NA
everQ2$f.29029.0.0[which(everQ2$f.29029.0.0 < 0)]<-NA

colnames(everQ2)[which(colnames(everQ2) == "f.29011.0.0")]<-"Feelings of depression"
colnames(everQ2)[which(colnames(everQ2) == "f.29012.0.0")]<-"Loss of interest in normal activities"
colnames(everQ2)[which(colnames(everQ2) == "f.29018.0.0")]<-"Feelings of tiredness"
colnames(everQ2)[which(colnames(everQ2) == "f.29021.gain")]<-"Gained weight"
colnames(everQ2)[which(colnames(everQ2) == "f.29021.loss")]<-"Lost weight"
colnames(everQ2)[which(colnames(everQ2) == "f.29023.0.0")]<-"Trouble falling asleep"
colnames(everQ2)[which(colnames(everQ2) == "f.29025.0.0")]<-"Sleeping too much"
colnames(everQ2)[which(colnames(everQ2) == "f.29024.0.0")]<-"Waking too early"
colnames(everQ2)[which(colnames(everQ2) == "f.29026.0.0")]<-"Difficulty concentrating"
colnames(everQ2)[which(colnames(everQ2) == "f.29027.0.0")]<-"Feelings of worthlessness"
colnames(everQ2)[which(colnames(everQ2) == "f.29029.0.0")]<-"Thoughts of death"

row.names(everQ2)<-everQ2$f.eid

saveRDS(everQ2[,c("Loss of interest in normal activities","Feelings of depression","Feelings of tiredness","Trouble falling asleep","Sleeping too much","Waking too early","Feelings of worthlessness","Difficulty concentrating","Gained weight","Lost weight","Thoughts of death")],"everQ2.rds")
