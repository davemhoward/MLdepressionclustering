### Built using R 4.3.0
### Load dependencies

library(data.table)
library(Hmisc)
library(stringi)

### Load data

MHQ_Full<-readRDS("data_fields.rds")

colnames(MHQ_Full) <- paste('f.', colnames(MHQ_Full), sep = '')
colnames(MHQ_Full) <- gsub("-", ".", colnames(MHQ_Full))


#MHQ<-MHQ_Full[!is.na(MHQ_Full$f.20499.0.0), ]
## shift to full cohort
MHQ<-MHQ_Full


MHQ$SRSchizophrenia<-with(MHQ, ifelse(is.na(f.20499.0.0), NA, 
			       ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 2) |
		                      (!is.na(f.20544.0.2) & f.20544.0.2 == 2) |
		                      (!is.na(f.20544.0.3) & f.20544.0.3 == 2) |
		                      (!is.na(f.20544.0.4) & f.20544.0.4 == 2) |
		                      (!is.na(f.20544.0.5) & f.20544.0.5 == 2) |
		                      (!is.na(f.20544.0.6) & f.20544.0.6 == 2) |
		                      (!is.na(f.20544.0.7) & f.20544.0.7 == 2) |
		                      (!is.na(f.20544.0.8) & f.20544.0.8 == 2) |
		                      (!is.na(f.20544.0.9) & f.20544.0.9 == 2) |
		                      (!is.na(f.20544.0.10) & f.20544.0.10 == 2) |
		                      (!is.na(f.20544.0.11) & f.20544.0.11 == 2) |
		                      (!is.na(f.20544.0.12) & f.20544.0.12 == 2) |
		                      (!is.na(f.20544.0.13) & f.20544.0.13 == 2) |
		                      (!is.na(f.20544.0.14) & f.20544.0.14 == 2) |
		                      (!is.na(f.20544.0.15) & f.20544.0.15 == 2) |
		                      (!is.na(f.20544.0.16) & f.20544.0.16 == 2), 1, 0)))

MHQ$SRPsychosisOther<-with(MHQ, ifelse(is.na(f.20499.0.0), NA, 
			        ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 3) |
		                       (!is.na(f.20544.0.2) & f.20544.0.2 == 3) |
		                       (!is.na(f.20544.0.3) & f.20544.0.3 == 3) |
		                       (!is.na(f.20544.0.4) & f.20544.0.4 == 3) |
		                       (!is.na(f.20544.0.5) & f.20544.0.5 == 3) |
		                       (!is.na(f.20544.0.6) & f.20544.0.6 == 3) |
		                       (!is.na(f.20544.0.7) & f.20544.0.7 == 3) |
		                       (!is.na(f.20544.0.8) & f.20544.0.8 == 3) |
		                       (!is.na(f.20544.0.9) & f.20544.0.9 == 3) |
		                       (!is.na(f.20544.0.10) & f.20544.0.10 == 3) |
		                       (!is.na(f.20544.0.11) & f.20544.0.11 == 3) |
		                       (!is.na(f.20544.0.12) & f.20544.0.12 == 3) |
		                       (!is.na(f.20544.0.13) & f.20544.0.13 == 3) |
		                       (!is.na(f.20544.0.14) & f.20544.0.14 == 3) |
		                       (!is.na(f.20544.0.15) & f.20544.0.15 == 3) |
		                       (!is.na(f.20544.0.16) & f.20544.0.16 == 3), 1, 0)))

MHQ$SRPsychosisAny<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
					 ifelse((!is.na(SRSchizophrenia) & SRSchizophrenia == 1) | (!is.na(SRPsychosisOther) & SRPsychosisOther == 1), 1, 0)))


MHQ$SRManiaBIP<-with(MHQ, ifelse(is.na(f.20499.0.0), NA, 
			  ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 10) |
		                 (!is.na(f.20544.0.2) & f.20544.0.2 == 10) |
		                 (!is.na(f.20544.0.3) & f.20544.0.3 == 10) |
		                 (!is.na(f.20544.0.4) & f.20544.0.4 == 10) |
		                 (!is.na(f.20544.0.5) & f.20544.0.5 == 10) |
		                 (!is.na(f.20544.0.6) & f.20544.0.6 == 10) |
		                 (!is.na(f.20544.0.7) & f.20544.0.7 == 10) |
		                 (!is.na(f.20544.0.8) & f.20544.0.8 == 10) |
		                 (!is.na(f.20544.0.9) & f.20544.0.9 == 10) |
		                 (!is.na(f.20544.0.10) & f.20544.0.10 == 10) |
		                 (!is.na(f.20544.0.11) & f.20544.0.11 == 10) |
		                 (!is.na(f.20544.0.12) & f.20544.0.12 == 10) |
		                 (!is.na(f.20544.0.13) & f.20544.0.13 == 10) |
		                 (!is.na(f.20544.0.14) & f.20544.0.14 == 10) |
		                 (!is.na(f.20544.0.15) & f.20544.0.15 == 10) |
		                 (!is.na(f.20544.0.16) & f.20544.0.16 == 10), 1, 0)))


MHQ$SRDepression<-with(MHQ, ifelse(is.na(f.20499.0.0), NA, 
			    ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 11) |
		                   (!is.na(f.20544.0.2) & f.20544.0.2 == 11) |
		                   (!is.na(f.20544.0.3) & f.20544.0.3 == 11) |
		                   (!is.na(f.20544.0.4) & f.20544.0.4 == 11) |
		                   (!is.na(f.20544.0.5) & f.20544.0.5 == 11) |
		                   (!is.na(f.20544.0.6) & f.20544.0.6 == 11) |
		                   (!is.na(f.20544.0.7) & f.20544.0.7 == 11) |
		                   (!is.na(f.20544.0.8) & f.20544.0.8 == 11) |
		                   (!is.na(f.20544.0.9) & f.20544.0.9 == 11) |
		                   (!is.na(f.20544.0.10) & f.20544.0.10 == 11) |
		                   (!is.na(f.20544.0.11) & f.20544.0.11 == 11) |
		                   (!is.na(f.20544.0.12) & f.20544.0.12 == 11) |
		                   (!is.na(f.20544.0.13) & f.20544.0.13 == 11) |
		                   (!is.na(f.20544.0.14) & f.20544.0.14 == 11) |
		                   (!is.na(f.20544.0.15) & f.20544.0.15 == 11) |
		                   (!is.na(f.20544.0.16) & f.20544.0.16 == 11), 1, 0)))


InterviewDepression<-apply(MHQ[,grep("f.20002.0", colnames(MHQ))], 1, function(row) "1286" %in% row)

MHQ$InterviewDepression[c(InterviewDepression)]<-1



MHQ$PHQ9.No.Info<-with(MHQ,ifelse((is.na(f.20514.0.0) | f.20514.0.0 < 0) &
                                  (is.na(f.20510.0.0) | f.20510.0.0 < 0),1,0))

MHQ$PHQ9.Screen<-with(MHQ,ifelse(((!is.na(f.20514.0.0) & f.20514.0.0 >= 2) |
				  (!is.na(f.20510.0.0) & f.20510.0.0 >= 2)) &
				 (!is.na(PHQ9.No.Info) & PHQ9.No.Info == 0),1,0))
MHQ$PHQ9.Items<-0

MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20514.0.0) & f.20514.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20507.0.0) & f.20507.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20510.0.0) & f.20510.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20508.0.0) & f.20508.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20517.0.0) & f.20517.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20518.0.0) & f.20518.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20519.0.0) & f.20519.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20511.0.0) & f.20511.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20513.0.0) & f.20513.0.0 >= 2, PHQ9.Items + 1, PHQ9.Items))



MHQ$f.20514.0.0<-as.numeric(unlist(MHQ$f.20514.0.0))
MHQ$f.20507.0.0<-as.numeric(unlist(MHQ$f.20507.0.0))
MHQ$f.20510.0.0<-as.numeric(unlist(MHQ$f.20510.0.0))
MHQ$f.20508.0.0<-as.numeric(unlist(MHQ$f.20508.0.0))
MHQ$f.20517.0.0<-as.numeric(unlist(MHQ$f.20517.0.0))
MHQ$f.20518.0.0<-as.numeric(unlist(MHQ$f.20518.0.0))
MHQ$f.20519.0.0<-as.numeric(unlist(MHQ$f.20519.0.0))
MHQ$f.20513.0.0<-as.numeric(unlist(MHQ$f.20513.0.0))
MHQ$f.20511.0.0<-as.numeric(unlist(MHQ$f.20511.0.0))

MHQ$PHQ9.Severity<-with(MHQ, ifelse(f.20514.0.0 < 0 | is.na(f.20514.0.0), 0, f.20514.0.0-1) +
			     ifelse(f.20507.0.0 < 0 | is.na(f.20507.0.0), 0, f.20507.0.0-1) +
			     ifelse(f.20510.0.0 < 0 | is.na(f.20510.0.0), 0, f.20510.0.0-1) +
			     ifelse(f.20508.0.0 < 0 | is.na(f.20508.0.0), 0, f.20508.0.0-1) +
			     ifelse(f.20517.0.0 < 0 | is.na(f.20517.0.0), 0, f.20517.0.0-1) +
			     ifelse(f.20518.0.0 < 0 | is.na(f.20518.0.0), 0, f.20518.0.0-1) +
			     ifelse(f.20519.0.0 < 0 | is.na(f.20519.0.0), 0, f.20519.0.0-1) +
			     ifelse(f.20513.0.0 < 0 | is.na(f.20513.0.0), 0, f.20513.0.0-1) +
			     ifelse(f.20511.0.0 < 0 | is.na(f.20511.0.0), 0, f.20511.0.0-1))

MHQ$CIDI.MDD.No.Info<-with(MHQ,ifelse(((is.na(f.20446.0.0) | f.20446.0.0 < 0) &
				       (is.na(f.20441.0.0) | f.20441.0.0 < 0 )), 1, 0))

MHQ$CIDI.MDD.Screen<-with(MHQ,ifelse(((!is.na(f.20446.0.0) & f.20446.0.0 == 1) |
			              (!is.na(f.20441.0.0) & f.20441.0.0 == 1)) &
				     (!is.na(f.20436.0.0) & f.20436.0.0 > 2) &
				     (!is.na(f.20439.0.0) & f.20439.0.0 > 1) &
				     (!is.na(f.20440.0.0) & f.20440.0.0 > 1), 1, 0))

MHQ$CIDI.MDD.Response<-0

MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20446.0.0) & f.20446.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20441.0.0) & f.20441.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20449.0.0) & f.20449.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20536.0.0) & f.20536.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20532.0.0) & f.20532.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20435.0.0) & f.20435.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20450.0.0) & f.20450.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20437.0.0) & f.20437.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))


MHQ$Depressed.Ever<-with(MHQ, ifelse(CIDI.MDD.No.Info == 1, NA,
			      ifelse(CIDI.MDD.Screen == 1 & (!is.na(CIDI.MDD.Response) & CIDI.MDD.Response > 4), 1, 
			      ifelse(CIDI.MDD.Screen == 0 & 
			             (!is.na(PHQ9.Severity) & PHQ9.Severity < 5) & 
				     (is.na(SRDepression) | (!is.na(SRDepression) & SRDepression == 0)) &
				     (is.na(InterviewDepression) | (!is.na(InterviewDepression) & InterviewDepression == 0)), 0, NA))))



MHQ$SmithBipolar<-with(MHQ, ifelse(is.na(f.20126.0.0), NA,
			    ifelse(!is.na(f.20126.0.0) & f.20126.0.0 < 3 & f.20126.0.0 > 0, 1, 0)))

MHQ$SmithMood<-with(MHQ, ifelse(is.na(f.20126.0.0), NA,
		       	 ifelse(!is.na(f.20126.0.0) & f.20126.0.0 > 0, 1, 0)))



MHQ$Total.Manifestations<-0

MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.1) & f.20548.0.1 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.2) & f.20548.0.2 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.3) & f.20548.0.3 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.4) & f.20548.0.4 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.5) & f.20548.0.5 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.6) & f.20548.0.6 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.7) & f.20548.0.7 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.8) & f.20548.0.8 > 0, Total.Manifestations + 1, Total.Manifestations))

### Wider defined bipolar: mania plus three manifestations, or irritability plus four manifestations, for a week or more


MHQ$Wider.Bipolar.Definition<-with(MHQ, ifelse(((is.na(f.20501.0.0) | f.20501.0.0 < 0) &
				 	        (is.na(f.20502.0.0) | f.20502.0.0 < 0)), NA,
				        ifelse(((f.20501.0.0 == 1 &
				       	        MHQ$Total.Manifestations > 2) |
				               (f.20502.0.0 == 1 &
				                MHQ$Total.Manifestations > 3)) &
				       	      !is.na(f.20492.0.0) & f.20492.0.0 == 3, 1, 
				        ifelse((!is.na(SRManiaBIP) & SRManiaBIP == 0) & (is.na(SmithBipolar) | (!is.na(SmithBipolar) & SmithBipolar == 0)), 0, NA))))



MHQ$BPDI<-with(MHQ, ifelse(is.na(Wider.Bipolar.Definition), NA,
		    ifelse(Wider.Bipolar.Definition == 1 &
			  (!is.na(f.20493.0.0) & f.20493.0.0 == 1) &
			  (!is.na(Depressed.Ever) & Depressed.Ever == 1), 1, 0)))

MHQ$BPDII<-with(MHQ, ifelse(is.na(Wider.Bipolar.Definition), NA,
		     ifelse(Wider.Bipolar.Definition == 1 &
			   (!is.na(f.20493.0.0) & f.20493.0.0 == 0) &
			   (!is.na(Depressed.Ever) & Depressed.Ever == 1), 1, 0)))



## print(summary(Wider.Bipolar.Definition ~ SmithBipolar, data=MHQ, fun=table, na.include=TRUE))


## well-being questionnaire

well<-MHQ_Full

well$SRSchizophrenia<-with(well, ifelse((!is.na(f.29000.0.0) & f.29000.0.0 == 3) |
		                      (!is.na(f.29000.0.1) & f.29000.0.1 == 3) |
		                      (!is.na(f.29000.0.2) & f.29000.0.2 == 3) |
		                      (!is.na(f.29000.0.3) & f.29000.0.3 == 3) |
		                      (!is.na(f.29000.0.4) & f.29000.0.4 == 3) |
		                      (!is.na(f.29000.0.5) & f.29000.0.5 == 3) |
		                      (!is.na(f.29000.0.6) & f.29000.0.6 == 3) |
		                      (!is.na(f.29000.0.7) & f.29000.0.7 == 3) |
		                      (!is.na(f.29000.0.8) & f.29000.0.8 == 3) |
		                      (!is.na(f.29000.0.9) & f.29000.0.9 == 3) |
		                      (!is.na(f.29000.0.10) & f.29000.0.10 == 3) |
		                      (!is.na(f.29000.0.11) & f.29000.0.11 == 3) |
		                      (!is.na(f.29000.0.12) & f.29000.0.12 == 3) |
		                      (!is.na(f.29000.0.13) & f.29000.0.13 == 3) |
		                      (!is.na(f.29000.0.14) & f.29000.0.14 == 3) |
		                      (!is.na(f.29000.0.15) & f.29000.0.15 == 3) |
		                      (!is.na(f.29000.0.16) & f.29000.0.16 == 3) |
		                      (!is.na(f.29000.0.17) & f.29000.0.17 == 3) |
		                      (!is.na(f.29000.0.18) & f.29000.0.18 == 3) |
		                      (!is.na(f.29000.0.19) & f.29000.0.19 == 3), 1, 0))


well$SRPsychosisOther<-with(well, ifelse((!is.na(f.29000.0.0) & f.29000.0.0 == 4) |
		                      (!is.na(f.29000.0.1) & f.29000.0.1 == 4) |
		                      (!is.na(f.29000.0.2) & f.29000.0.2 == 4) |
		                      (!is.na(f.29000.0.3) & f.29000.0.3 == 4) |
		                      (!is.na(f.29000.0.4) & f.29000.0.4 == 4) |
		                      (!is.na(f.29000.0.5) & f.29000.0.5 == 4) |
		                      (!is.na(f.29000.0.6) & f.29000.0.6 == 4) |
		                      (!is.na(f.29000.0.7) & f.29000.0.7 == 4) |
		                      (!is.na(f.29000.0.8) & f.29000.0.8 == 4) |
		                      (!is.na(f.29000.0.9) & f.29000.0.9 == 4) |
		                      (!is.na(f.29000.0.10) & f.29000.0.10 == 4) |
		                      (!is.na(f.29000.0.11) & f.29000.0.11 == 4) |
		                      (!is.na(f.29000.0.12) & f.29000.0.12 == 4) |
		                      (!is.na(f.29000.0.13) & f.29000.0.13 == 4) |
		                      (!is.na(f.29000.0.14) & f.29000.0.14 == 4) |
		                      (!is.na(f.29000.0.15) & f.29000.0.15 == 4) |
		                      (!is.na(f.29000.0.16) & f.29000.0.16 == 4) |
		                      (!is.na(f.29000.0.17) & f.29000.0.17 == 4) |
		                      (!is.na(f.29000.0.18) & f.29000.0.18 == 4) |
		                      (!is.na(f.29000.0.19) & f.29000.0.19 == 4), 1, 0))

well$SRPsychosisAny<-with(well, ifelse((!is.na(SRSchizophrenia) & SRSchizophrenia == 1) |
					    (!is.na(SRPsychosisOther) & SRPsychosisOther == 1), 1, 0))

well$SRManiaBIP<-with(well, ifelse((!is.na(f.29000.0.0) & f.29000.0.0 == 2) |
		                      (!is.na(f.29000.0.1) & f.29000.0.1 == 2) |
		                      (!is.na(f.29000.0.2) & f.29000.0.2 == 2) |
		                      (!is.na(f.29000.0.3) & f.29000.0.3 == 2) |
		                      (!is.na(f.29000.0.4) & f.29000.0.4 == 2) |
		                      (!is.na(f.29000.0.5) & f.29000.0.5 == 2) |
		                      (!is.na(f.29000.0.6) & f.29000.0.6 == 2) |
		                      (!is.na(f.29000.0.7) & f.29000.0.7 == 2) |
		                      (!is.na(f.29000.0.8) & f.29000.0.8 == 2) |
		                      (!is.na(f.29000.0.9) & f.29000.0.9 == 2) |
		                      (!is.na(f.29000.0.10) & f.29000.0.10 == 2) |
		                      (!is.na(f.29000.0.11) & f.29000.0.11 == 2) |
		                      (!is.na(f.29000.0.12) & f.29000.0.12 == 2) |
		                      (!is.na(f.29000.0.13) & f.29000.0.13 == 2) |
		                      (!is.na(f.29000.0.14) & f.29000.0.14 == 2) |
		                      (!is.na(f.29000.0.15) & f.29000.0.15 == 2) |
		                      (!is.na(f.29000.0.16) & f.29000.0.16 == 2) |
		                      (!is.na(f.29000.0.17) & f.29000.0.17 == 2) |
		                      (!is.na(f.29000.0.18) & f.29000.0.18 == 2) |
		                      (!is.na(f.29000.0.19) & f.29000.0.19 == 2), 1, 0))

well$SmithBipolar<-with(well, ifelse(is.na(f.20126.0.0), NA,
			    ifelse(!is.na(f.20126.0.0) & f.20126.0.0 < 3 & f.20126.0.0 > 0, 1, 0)))


well$Total.Manifestations<-0

well$Total.Manifestations<-with(well, ifelse(!is.na(f.29051.0.0) & f.29051.0.0 > 0, Total.Manifestations + 1, Total.Manifestations))
well$Total.Manifestations<-with(well, ifelse(!is.na(f.29051.0.1) & f.29051.0.1 > 0, Total.Manifestations + 1, Total.Manifestations))
well$Total.Manifestations<-with(well, ifelse(!is.na(f.29051.0.2) & f.29051.0.2 > 0, Total.Manifestations + 1, Total.Manifestations))
well$Total.Manifestations<-with(well, ifelse(!is.na(f.29051.0.3) & f.29051.0.3 > 0, Total.Manifestations + 1, Total.Manifestations))
well$Total.Manifestations<-with(well, ifelse(!is.na(f.29051.0.4) & f.29051.0.4 > 0, Total.Manifestations + 1, Total.Manifestations))
well$Total.Manifestations<-with(well, ifelse(!is.na(f.29051.0.5) & f.29051.0.5 > 0, Total.Manifestations + 1, Total.Manifestations))
well$Total.Manifestations<-with(well, ifelse(!is.na(f.29051.0.6) & f.29051.0.6 > 0, Total.Manifestations + 1, Total.Manifestations))
well$Total.Manifestations<-with(well, ifelse(!is.na(f.29051.0.7) & f.29051.0.7 > 0, Total.Manifestations + 1, Total.Manifestations))


### Wider defined bipolar: mania plus three manifestations, or irritability plus four manifestations, for a week or more
#straight swap of 20501 with 29049
#straight swap of 20502 with 29050
#straight swap of 20492 with 29052

well$Wider.Bipolar.Definition<-with(well, ifelse(((is.na(f.29049.0.0) | f.29049.0.0 < 0) &
				 	        (is.na(f.29050.0.0) | f.29050.0.0 < 0)), NA,
				        ifelse(((f.29049.0.0 == 1 &
				       	        well$Total.Manifestations > 2) |
				               (f.29050.0.0 == 1 &
				                well$Total.Manifestations > 3)) &
				       	      !is.na(f.29052.0.0) & f.29052.0.0 == 3, 1, 
				        ifelse((!is.na(SRManiaBIP) & SRManiaBIP == 0) & (is.na(SmithBipolar) | (!is.na(SmithBipolar) & SmithBipolar == 0)), 0, NA))))



table(MHQ$SRPsychosisAny)   ## 721
table(MHQ$SRManiaBIP)       ## 836
table(MHQ$Wider.Bipolar.Definition)  ## 1613


table(well$SRPsychosisAny)  ## 707
table(well$SRManiaBIP)      ## 825
table(well$Wider.Bipolar.Definition) ## 1493

output<-merge(MHQ[,c("f.eid","SRPsychosisAny","SRManiaBIP","Wider.Bipolar.Definition")],well[,c("f.eid","SRPsychosisAny","SRManiaBIP","Wider.Bipolar.Definition")],by="f.eid")

output$Any<-with(output, ifelse((!is.na(SRPsychosisAny.x) & SRPsychosisAny.x == 1) |
						(!is.na(SRManiaBIP.x) & SRManiaBIP.x == 1) |
						(!is.na(Wider.Bipolar.Definition.x) & Wider.Bipolar.Definition.x == 1) |
						(!is.na(SRPsychosisAny.y) & SRPsychosisAny.y == 1) |
						(!is.na(SRManiaBIP.y) & SRManiaBIP.y == 1) |
					    (!is.na(Wider.Bipolar.Definition.y) & Wider.Bipolar.Definition.y == 1), 1, 0))

write.table(output[which(output$Any == 1),1], file="sczbpcases.txt", quote=FALSE, sep="", row.names=FALSE, col.names=FALSE) 
