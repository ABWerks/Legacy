#----------------------------------------------------------------------------------------------
# Original Data from "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/YYYY/samadult.sas"
# For YYYY %in% 2009:2012
#----------------------------------------------------------------------------------------------
# You'll need the foreign package or the read.SAScii package to read the data into R
# If you are really interested email me and I can send you a load script or share the dropbox link
#----------------------------------------------------------------------------------------------
# Documentation at: # ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/NHIS/2012/samadult_layout.pdf
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

rm(list=ls())
load("/Users/adam/Dropbox/Data/statr1_fp.rdata")
#---------------------------------------------------------------------------------------------
# Packages for Build:
require(MASS)
require(Hmisc)
require(plyr)
#---------------------------------------------------------------------------------------------
# Survey Structure
# ID
# Use FPX variable in combination with HHX, FMX, and SRVY_YR to identify individual persons.

# Survey weight
# WTFA_SA: This weight should be used for most sample adult analyses when using a full year of data. This weight includes post-stratification adjustments (age, race/ethnicity, sex) using Census Bureau population control totals. The sum of these weights is equal to the average of the civilian, noninstitutionalized U.S. population estimates for persons aged 18 and above for February, May, August, and November.

# For combining survey weights across years they recommend either dividing by the number of years or developing own weights.

# NHIS Survey Specs (see documentation)
# nhissvy <- svydesign(id=~psu_p, strata=~strat_p,
						# nest = TRUE,
						# weights=~wtfa,
						# data=< existing data frame name>)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Design variables
vars.structure <- c("fpx","fmx","hhx","wtfa_sa","wtia_sa","srvy_yr"
                    ,"psu_p","strat_p","intv_qrt")
#------------------------------------------------------------------------------
# combine years
ds2009$year<-2009;ds2010$year<-2010;ds2011$year<-2011;ds2012$year<-2012            
ds <- rbind.fill(ds2009,ds2010,ds2011,ds2012)
names(ds) <- tolower(names(ds))
ds$id <- paste(ds$srvy_yr,ds$fpx,ds$hhx,ds$fmx)
#------------------------------------------------------------------------------
# Adjust the sample weights
ds$sw <- ds$wtfa_sa/length(unique(ds$year))
#------------------------------------------------------------------------------
# Features shared between years
features.yearly <- aggregate(ds,list(year=ds$year),function(x) all(is.na(x)))
features.yearly <- apply(features.yearly,2,sum)
features <- features.yearly[features.yearly<1]
#------------------------------------------------------------------------------
# Feature cleaning
#------------------------------------------------------------------------------
features.drop <- vars.structure
mm <- ds[,names(features)]
#------------------------------------------------------------------------------
# Region
mm$region <- factor(mm$region,levels=1:4,labels=c("Northeast","Midwest","South","West"))
#------------------------------------------------------------------------------
# Gender
mm$female <- ifelse(mm$sex==2,1,0)
ds$female <- ifelse(ds$sex==2,1,0)
#------------------------------------------------------------------------------
# Time
mm$svy.time <- factor(paste(mm$srvy_yr,mm$intv_qrt,sep="."))
#------------------------------------------------------------------------------
# Hispanic
mm$hisp <- ifelse(mm$hispan_i!=12,1,0)
ds$hisp <- ifelse(ds$hispan_i!=12,1,0)
#------------------------------------------------------------------------------
# OMB Race
mm$race <- factor(mm$racerpi2,levels=1:6,labels=c("White","Black","AIAN","Asian","Secret","Multi"))
#------------------------------------------------------------------------------
# Age
mm$agecat <- cut2(mm$age_p,g=5)
#------------------------------------------------------------------------------
# Marital Status
mm$marital <- factor(ifelse(mm$r_maritl %in% c(1,8),"Partner",ifelse(mm$r_maritl %in% c(2,3,6),"Separated",ifelse(mm$r_maritl==5,"Divorced",ifelse(mm$r_maritl==7,"Single","Unknown")))))

ds$marital <- ifelse(ds$r_maritl %in% c(1,8),1
                     ,ifelse(ds$r_maritl %in% c(2,3,6),2
                             ,ifelse(ds$r_maritl==5,3
                                     ,ifelse(ds$r_maritl==7,4,5))))
#------------------------------------------------------------------------------
# Age Any Cancer Dx 
vars.cage <- names(mm)[grep("^(canage)",names(mm))]
mm$canage <- apply(mm[,vars.cage],1,function(x){
	mean(x[x<97],na.rm=TRUE)
})

ds$canage <- apply(ds[,vars.cage],1,function(x){
  mean(x[x<97],na.rm=TRUE)
})

#------------------------------------------------------------------------------
# Employment 
mm$emp <- ifelse(mm$doinglwa %in% c(1,2,4),1,0)
ds$emp <- ifelse(ds$doinglwa %in% c(1,2,4),1,0)

# Retired
mm$retired <- ifelse(mm$whynowka==3,1,0)
mm$retired[is.na(mm$retired)] <- 0
ds$retired <- ifelse(ds$whynowka==3,1,0)
ds$retired[is.na(ds$retired)] <- 0

# Disabled
mm$disabled <- ifelse(mm$whynowka %in% c(6,9),1,0)
mm$disabled[is.na(mm$disabled)] <- 0
ds$disabled <- ifelse(ds$whynowka %in% c(6,9),1,0)
ds$disabled[is.na(ds$disabled)] <- 0

# Student 
mm$student <- ifelse(mm$whynowka==2,1,0)
mm$student[is.na(mm$student)] <- 0
ds$student <- ifelse(ds$whynowka==2,1,0)
ds$student[is.na(ds$student)] <- 0

# Home Engineer
mm$homee <- ifelse(mm$whynowka==1,1,0)
mm$homee[is.na(mm$homee)] <- 0
ds$homee <- ifelse(ds$whynowka==1,1,0)
ds$homee[is.na(ds$homee)] <- 0

# Government Employee!
mm$gee <- ifelse(mm$wrkcata %in% 2:4,1,0)
ds$gee <- ifelse(ds$wrkcata %in% 2:4,1,0)

# Paid Sick Leave
mm$pdsick <- ifelse(mm$pdsicka==1,1,0)
mm$pdsick[is.na(mm$pdsick)] <- 0
ds$pdsick <- ifelse(ds$pdsicka==1,1,0)
ds$pdsick[is.na(ds$pdsick)] <- 0

# Occupation?
#------------------------------------------------------------------------------
# Comorbidities
vars.como <- names(mm)[grep("(ev)$",names(mm))]
vars.como <- c(vars.como,"dibpre1","ahayfyr","sinyr","cbrchyr","kidwkyr","livyr"
				,"jntsymp","jntchr"
				,"arth1",names(mm)[grep("^(pain)",names(mm))],"amigr","acold2w","aintil2w"
				,"pregnow","avision","lupprt","speceq","alcndrt","alchronr"
				,"ahep","ahepliv","travel")
f.yesno <- function(x){
	.var <- ifelse(x==1,1,0)
	.var[is.na(.var)] <- 0
	.var
}					
mm[,vars.como] <- apply(mm[,vars.como],2,f.yesno)
ds[,vars.como] <- apply(ds[,vars.como],2,f.yesno)
#------------------------------------------------------------------------------
# Mental Health
vars.mh <- c("sad","nervous","restless","hopeless","effort","worthls")
f.mh <- function(x){
	factor(ifelse(x %in% 1:2,"Severe",ifelse(x %in% 3:4,"Moderate"
			,ifelse(x==5,"None","Unknown"))))
}	
mm[,vars.mh] <- apply(mm[,vars.mh],2,f.mh)
f.mh2 <- function(x){
  ifelse(x %in% 1:2,1
         ,ifelse(x %in% 3:4,2
                 ,ifelse(x==5,3,4)))
}	
ds[,vars.mh] <- apply(ds[,vars.mh],2,f.mh2)

#------------------------------------------------------------------------------
# Perceived Health
vars.self <- c("ahstatyr",names(mm)[grep("^(fl)",names(mm))])
f.self <- function(x){
	factor(ifelse(x %in% c(0,6),"None",ifelse(x %in% 1:2,"Moderate"
			,ifelse(x %in% 3:4,"Severe","Unknown"))))
}
mm$ahstatyr <- cut2(mm$ahstatyr,cuts=c(1,2,3,7))
levels(mm$ahstatyr) <- c("Better","Worse","Same","Unknown")
mm[,vars.self[-1]] <- apply(mm[,vars.self[-1]],2,f.self)
f.self2 <- function(x){
  ifelse(x %in% c(0,6),0
         ,ifelse(x %in% 1:2,1
                 ,ifelse(x %in% 3:4,2,3)))
}
ds[,vars.self[-1]] <- apply(ds[,vars.self[-1]],2,f.self2)
#------------------------------------------------------------------------------
# Smoking
mm$smkstat2 <- cut2(mm$smkstat2,cuts=c(3,4,5))
levels(mm$smkstat2) <- c("Current","Former","Never","Unknown")
ds$smkstat2 <- ifelse(ds$smkstat2==4,0
                      ,ifelse(ds$smkstat2 %in% c(1:2,5),1,2))
                              
#------------------------------------------------------------------------------
# Active
mm$active <- factor(ifelse((mm$strngno>0 & mm$strngno<996) | (mm$vigno>0 & mm$vigno<996),"High"
					,ifelse(mm$modno>0 & mm$modno<996,"Moderate"
					,ifelse(mm$modno==996,"Unable"
					,ifelse(mm$modno==0,"Never","Unknown")))))
ds$active <- ifelse((ds$strngno>0 & ds$strngno<996) | (ds$vigno>0 & ds$vigno<996),3
                           ,ifelse(ds$modno>0 & ds$modno<996,2
                                   ,ifelse(ds$modno==996,1
                                           ,ifelse(ds$modno==0,0,0))))

#------------------------------------------------------------------------------
# Home Health
mm$ahchyr <- ifelse(mm$ahchyr==1,1,0)
ds$ahchyr <- ifelse(ds$ahchyr==1,1,0)
# Surgery
mm$asrgyr <- ifelse(mm$asrgyr==1,1,0)
ds$asrgyr <- ifelse(ds$asrgyr==1,1,0)

# Regular Doctor
mm$ausualpl <- ifelse(mm$ausualpl %in% c(1,3),1,0)
ds$ausualpl <- ifelse(ds$ausualpl %in% c(1,3),1,0)

# Emergency Room
mm$ahernoy2 <- ifelse(mm$ahernoy2 %in% 0:8,mm$ahernoy2,NA)
ds$ahernoy2 <- ifelse(ds$ahernoy2 %in% 0:8,mm$ahernoy2,NA)
#------------------------------------------------------------------------------
# AUDIT-C
#------------------------------------------------------------------------------
vars.auditc <- c("alc12mtp","alc12myr","alcamt","alc5upno")
# Q1: How often did you have a drink containing alcohol in the past year? 
# 0-4
# Answer  Points
# Never  0
# Monthly or less  1
# Two to four times a month  2
# Two to three times a week	3
# Four or more times a week	4
# xtabs(~alc12mtp+alc12myr,mm)
# xtabs(~alc12mtp+alc12mwk,mm)
# xtabs(~alc12myr+alc12mwk,mm
#       ,subset=alc12myr<=12)
# xtabs(~alc12myr+alc12mwk,mm
#       ,subset=alc12mwk==7)
# summary(mm$alc12myr)
q1 <- ifelse(mm$alc12mtp==0,0
             ,ifelse(mm$alc12myr %in% 1:23,1
                     ,ifelse(mm$alc12myr %in% 24:103,2
                             ,ifelse(mm$alc12myr %in% 104:207,3
                                     ,ifelse(mm$alc12myr %in% 208:365
                                             ,4,NA)))))
# summary(factor(q1))

# Q2: How many drinks did you have on a typical day when you were drinking in the past year?
# Answer  Points
# None, I do not drink	0
# 1 or 2	0
# 3 or 4	1
# 5 or 6	2
# 7 to 9	3
# 10 or more	4
q2 <- ifelse(mm$alcamt %in% 0:2,0
             ,ifelse(mm$alcamt %in% 3:4,1
                     ,ifelse(mm$alcamt %in% 5:6,2
                             ,ifelse(mm$alcamt %in% 7:9,3
                                     ,ifelse(mm$alcamt %in% 10:95,4
                                             ,NA)))))
# summary(factor(q2))
# Q3: How often did you have six or more drinks on one occasion in the past year?
# Answer  Points
# Never	0
# Less than monthly	1
# Monthly	2
# Weekly	3
# Daily or almost daily	4
q3 <- ifelse(mm$alc5upno==0,0
             ,ifelse(mm$alc5upno %in% 1:11,1
                     ,ifelse(mm$alc5upno %in% 12:51,2
                             ,ifelse(mm$alc5upno %in% 52:364,3
                                     ,ifelse(mm$alc5upno==365,4
                                             ,NA)))))
# summary(factor(q3))
audit.c <- data.frame(cbind(q1,q2,q3))
audit.c$audit.c <- rowSums(audit.c[,1:3],na.rm=TRUE)
# summary(factor(audit.c$audit.c))
w.na <- apply(audit.c[,1:3],1,function(x){
  sum(is.na(x))
})
# summary(w.na)
audit.c$audit.c[w.na==3] <-99
# summary(factor(audit.c$audit.c))
mm$audit.c <- audit.c$audit.c
mm$audit.3 <- ifelse(mm$audit.c==0,0
                     ,ifelse(mm$audit.c %in% 1:3,1
                             ,ifelse(mm$audit.c %in% 4:12,2
                                     ,3)))
mm$audit.3 <- ifelse(mm$female==1 & mm$audit.c==3
                     ,2,mm$audit.3) 

ds$audit.c <- mm$audit.c
ds$audit.3 <- mm$audit.3
# summary(factor(mm$audit.3))
mm$audit.3 <- factor(mm$audit.3,levels=0:3
                     ,labels=c("None","Moderate","Heavy","Missing"))
# mm$audit.3 <- relevel(mm$audit.3,"Moderate")
# summary(mm$audit.3)
#------------------------------------------------------------------------------
# Alcohol status
# table(mm$alcstat)
mm$f.alcstat <- factor(mm$alcstat,levels=1:10
                     ,labels=c("Lifetime Abstainer","Former Infrequent"
                               ,"Former Regular","Former UK"
                               ,"Current Infreq","Current Light"
                               ,"Current Moderate","Current Heavier"
                               ,"Current UK","UK"))
# mm$alcstat <- relevel(mm$alcstat,"Current Light")
table(mm$audit.3,mm$alcstat)
# Collapse Unknown Alcohol Levels
mm$f.alcstat2 <- ifelse(mm$alcstat %in% c(4,9,10),4,mm$alcstat)
ds$f.alcstat2 <- mm$f.alcstat2
table(mm$f.alcstat2,mm$f.alcstat)
mm$f.alcstat2 <- factor(mm$f.alcstat2,levels=1:8
						,labels=c("Lifetime Abstainer","Former Infrequent"
                               ,"Former Regular","UK"
                               ,"Current Infreq","Current Light"
                               ,"Current Moderate","Current Heavier"))

#------------------------------------------------------------------------------
# Other Alcohol
vars.alc <- c("alc1yr","alclife","alc12mno","alc12mwk","alc5uptp","alc5upyr")
# apply(mm[,vars.alc],2,function(x) summary(factor(x)))

#------------------------------------------------------------------------------
# Recode refused and unknown
#------------------------------------------------------------------------------
mm$dibage[mm$dibage>96] <- NA
mm$smkreg[mm$smkreg>96] <- NA

mm$wkdayr[mm$wkdayr>996] <- NA
mm$beddayr[mm$beddayr>996] <- NA
mm$bmi[mm$bmi==999900] <- NA
mm$bmi <- mm$bmi/1000
mm$canage[is.na(mm$canage)] <- 0
mm$dibage[is.na(mm$dibage)] <- 0
mm$smkreg[is.na(mm$smkreg)] <- 0
mm$wkdayr[is.na(mm$wkdayr)] <- 0
mm$beddayr[is.na(mm$beddayr)] <- 0
mm$bmi[is.na(mm$bmi)] <- mean(mm$bmi,na.rm=TRUE)
mm$ahernoy2[is.na(mm$ahernoy2)] <- 0

ds$dibage[ds$dibage>96] <- NA
ds$smkreg[ds$smkreg>96] <- NA

ds$wkdayr[ds$wkdayr>996] <- NA
ds$beddayr[ds$beddayr>996] <- NA
ds$bmi[ds$bmi==999900] <- NA
ds$bmi <- ds$bmi/1000

ds$canage[is.na(ds$canage)] <- 0
ds$dibage[is.na(ds$dibage)] <- 0
ds$smkreg[is.na(ds$smkreg)] <- 0
ds$wkdayr[is.na(ds$wkdayr)] <- 0
ds$beddayr[is.na(ds$beddayr)] <- 0
ds$bmi[is.na(ds$bmi)] <- mean(ds$bmi,na.rm=TRUE)
ds$ahernoy2[is.na(ds$ahernoy2)] <- 0
#------------------------------------------------------------------------------
# Continuous variables for scaling
#------------------------------------------------------------------------------
apply(mm,2,function(x) summary(factor(x)))
# Remove rectype, vars.structure, 
cvars <- c("age_p","canage","dibage","wkdayr","beddayr","smkreg","bmi")

#------------------------------------------------------------------------------
# Redundant and Spurious Features
#------------------------------------------------------------------------------
features.drop <- c(vars.structure,"rectype","sex","srvy_yr","intv_qrt","hispan_i"
						,"racerpi2","mracrpi2","mracbpi2","r_maritl"
						,names(mm)[grep("^(prox)",names(mm))]
						,vars.cage,"doinglwa","doinglwp","whynowka","everwrk"
						,"indstrn1","indstrn2","occupn1","occupn2","businc1a","yrswrkpa"
						,"wrklongh","hourpda","pdsicka","onejob","locall1a","wrkcata"
						,names(mm)[grep("^(jmthp)",names(mm))],"jnthp","arthlmt"
						,"hraidnow","ahearst1","ablind","hypdifv","aasstill"
						,"aasmyr","ulcyr",names(mm)[grep("^(cnkind)",names(mm))]
						,"dibpre1","difage2","insln","dibpill"
						,"mhamtmo","alcndrt","sleep","ahcplrou"
						,names(mm)[grep("^(aflhc)",names(mm))]
						,names(mm)[grep("^(altime)",names(mm))]
						,names(mm)[grep("^(aldur)",names(mm))]
						,names(mm)[grep("^(alchr)",names(mm))]
						,names(mm)[grep("^(alunit)",names(mm))]
						,"smkev","smknow","smkqtno","smkqty","smkqttp"
						,names(mm)[grep("cig",names(mm))]
						,names(mm)[grep("^(strng)",names(mm))],"strfreqw"
						,names(mm)[grep("^(vig)",names(mm))]
						,names(mm)[grep("^(mod)",names(mm))]
						,vars.alc,vars.auditc,"aheight","aweightp","aplkind","ahcplknd"
						,"ahcchgyr","ahcchgyr","ahcchghi"
						,names(mm)[grep("^(ahcdlyr)",names(mm))]
						,names(mm)[grep("^(ahc[as]f?yr)",names(mm))]
						,names(mm)[grep("^(adnlong)",names(mm))]
						,names(mm)[grep("^(ahch?[nm])",names(mm))]
						,"asrgnoyr","amdlongr","shtpnuyr","apox","apox12mo"
						,"shthepb","shepdos","shthepa","shepanum","shingles"
						,"shttd","shttd05","wrkdir","year")

# Save to local disk
# save.image("~/Documents/DATA/statr2_fp.rdata")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Overview and Tables
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Cleaned data
# See fp_shape_data.R for details
rm(list=ls())
# load("/Users/adam/Documents/DATA/statr2_fp.rdata")
load("/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp.rdata")
gc()
# Sys.setenv(PATH=paste(Sys.getenv("PATH"),"/usr/texbin",sep=":"))
#-------------------------------------------------------------------------------------------
# Packages for Overview
require(survey)
require(Hmisc)
require(vcd)
require(psych)
#------------------------------------------------------------------------------------------
# Functions
# Case by case missing weighted mean function
f.svymean <- function(x){
	s.t <- NULL
	ff <- NULL
	s <- NULL
	for(n in x){
		ff <- as.formula(paste("~",n))
		s <- svymean(ff,s.ds,na.rm=TRUE)
		s.t <- rbind(s.t,ftable(s))
	}
	s.t <- round(as.data.frame(s.t),3)
	names(s.t) <- c("Mean","SE")
	s.t
}
f.svyatable <- function(x){
	s.t <- NULL
	ff <- NULL
	s <- NULL
	for(n in x){
		ff <- as.formula(paste("~",n,"+audit.3"))
		s <- ftable(svytable(ff,s.ds2,Ntotal=1))
		s.t <- rbind(s.t,s)
	}
	s.t <- round(as.data.frame(s.t),3)
	colnames(s.t) <- levels(ds.train$audit.3)
	s.t
}

f.svy.scale <- function(x){
	mu.m <- ftable(svymean(~x,s.ds,na.rm=TRUE))[1]
	mu.sd <- ftable(svymean(~x,s.ds,na.rm=TRUE))[2]
	(x-mu.m)/mu.sd
}
#------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
ds1 <- mm[mm$year==2012,]
dim(ds1)
#-------------------------------------------------------------------------------------------
# Split data into Training and Test by household 
set.seed(201403)
split.id <- unique(ds1$hhx)
length(split.id)
split.id <- sample(split.id,length(split.id))

ds.train <- ds1[ds1$hhx %in% split.id[1:floor(2*length(split.id)/3)]
                ,-1*which(names(ds1) %in% features.drop[c(-4,-7,-8)])]
dim(ds.train)
names(ds.train)
ds.test <- ds1[ds1$hhx %in% split.id[ceiling(2*length(split.id)/3):length(split.id)],-1*which(names(ds1) %in% features.drop[c(-4,-7,-8)])]
dim(ds.test)
names(ds.train)

s.ds <- svydesign(id=~psu_p, strata=~strat_p,nest = TRUE,weights=~wtfa_sa,data=ds.train)

#-------------------------------------------------------------------------------------------
# Table 1
length(names(ds))
# region+age_p+hypev+chdev+angev+miev+hrtev+strev+ephev+aasmev+ulcev+canev+dibev+dibage+ahayfyr+sinyr+cbrchyr+kidwkyr+livyr+jntsymp+jntchr+arth+paineck+painlb+painleg+painface+amigr+acoldw+aintilw+pregnow+hraidev+avision+lupprt+sad+nervous+restless+hopeless+effort+worthls+wkdayr+beddayr+ahstatyr+speceq+flwalk+flclimb+flstand+flsit+flstoop+flreach+flgrasp+flcarry+flpush+flshop+flsocl+flrelax+flaar+smkreg+smkstat+alcstat+bmi+ausualpl+ahcplrou+ahernoy+ahchyr+asrgyr+ahep+ahepliv+livev+travel+id+sw+female+svy.time+hisp+race+agecat+marital+canage+emp+retired+disabled+student+homee+pdsick+active+audit.c+audit.+gee+

# Demographic Variables
dem <- c("age_p","female","race","hisp","marital","bmi","emp","region")
(t.dem <- f.svymean(dem))
(t.adem <- f.svyatable(dem[c(-1,-6)]))
rownames(t.adem) <- c("Male","Female",levels(ds.train$race),"NotHispanic","Hispanic",levels(ds.train$marital),"Unemployed","Employed",levels(ds.train$region))
# write.csv(t.adem,"/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp_table1.csv")

# Comorbidities
como <- c("hypev","chdev","angev","miev","hrtev","strev","ephev","aasmev","ulcev","canev","dibev","ahayfyr","sinyr","cbrchyr","kidwkyr","livyr","jntsymp","jntchr","arth1","paineck","painlb","painleg","painface","amigr","acold2w","aintil2w","pregnow","hraidev","avision","lupprt","speceq","ahep")
(t.como <- f.svymean(como))
cor.como <- cor(ds.train[,como])
w.c <- apply(cor.como,1,function(x) ifelse(any(x>.9 &x<1),1,0))
cor.como[w.c==1,]
(t.acomo <- f.svyatable(como[1:11]))
rownames(t.acomo) <- c("HypN","HypY","CHDN","CHDY","AngN","AngY","MIN","MIY","HrtN","HrtY","StrN","StrY","EphN","EphY","AasN","AasY","UlcN","UlcY","CanN","CanY","DibN","DibY")
# write.csv(t.adem,"/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp_table2.csv")

# Age of Dx cancer or diabetes
(t.cage <- f.svymean(c("dibage","canage")))
pairs.panels(ds.train[,c("dibage","canage")])
# Mental Health
(t.mh <- f.svymean(c("sad","nervous","restless","hopeless","effort","worthls"))

# Difficulty 
(t.fl <- f.svymean(c("flwalk","flclimb","flstand","flsit","flstoop","flreach","flgrasp","flcarry","flpush","flshop","flsocl","flrelax","fla1ar")))

# SES
(t.ses <- f.svymean(c("disabled","retired","student","gee","homee","pdsick","travel")))

# Health
(t.health <- f.svymean(c("ahstatyr","ausualpl","ahchyr","asrgyr","active")))

# Health Care Util
(t.huse <- f.svymean(c("wkdayr","beddayr","ahernoy2")))

# Smoke
(t.smoke <- f.svymean(c("smkreg","smkstat2")))

# Alcohol
(t.malcohol <- f.svymean(c("f.alcstat2","audit.3")))
round(svytable(~f.alcstat2+audit.3,s.ds,Ntotal=1),3)

mosaic(svytable(~alcstat+audit.3,design=s.ds,round=TRUE),shade=TRUE)
svyplot(bmi~age_p, design=s.ds, style="trans",basecol=rainbow(20,start=.6)
			,ylab="BMI",xlab="AGE")
lines(svysmooth(bmi~age_p, design=s.ds))

svyplot(bmi~canage, design=s.ds, style="trans",basecol=rainbow(20,start=.6))
svyplot(bmi~smkreg, design=s.ds, style="trans",basecol=rainbow(20,start=.6))
svyplot(canage~dibage, design=s.ds, style="bubble",basecol=rainbow(20,start=.6))
lines(svysmooth(canage~dibage, design=s.ds),col="red")
svyplot(canage~smkreg, design=s.ds, style="bubble",basecol=rainbow(20,start=.6))
lines(svysmooth(canage~smkreg, design=s.ds),col="red")


svyhist(~bmi,s.ds,col="mistyrose4",main="NHIS BMI",xlab="")
lines(svysmooth(~bmi, design=s.ds),col="mistyrose3")
# svyboxplot(bmi~alcstat,s.ds)
svyhist(~age_p,s.ds,col=rgb(1,0,0,.5),main="",xlab="",ylim=c(0,.03))
svyhist(~canage,s.ds,col=rgb(0,1,0,.5),add=TRUE)
svyhist(~dibage,s.ds,col=rgb(0,0,1,.5),add=TRUE)

# cvars <- c("age_p","canage","dibage","wkdayr","beddayr","smkreg","bmi","ahernoy2")
round(svytable(~female+f.alcstat2,s.ds,Ntotal=1),3)

# -----------------------------------------------------------------------------------------------------
# Test set
s.ds2 <- svydesign(id=~psu_p, strata=~strat_p,nest = TRUE,weights=~wtfa_sa,data=ds.test)
(t.adem <- f.svyatable(dem[c(-1,-6)]))
rownames(t.adem) <- c("Male","Female",levels(ds.test$race),"NotHispanic","Hispanic",levels(ds.test$marital),"Unemployed","Employed",levels(ds.test$region))
# write.csv(t.adem,"/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp_table3.csv")
como <- c("hypev","chdev","angev","miev","hrtev","strev","ephev","aasmev","ulcev","canev","dibev","ahayfyr","sinyr","cbrchyr","kidwkyr","livyr","jntsymp","jntchr","arth1","paineck","painlb","painleg","painface","amigr","acold2w","aintil2w","pregnow","hraidev","avision","lupprt","speceq","ahep")
png("/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp_plot_pairs.png")
require(RColorBrewer)
png("/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp_plot_trainheat.png")
par(mar=c(2,2,2,0))
heatmap(ds.train,col=brewer.pal(12,'Set3'),labRow=NA)
dev.off()		

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Models
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
rm(list=ls())
gc()
load("/Users/adam/Documents/DATA/statr2_fp.rdata")
#-------------------------------------------------------------------------------------------
# Packages for Models
require(Hmisc)
require(glmnet)
require(psych)
require(RSNNS) 
require(doMC)
require(lattice)
require(xtable)
registerDoMC(cores=3)
source("/Users/adam/Dropbox/STATR2/Work/featureFiltering1.r")
source("/Users/adam/Dropbox/STATR2/Work/plotNNet.r")
#-------------------------------------------------------------------------------------------
# Data
#-------------------------------------------------------------------------------------------
ds1 <- ds[ds$year==2012,names(ds) %in% names(mm)]
dim(ds1)
#-------------------------------------------------------------------------------------------
# Functions
#0-1 Loss
f.err <- function(x){
	mean(apply(x,1,function(xx) (which.max(xx)-1)==ds.test$audit.3))
}
# Cross-Entropy
f.errx <- function(x) {
	-1*sum(apply(x,1,function(xx) sum(xx*log(xx))))
}
#-------------------------------------------------------------------------------------------
# Data prep
#-------------------------------------------------------------------------------------------
# Split data into Training and Test by household 
set.seed(201403)
split.id <- unique(ds1$hhx)
length(split.id)
split.id <- sample(split.id,length(split.id))

ds.train <- ds1[ds1$hhx %in% split.id[1:floor(2*length(split.id)/3)],]

cv.groups <- sample(unique(ds.train$hhx)
                    ,length(unique(ds.train$hhx)))
cv.groups <- cbind(cv.groups,rep(1:10,length(cv.groups)))
cvid <- ifelse(ds.train$hhx %in% cv.groups[,1],cv.groups[,2],NA)
cvid <- as.numeric(cvid)
table(cvid);length(cvid)

ds.train <- ds1[ds1$hhx %in% split.id[1:floor(2*length(split.id)/3)]
                ,-1*which(names(ds1) %in% features.drop[-4])]

dim(ds.train)
ds.test <- ds1[ds1$hhx %in% split.id[ceiling(2*length(split.id)/3):length(split.id)],-1*which(names(ds1) %in% features.drop[-4])]
dim(ds.test)
#---------------------------------------------------------------------------------------
# Lasso prep
#----------------------------------------------------------------------------------------
names(ds.train)
names(ds.train)[-1*c(-1,-60,-70,-71,-84:-86)]
summary(factor(ds.train$audit.3))
m1 <- as.matrix(ds.train[,c(-1,-60,-70,-71,-84:-86)])
m1 <- apply(m1,2,as.numeric)
apply(m1,2,summary)
m2 <- as.matrix(ds.test[,c(-1,-60,-70,-71,-84:-86)])
m2 <- apply(m2,2,as.numeric)
apply(m2,2,summary)

cv.ml <- cv.glmnet(x=m1,y=ds.train$audit.3,foldid=cvid
                   ,weights=ds.train$wtfa_sa
                   ,family="multinomial"
                   ,type.measure="class"
                   ,parallel=TRUE)
(cv.ml$cvm[which.min(cv.ml$lambda)])
(cv.ml$cvm[which(cv.ml$lambda==cv.ml$lambda.1se)])
#---------------------------------------------------------------------------------------
# lasso cv errors and confusion matrix
#----------------------------------------------------------------------------------------
ptest.ml <- predict(cv.ml,m1,s=min(cv.ml$lambda),type="class",weights=ds.train$wtfa_sa)
(t.ml <- table(AuditC=ds.train$audit.3,Predicted=ptest.ml))
write.csv(t.ml,"/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp_cmlasso.csv")
xtable(t.ml)
mean(ptest.ml==ds.train$audit.3)

allerr <- NULL
for(i in 1:length(cv.ml$lambda)){
	allerr <- c(allerr,f.errx(predict(cv.ml,m1,s=cv.ml$lambda[i],type="response",weights=ds.train$wtfa_sa)))
}

plot(allerr~cv.ml$lambda,main="Lasso",xlab="lambda",ylab="Cross-Entropy Error")

ptest.ml <- predict(cv.ml,m1,s=min(cv.ml$lambda),type="response",weights=ds.train$wtfa_sa)
f.errx(ptest.ml)
#---------------------------------------------------------------------------------------
# Lasso final prediction and confusion matrix
#----------------------------------------------------------------------------------------
ptest.ml <- predict(cv.ml,m2,s=min(cv.ml$lambda),type="class",weights=ds.test$wtfa_sa)
(t.ml <- table(AuditC=ds.train$audit.3,Predicted=ptest.ml))
write.csv(t.ml,"/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp_cmflasso2.csv")
f.errx(predict(cv.ml,m2,s=min(cv.ml$lambda),type="response",weights=ds.test$wtfa_sa))
mean(ptest.ml==ds.test$audit.3)
#---------------------------------------------------------------------------------------
# Nnet 
#----------------------------------------------------------------------------------------
ds.train.class <- decodeClassLabels(ds.train$audit.3)
ds.test.class <- decodeClassLabels(ds.test$audit.3)

nhisCombo=normTrainingAndTestSet(list(
  inputsTrain=ds.train[,c(-1,-60,-70,-71,-84:-86)]
  ,inputsTest=ds.test[,c(-1,-60,-70,-71,-84:-86)]
  ,targetsTrain=ds.train.class
  ,targetsTest=ds.test.class))

# my1st1layer=mlp(nhisCombo$inputsTrain,nhisCombo$targetsTrain
                # ,size=c(6,3),learnFuncParams=c(0.2,0.1))
# onelayerpred=predict(my1st1layer,nhisCombo$inputsTest)
# RSNNS::confusionMatrix(ds.test.class,onelayerpred)
# mean(apply(onelayerpred,1,function(x) which.max(x)-1)==ds.test$audit.3)
#---------------------------------------------------------------------------------------
# Nnet tuning
#----------------------------------------------------------------------------------------
# Tuning Grid
lgrid <-  expand.grid(c(10,15,20),c(3,5,8),c(0.01,.1)) 
colnames(lgrid)  <- c("h1","h2","r") 
rownames(lgrid) <- paste("nnet-"
						,apply(lgrid,1,function(x) {
							paste(x,sep="", collapse="-")
							})
					, sep="") 
lgrid
nn.pred <- foreach(i=1:nrow(lgrid),.packages="RSNNS") %dopar% {
  model <- mlp(nhisCombo$inputsTrain, nhisCombo$targetsTrain
               , size=c(lgrid$h1[i],lgrid$h2[i])
               , learnFunc="Std_Backpropagation"
               ,learnFuncParams=c(lgrid$r[i], 1)
               , maxit=200
               ,inputsTest=nhisCombo$inputsTest
               ,targetsTest=nhisCombo$targetsTest)
  # s.model <- summary(model)
  err <- f.errx(fitted.values(model))
  return(err)
}
length(nn.pred)
(t.err <- cbind(lgrid,err=unlist(nn.pred)))
write.csv(t.err,"/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp_cmnnet.csv")
#---------------------------------------------------------------------------------------
# Nnet CV Confusion matrix
#----------------------------------------------------------------------------------------
pred.nn2 <- predict(model,nhisCombo$inputsTrain)                
t.nerrc <- RSNNS::confusionMatrix(pred.nn2,ds.train.class)                     
write.csv(t.nerrc,"/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp_ctable_nnetc.csv")
xtable(t.nerrc)
#---------------------------------------------------------------------------------------
# Final NNet pred
#----------------------------------------------------------------------------------------
min.err <- t.err[which.min(t.err$err),]
model <- mlp(nhisCombo$inputsTrain, nhisCombo$targetsTrain
               , size=c(min.err[,"h1"],min.err[,"h2"])
               , learnFunc="Std_Backpropagation"
               ,learnFuncParams=c(min.err[,"r"])
               , maxit=200
               ,inputsTest=nhisCombo$inputsTest
               ,targetsTest=nhisCombo$targetsTest)
pred.nn <- predict(model,nhisCombo$inputsTest)                
t.nerrf <- RSNNS::confusionMatrix(pred.nn,ds.test.class)               
write.csv(t.nerrf,"/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp_ctable_nnetf.csv")
xtable(t.nerrf)
f.errx(pred.nn)
f.err(pred.nn)
par(mar=c(0,0,0,0))
png("/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp_plot_nnetf.png",width=800,height=800)
plot.nnet(model,x.lab=names(ds.test)[c(-1,-60,-70,-71,-84:-86)],y.lab=levels(ds.test$audit.3),cex=0.8)
dev.off()
#---------------------------------------------------------------------------------------
# Weight Matrices 
#----------------------------------------------------------------------------------------
# Layer 1
f.weights <- weightMatrix(model)[1:79,80:89]
rownames(f.weights) <- names(ds.test)[c(-1,-60,-70,-71,-84:-86)]
f.weights
h2 <- apply(f.weights,1,function(x) ifelse(x==max(x),1,0))
write.csv(h2,"/Users/adam/Dropbox/STATR2/FinalProject/statr2_fp_thidden1.csv")
l.weights <- apply(h2,1,function(x) colnames(h2)[x==1])
layer2 <- NULL
for(i in 1:length(l.weights)){
	layer2 <- rbind(layer2,cbind(layer=names(l.weights)[i],features=paste(l.weights[[i]],sep=",")))
}
layer2
ag.layer2 <- aggregate(features~layer,layer2,paste)
xtable(ag.layer2)

# Layer 2
(f.weights <- weightMatrix(model)[80:89,c(-1:-89,-98:-101)])
rownames(f.weights) <- paste0("H",1:10)
colnames(f.weights) <- paste0("HH",1:8)
f.weights
xtable(f.weights)

# End nodes
(f.weights <- weightMatrix(model)[90:97,98:101])
rownames(f.weights) <- paste0("HH",1:8)
colnames(f.weights) <- c("None","Moderate","Heavy","Missing")
f.weights
xtable(f.weights)

	