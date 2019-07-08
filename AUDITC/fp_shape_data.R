#----------------------------------------------------------------------------------------------
# Original Data from "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/YYYY/samadult.sas"
# For YYYY %in% 2009:2012
#----------------------------------------------------------------------------------------------
# You'll need the foreign package or the read.SAScii package to read the data into R
# If you are really interested email me and I can send you a load script
#----------------------------------------------------------------------------------------------
# Documentation at: # ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/NHIS/2012/samadult_layout.pdf
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

rm(list=ls())
load("auditc.rdata")
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
