# Cleaned data
# See fp_shape_data.R for details
rm(list=ls())
load("auditc.rdata")
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

# Comorbidities
como <- c("hypev","chdev","angev","miev","hrtev","strev","ephev","aasmev","ulcev","canev","dibev","ahayfyr","sinyr","cbrchyr","kidwkyr","livyr","jntsymp","jntchr","arth1","paineck","painlb","painleg","painface","amigr","acold2w","aintil2w","pregnow","hraidev","avision","lupprt","speceq","ahep")
(t.como <- f.svymean(como))
cor.como <- cor(ds.train[,como])
w.c <- apply(cor.como,1,function(x) ifelse(any(x>.9 &x<1),1,0))
cor.como[w.c==1,]
(t.acomo <- f.svyatable(como[1:11]))
rownames(t.acomo) <- c("HypN","HypY","CHDN","CHDY","AngN","AngY","MIN","MIY","HrtN","HrtY","StrN","StrY","EphN","EphY","AasN","AasY","UlcN","UlcY","CanN","CanY","DibN","DibY")

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
como <- c("hypev","chdev","angev","miev","hrtev","strev","ephev","aasmev","ulcev","canev","dibev","ahayfyr","sinyr","cbrchyr","kidwkyr","livyr","jntsymp","jntchr","arth1","paineck","painlb","painleg","painface","amigr","acold2w","aintil2w","pregnow","hraidev","avision","lupprt","speceq","ahep")
png("fp_plot_pairs.png")
require(RColorBrewer)
png("fp_plot_trainheat.png")
par(mar=c(2,2,2,0))
heatmap(ds.train,col=brewer.pal(12,'Set3'),labRow=NA)
dev.off()
