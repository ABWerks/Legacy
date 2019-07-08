# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ABWerks Legacy Project
# UW CEU Statistical Programming with R
# Spring 2014
# Script runs the models (Lasso + RSNNS) for the AUDIT-C predictions project
#
# Dependency: fp_shape_data.R
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
rm(list=ls())
gc()
load("auditc.rdata")
ls()
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
## source("../Functions/featureFiltering1.r")
## source("../Functions/plotNNet.r")
#-------------------------------------------------------------------------------------------
# Data
#-------------------------------------------------------------------------------------------
ds1 <- ds[ds$year==2012,names(ds) %in% names(mm)]
dim(ds1)
#-------------------------------------------------------------------------------------------
# Loss Functions
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

## Ensure households are not split across folds
ds.train <- ds1[ds1$hhx %in% split.id[1:floor(2*length(split.id)/3)],]
## Randomize the IDs
cv.groups <- sample(unique(ds.train$hhx)
                    ,length(unique(ds.train$hhx)))
## 10-fold balanced CV groups
cv.groups <- cbind(cv.groups,rep(1:10,length(cv.groups)))
cvid <- ifelse(ds.train$hhx %in% cv.groups[,1],cv.groups[,2],NA)
cvid <- as.numeric(cvid)
table(cvid);length(cvid)
## Train and test sets
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
## The target is AUDITC score
summary(factor(ds.train$audit.3))
## Predictors
## glmnet requires a matrix feature space
m1 <- as.matrix(ds.train[,c(-1,-60,-70,-71,-84:-86)])
m1 <- apply(m1,2,as.numeric)
apply(m1,2,summary)
m2 <- as.matrix(ds.test[,c(-1,-60,-70,-71,-84:-86)])
m2 <- apply(m2,2,as.numeric)
apply(m2,2,summary)
## Run the CV lasso
cv.ml <- cv.glmnet(x=m1,y=ds.train$audit.3,foldid=cvid
                   ,weights=ds.train$wtfa_sa
                   ,family="multinomial"
                   ,type.measure="class"
                   ,parallel=TRUE)
## Absolute minimum lambda
(cv.ml$cvm[which.min(cv.ml$lambda)])
## Parsimonious lambda 1 SD worse than the minimum
(cv.ml$cvm[which(cv.ml$lambda==cv.ml$lambda.1se)])
#---------------------------------------------------------------------------------------
# lasso cv errors and confusion matrix
#----------------------------------------------------------------------------------------
ptest.ml <- predict(cv.ml,m1,s=min(cv.ml$lambda),type="class",weights=ds.train$wtfa_sa)
(t.ml <- table(AuditC=ds.train$audit.3,Predicted=ptest.ml))
xtable(t.ml)
# Flat Error
mean(ptest.ml==ds.train$audit.3)
# Cross entropy error
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
(t.ml <- table(AuditC=ds.test$audit.3,Predicted=ptest.ml))
f.errx(predict(cv.ml,m2,s=min(cv.ml$lambda),type="response",weights=ds.test$wtfa_sa))
mean(ptest.ml==ds.test$audit.3)
#---------------------------------------------------------------------------------------
# RSNNS
#----------------------------------------------------------------------------------------
## Convert class labels to matrix of dummyvars
ds.train.class <- decodeClassLabels(ds.train$audit.3)
ds.test.class <- decodeClassLabels(ds.test$audit.3)
## Create the inputs for the RSNNS
nhisCombo=normTrainingAndTestSet(list(
  inputsTrain=ds.train[,c(-1,-60,-70,-71,-84:-86)]
  ,inputsTest=ds.test[,c(-1,-60,-70,-71,-84:-86)]
  ,targetsTrain=ds.train.class
  ,targetsTest=ds.test.class))
## Test run with mlp on 2 layers with 6 and 3 nodes
onelayer=mlp(nhisCombo$inputsTrain,nhisCombo$targetsTrain
                ,size=c(6,3),learnFuncParams=c(0.2,0.1))
onelayerpred=predict(onelayer,nhisCombo$inputsTest)
RSNNS::confusionMatrix(ds.test.class,onelayerpred)
## Flat error
mean(apply(onelayerpred,1,function(x) which.max(x)-1)==ds.test$audit.3)
#---------------------------------------------------------------------------------------
# RSNNS tuning
#----------------------------------------------------------------------------------------
# Tuning Grid
## Tune several mlp across a grid
lgrid <-  expand.grid(c(10,15,20),c(3,5,8),c(0.01,.1))
colnames(lgrid)  <- c("h1","h2","r")
rownames(lgrid) <- paste("nnet-"
						,apply(lgrid,1,function(x) {
							paste(x,sep="", collapse="-")
							})
					, sep="")
lgrid
## Run in mlp parallel across the grid, go take a nap
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
#---------------------------------------------------------------------------------------
# Nnet CV Confusion matrix
#----------------------------------------------------------------------------------------
pred.nn2 <- predict(model,nhisCombo$inputsTrain)
t.nerrc <- RSNNS::confusionMatrix(pred.nn2,ds.train.class)
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
xtable(t.nerrf)
f.errx(pred.nn)
f.err(pred.nn)
par(mar=c(0,0,0,0))
png("fp_plot_nnetf.png",width=800,height=800)
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
