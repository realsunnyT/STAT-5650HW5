# Final Homework (HW5)

# # # # #
# # # # #
# # 1 # #
# # # # #
# # # # #

class.sum <- function(truth,predicted){
  xt=table(truth,round(predicted+0.000001))
  pcc=round(100*sum(diag(xt))/sum(xt),2)
  spec=round(100*xt[1,1]/sum(xt[1,]),2)
  sens=round(100*xt[2,2]/sum(xt[2,]),2)
  kap=round(kappa(xt)[1],4)
  au=round(roc.area(truth,predicted)$A,4)
  return(cbind(c("Percent Correctly Classified = ","Specificity = 
","Sensitivity = ","Kappa =","AUC= "),c(pcc,spec,sens,kap,au)))
}

kappa <- function(x){
  n=sum(x)
  pobs=(x[1,1]+x[2,2])/n
  pexp=(sum(x[1,])*sum(x[,1])+sum(x[2,])*sum(x[,2]))/n^2
  kappa=(pobs-pexp)/(1-pexp)
  t1=0
  t2=0
  t3=0
  pii=x/n
  pidot=apply(pii,1,sum)
  pdotj=apply(pii,2,sum)
  for(i in 1:2){
    t1 = t1 + pii[i,i]*((1-pexp) - (1-pobs)*(pidot[i]+pdotj[i]))^2
  }
  t2 = pii[1,2]*(pdotj[1]+pidot[2])^2 + pii[2,1]*(pdotj[2] + 
                                                    pidot[1])^2
  t3 = (pobs*pexp-2*pexp+pobs)^2
  vhat = (t1 + t2*(1-pobs)^2 -t3)/(n*(1-pexp)^4)
  se=sqrt(vhat)
  return(c(kappa,se))
}

library(randomForest)
library(verification)

glasses = read.csv("Glass copy.csv")

# # #
# a #
# # #

glass.rf=randomForest(as.factor(GlassType)~ . ,data=glasses)

glass.rf$confusion
glass.rf.confusion <- table(glasses$GlassType,predict(glass.rf,type="response"))
100*sum(diag(glass.rf.confusion))/sum(glass.rf.confusion)

# # #
# b #
# # #

glass.rf2=randomForest(as.factor(GlassType)~ . ,importance=TRUE,data=glasses)

varImpPlot(glass.rf2,scale=FALSE)

# model using the top 4
glass.rf2.top4=randomForest(as.factor(GlassType)~ Magnesium+Aluminum+Refindex+Calcium ,data=glasses)

glass.rf2.top4$confusion
glass.rf2.confusion=table(glasses$GlassType,predict(glass.rf2.top4,type="response"))
100*sum(diag(glass.rf2.confusion))/sum(glass.rf2.confusion)

# model using not bottom 2
glass.rf2.top5=randomForest(as.factor(GlassType)~ .-Iron-Silicon,data=glasses)

glass.rf2.top5$confusion
glass.rf2.confusion2=table(glasses$GlassType,predict(glass.rf2.top5,type="response"))
100*sum(diag(glass.rf2.confusion2))/sum(glass.rf2.confusion2)

# # # # #
# # # # #
# # 2 # #
# # # # #
# # # # #

orange = read.csv("neworangelabeled.csv")

# I quickly went through and dummy labeled the countries in excel.
# BEL = 0
# LSP = 1
# TME = 2
# VME = 3

# decision tree model
library(rpart)
# cp plot
orange.rpartfull=rpart(Country~ . ,method="class", 
                      control=rpart.control(cp=0.0,minsplit=2),data=orange)
plot(orange.rpartfull)
plotcp(orange.rpartfull)

# I will use a cp of 0.12 for this decision tree
orange.rpartcp12=rpart(Country~ . 
                       ,method="class",data=orange,control=rpart.control(cp=0.12))
plot(orange.rpartcp12,margin=0.1)
text(orange.rpartcp12,use.n=TRUE)
orange.rpartcp12
table(orange$Country,predict(orange.rpartcp12,type="class"))
xvs=rep(c(1:10),length=nrow(orange))
xvs=sample(xvs)
orange.rpartcp12.xval.predprob=rep(0,length(nrow(orange)))
orange.rpartcp12.xval.predclass=rep(0,length(nrow(orange)))
for(i in 1:10){
  train=orange[xvs!=i,]
  test=orange[xvs==i,]
  rp=rpart(Country~ .
           ,method="class",data=train,control=rpart.control(cp=0.12))
  orange.rpartcp12.xval.predprob[xvs==i]=predict(rp,test,type="prob")[,2]
  orange.rpartcp12.xval.predclass[xvs==i]=predict(rp,test,type="class")
}
oranget = table(orange$Country, round(orange.rpartcp12.xval.predclass))
oranget
100*sum(diag(oranget))/sum(oranget)

# random forest
orange.rf=randomForest(as.factor(Country)~ . ,data=orange)

orange.rf$confusion
orange.rf.confusion <- table(orange$Country,predict(orange.rf,type="response"))
100*sum(diag(orange.rf.confusion))/sum(orange.rf.confusion)

# cutting variables
orange.rf2=randomForest(as.factor(Country)~ . ,importance=TRUE,data=orange)

varImpPlot(orange.rf2,scale=FALSE)

# taking out bottom 3
orange.rf2.bot3=randomForest(as.factor(Country)~ .-Manganese-Zinc-Potassium,data=orange)

orange.rf2.bot3$confusion
orange.rf2.confusion2=table(orange$Country,predict(orange.rf2.bot3,type="response"))
100*sum(diag(orange.rf2.confusion2))/sum(orange.rf2.confusion2)

# taking out bottom 4
orange.rf2.bot4=randomForest(as.factor(Country)~ .-Boron-Manganese-Zinc-Potassium,data=orange)

orange.rf2.bot4$confusion
orange.rf2.confusion3=table(orange$Country,predict(orange.rf2.bot4,type="response"))
100*sum(diag(orange.rf2.confusion3))/sum(orange.rf2.confusion3)

# taking out bottom 7
orange.rf2.bot7=randomForest(as.factor(Country)~ .-Rubidium-Phosphorus-Magnesium-Boron-Manganese-Zinc-Potassium,
                             data=orange)

orange.rf2.bot7$confusion
orange.rf2.confusion4=table(orange$Country,predict(orange.rf2.bot7,type="response"))
100*sum(diag(orange.rf2.confusion4))/sum(orange.rf2.confusion4)

# # # # #
# # # # #
# # 3 # # 
# # # # #
# # # # #

mull = read.csv('mull2.csv')
val = read.csv('newvalid.csv')

# discarded LABEGRD_ID, aspd, UTMX, and UTMY within both datasets
# as they are not needed for this analysis.

# # #
# a #
# # #

# logistic regression baseline - no variable selection
mull.lr = glm(VETH~ . ,family=binomial,data=mull)

mull.lr.xval=rep(0,nrow(mull))
xvs=rep(1:10,length=nrow(mull))
xvs=sample(xvs)
for(i in 1:10){
  train=mull[xvs!=i,]
  test=mull[xvs==i,]
  glub=glm(VETH~ . ,family=binomial,data=train)
  mull.lr.xval[xvs==i]=predict(glub,test,type="response")
}
table(mull$VETH,round(mull.lr.xval))
class.sum(mull$VETH,mull.lr.xval)

val.lr = predict(mull.lr,val,type="response")
table(val$VETH,round(val.lr))
class.sum(val$VETH,val.lr)

# logistic regression baseline - with variable selection
mull.lr14 = step(mull.lr)

mull.lr14.xval=rep(0,nrow(mull))
xvs=rep(1:10,length=nrow(mull))
xvs=sample(xvs)
for(i in 1:10){
  train=mull[xvs!=i,]
  test=mull[xvs==i,]
  glub=step(glm(VETH~ . ,family=binomial,data=train))
  mull.lr14.xval[xvs==i]=predict(glub,test,type="response")
}
table(mull$VETH,round(mull.lr14.xval))
class.sum(mull$VETH,mull.lr14.xval)

val.lr14 = predict(mull.lr14,val,type="response")
table(val$VETH,round(val.lr14))
class.sum(val$VETH,val.lr14)

# # #
# b #
# # #

# classification tree
# cp plot
mull.rpartfull=rpart(VETH~ . ,method="class", 
                       control=rpart.control(cp=0.0,minsplit=2),data=mull)
plot(mull.rpartfull)
plotcp(mull.rpartfull)

# fitting cp 0.00028
mull.rpartcp028=rpart(VETH~ . 
                       ,method="class",data=mull,control=rpart.control(cp=0.00028))
plot(mull.rpartcp028,margin=0.1)

xvs=rep(c(1:10),length=nrow(mull))
xvs=sample(xvs)
mull.rpartcp028.xval=rep(0,length(nrow(mull)))
for(i in 1:10){
  train=mull[xvs!=i,]
  test=mull[xvs==i,]
  rp=rpart(VETH~ . 
           ,method="class",data=train,control=rpart.control(cp=0.00028))
  mull.rpartcp028.xval[xvs==i]=predict(rp,test,type="prob")[,2]
}
table(mull$VETH,round(mull.rpartcp028.xval))
class.sum(mull$VETH,mull.rpartcp028.xval)
table(val$VETH,predict(mull.rpartcp028,val,type="class"))
class.sum(val$VETH,predict(mull.rpartcp028,val,type="prob")[,2]
)

# random forest
# varimp plotting
mull.rf=randomForest(as.factor(VETH)~ . ,importance=TRUE,data=mull)
varImpPlot(mull.rf,scale=FALSE)

# modeling
# full rf
mull.fullrf=randomForest(as.factor(VETH)~ . ,data=mull)

mull.fullrf$confusion
mull.fullrf.confusion <- table(mull$VETH,predict(mull.fullrf,type="response"))
100*sum(diag(mull.fullrf.confusion))/sum(mull.fullrf.confusion)

# first model (bot9)
mull.rfbot9=randomForest(as.factor(VETH)~ .-vpsaa-vpdda-tmaxa-vpddd-vpsad-sfmma-ddeg-etpja-TransAspd,
                         data=mull)

mull.rfbot9$confusion
mull.rfbot9.confusion <- table(mull$VETH,predict(mull.rfbot9,type="response"))
100*sum(diag(mull.rfbot9.confusion))/sum(mull.rfbot9.confusion)

# second model (bot11)
mull.rfbot11=randomForest(as.factor(VETH)~ .-vpsaa-vpdda-tmaxa-vpddd-vpsad-sfmma-ddeg-etpja-TransAspd
                          -taved-tdayd,
                         data=mull)

mull.rfbot11$confusion
mull.rfbot11.confusion <- table(mull$VETH,predict(mull.rfbot11,type="response"))
100*sum(diag(mull.rfbot11.confusion))/sum(mull.rfbot11.confusion)

# CV and val testing
mull.rfbot11.xval.class=rep(0,length=nrow(mull))
mull.rfbot11.xval.prob=rep(0,length=nrow(mull))
xvs=rep(1:10,length=nrow(mull))
xvs=sample(xvs)
for(i in 1:10){
  train=mull[xvs!=i,]
  test=mull[xvs==i,]
  glub=randomForest(as.factor(VETH)~ .-vpsaa-vpdda-tmaxa-vpddd-vpsad-sfmma
                    -ddeg-etpja-TransAspd-taved-tdayd, 
                    data=train)
  mull.rfbot11.xval.class[xvs==i]=predict(glub,test,type="response")
  mull.rfbot11.xval.prob[xvs==i]=predict(glub,test,type="prob")[,2]
}

table(mull$VETH,mull.rfbot11.xval.class)
class.sum(mull$VETH,mull.rfbot11.xval.prob)

table(val$VETH,predict(mull.rfbot11,val,type="response"))
class.sum(val$VETH,predict(mull.rfbot11,val,type="prob")[,2])

# adaboost
library(ada)

mull.ada=ada(as.factor(VETH)~ .-vpsaa-vpdda-tmaxa-vpddd-vpsad-sfmma
             -ddeg-etpja-TransAspd-taved-tdayd 
             ,loss="exponential",data=mull)

mull.ada.xvalpr=rep(0,nrow(mull))
xvs=rep(1:10,length=nrow(mull))
xvs=sample(xvs)
for(i in 1:10){
  train=mull[xvs!=i,]
  test=mull[xvs==i,]
  glub=ada(as.factor(VETH)~ .-vpsaa-vpdda-tmaxa-vpddd-vpsad-sfmma
           -ddeg-etpja-TransAspd-taved-tdayd 
           ,loss="exponential",data=train)
  mull.ada.xvalpr[xvs==i]=predict(glub,newdata=test,type="prob")[,2]
}

table(mull$VETH,round(mull.ada.xvalpr))
class.sum(mull$VETH,mull.ada.xvalpr)

table(val$VETH,round(predict(mull.ada,newdata=val,type="prob")[,2]))
class.sum(val$VETH,predict(mull.ada,newdata=val,type="prob")[,2])


# gradient boosting machine
library(caret)
library(gbm)

fitControl = trainControl(method = "cv", number = 10)

# tuning
gbmGrid = expand.grid(interaction.depth = c(12, 14, 16, 18, 20, 22, 24),
                      n.trees = c(25,50,75,100,125,150), shrinkage = c(0.01, 0.05, 0.1, 0.2, 0.25, 0.3), 
                      n.minobsinnode=10)
gbmFit = train(as.factor(VETH)~ .-vpsaa-vpdda-tmaxa-vpddd-vpsad-sfmma
               -ddeg-etpja-TransAspd-taved-tdayd,
               method="gbm", tuneGrid = gbmGrid, trControl = fitControl, data=mull)
gbmFit

# modeling
mull.gbm2=gbm(VETH ~ .-vpsaa-vpdda-tmaxa-vpddd-vpsad-sfmma
              -ddeg-etpja-TransAspd-taved-tdayd,
              distribution="bernoulli",interaction.depth=24,
              n.trees=150, shrinkage=0.3,n.minobsinnode=10,data=mull)

mull.gbmopt.xvalpr=rep(0,nrow(mull))
xvs=rep(1:10,length=nrow(mull))
xvs=sample(xvs)
for(i in 1:10){
  train=mull[xvs!=i,]
  test=mull[xvs==i,]
  glub=gbm(VETH~ . -vpsaa-vpdda-tmaxa-vpddd-vpsad-sfmma
           -ddeg-etpja-TransAspd-taved-tdayd,
           distribution="bernoulli",interaction.depth=24,n.trees=150,
           shrinkage=0.3,n.minobsinnode=10,data=train)
  mull.gbmopt.xvalpr[xvs==i]=predict(glub,newdata=test,type="response",n.trees=50)
}

table(mull$VETH,round(mull.gbmopt.xvalpr))
class.sum(mull$VETH,mull.gbmopt.xvalpr)

table(val$VETH,round(predict(mull.gbm2,newdata=val,type="response",n.trees=150)))
class.sum(val$VETH,predict(mull.gbm2,newdata=val,type="response",n.trees=150))

# svm
library(e1071)
library(EZtune)

mullcut = read.csv('mull2cut.csv')
valcut = read.csv('newvalidcut.csv')

xmull = as.matrix(mullcut[,1:20])
ymull = as.vector(mullcut[,21])

mull.svm.tune <- eztune(xmull, ymull,method="svm", fast=FALSE, cross=10)
mull.svm.tune

mull.tunedsvm=svm(as.factor(VETH)~ .,
                   probability=TRUE, cost=47.001, gamma=31.97783, data=mullcut)

mull.tunedsvm.xvalpred=rep(0,nrow(mullcut))
xvs=rep(1:10,length=nrow(mullcut))
xvs=sample(xvs)
for(i in 1:10){
  train=mullcut[xvs!=i,]
  test=mullcut[xvs==i,]
  glub=svm(as.factor(VETH)~ .,
           probability=TRUE, cost = 47.001, gamma = 31.97783, data=train)
  mull.tunedsvm.xvalpred[xvs==i]=attr(predict(glub,test,probability=TRUE),"probabilities")[,1]
}

table(mullcut$VETH,round(mull.tunedsvm.xvalpred))
class.sum(mullcut$VETH,mull.tunedsvm.xvalpred)

mull.tunedsvm.valpred=predict(mull.tunedsvm,valcut,probability=TRUE)
table(valcut$VETH,round(attr(mull.tunedsvm.valpred,"probabilities")[,1]))
class.sum(valcut$VETH,attr(mull.tunedsvm.valpred,"probabilities")[,1])

# # # # #
# # # # #
# # 4 # #
# # # # #
# # # # #

library(cluster)
?hclust

pov = read.csv("Poverty.csv")

# cleaning up dataframe for clustering
rownames(pov) = pov$Country
pov = pov[,-4]

dd <- dist(scale(pov), method = "euclidean")
hc.ward <- hclust(dd, method = "ward.D2")
hc.cen <- hclust(dd, method = "cen")
plot(hc.ward, hang = -1)
plot(hc.cen, hang = -1)

# # # # #
# # # # #
# # 5 # #
# # # # #
# # # # #

chemo = read.csv("chemo2.csv")
chemolr1 = chemo[,-24:-28]
chemo = chemo[,-23]
chemolr2 = chemo[,-24:-27]
chemo = chemo[,-23]
chemolr3 = chemo[,-24:-26]
chemo = chemo[,-23]
chemolr4 = chemo[,-24:-25]
chemo = chemo[,-23]
chemolr5 = chemo[,-24]
chemolr6 = chemo[,-23]
chemoX = chemo[,-23:-28]

# # #
# a #
# # #

# PCA

# all lr are not needed, only useful is over x

# lr1
chemolr1.eig = eigen(cov(chemolr1))
100*chemolr1.eig$values/sum(chemolr1.eig$values)

# lr2
chemolr2.eig = eigen(cov(chemolr2))
100*chemolr2.eig$values/sum(chemolr2.eig$values)

# lr3
chemolr3.eig = eigen(cov(chemolr3))
100*chemolr3.eig$values/sum(chemolr3.eig$values)

# lr4
chemolr4.eig = eigen(cov(chemolr4))
100*chemolr4.eig$values/sum(chemolr4.eig$values)

# lr5
chemolr5.eig = eigen(cov(chemolr5))
100*chemolr5.eig$values/sum(chemolr5.eig$values)

# lr6
chemolr6.eig = eigen(cov(chemolr6))
100*chemolr6.eig$values/sum(chemolr6.eig$values)

# over x *important one*
chemoX.eig = eigen(cov(chemoX))
100*chemoX.eig$values/sum(chemoX.eig$values)

# over chemo
chemo.eig = eigen(cov(chemo))
100*chemo.eig$values/sum(chemo.eig$values)
9.966811e+01+1.674823e-01

# # #
# b #
# # #

chemo = read.csv("chemo2.csv")
chemoX = chemo[,-23:-28]
chemoY = chemo[,-1:-22]

# canonical correlation
library(CCP)
library(CCA)

cc1 <- cc(chemoY, chemoX)
rho <- cc1$cor

n <- dim(chemoY)[1]
p <- length(chemoY)
q <- length(chemoX)

p.asym(rho, n, p, q, tstat = "Wilks")







