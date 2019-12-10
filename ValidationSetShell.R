#open data
bodyfat = read.csv("bodyfat.csv")
dim(bodyfat)
n = dim(bodyfat)[1]
names(bodyfat) 

# specify models to consider
#model list specification
LinModel1 = (BodyFatSiri ~ Abs)
LinModel2 = (BodyFatSiri ~ Abs+Weight)
LinModel3 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm)
LinModel4 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck+Biceps+Age)
LinModel5 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck+Biceps+Age+Thigh+Hip+Ankle)
LinModel6 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck+Biceps+Age+Thigh+Hip+Ankle+BMI+Height+Chest+Knee)
allLinModels = list(LinModel1,LinModel2,LinModel3,LinModel4,LinModel5,LinModel6)	
nLinmodels = length(allLinModels)

library(glmnet)  # use RR and LASSO modeling commands from package glmnet 
lambdalistRR = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5)  # specifies RR models to consider
nRRmodels = length(lambdalistRR)
lambdalistLASSO = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5)  # specifies LASSO models to consider
nLASSOmodels = length(lambdalistLASSO)
nmodels = nLinmodels+nRRmodels+nLASSOmodels


################################################################
##### Validation set assessment of entire modeling process #####				 
################################################################

##### model assessment outer validation shell #####
fulldata.out = bodyfat
k.out = 10 
n.out = dim(fulldata.out)[1]
#define the split into training set (of size about 2/3 of data) and validation set (of size about 1/3)
n.train.out = round(n.out*2/3); n.train.out
n.valid.out = n.out-n.train.out; n.valid.out
set.seed(8)
valid.out = sample(1:n.out,n.valid.out)  #produces list of data to exclude
include.train.out = !is.element(1:n.out,valid.out)  # sets up a T-F vector to be used similarly as group T-F vectors
include.valid.out = is.element(1:n.out,valid.out)  # sets up a T-F vector to be used similarly as group T-F vectors

#just one split into training and validation sets
traindata.out = bodyfat[include.train.out,]
trainx.out = model.matrix(BodyFatSiri~.,data=traindata.out)[,-(1:4)]
trainy.out = traindata.out[,3]
validdata.out = bodyfat[include.valid.out,]
validx.out = model.matrix(BodyFatSiri~.,data=validdata.out)[,-(1:4)]
validy.out = validdata.out[,3]

  ### entire model-fitting process  ###
fulldata.in = traindata.out
  ###	:	:	:	:	:	:	:   ###
  ###INCLUDING ALL CONSIDERED MODELS###				 
  ###   :	:	:	:	:	:	:   ###
  ### resulting in bestmodel.in ###

if (bestmodel.in <= nLinmodels) {  # then best is one of linear models
  allpredictedvalid.out = predict(bestfit,validdata.out)
} else if (bestmodel.in <= nRRmodels+nLinmodels) {  # then best is one of RR models
  allpredictedvalid.out = predict(bestfit,newx=validdata.out,s=bestlambdaRR)
} else {  # then best is one of LASSO models
  allpredictedvalid.out = predict(bestfit,newx=validdata.out,s=bestlambdaLASSO)
}

plot(allpredictedvalid.out,validy.out)
MSE.out = sum((allpredictedvalid.out-validy.out)^2)/n.valid.out; MSE.out
R2.out = 1-sum((allpredictedvalid.out-validy.out)^2)/sum((validy.out-mean(validy.out))^2); R2.out

