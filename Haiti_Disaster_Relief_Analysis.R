# Jordan Bales
# jcb4dt
# SYS 6018 
# Disaster Relief project part 2
# 11/15/19
####################### Libraries needed ############################################
library(readr)
library(ISLR)
library(e1071)
library(rfUtilities)
library(class)
library(randomForest)
library(caret)
library(MASS)
library(pROC)
############################# Data import and Cleaning ######################################
#Data cleaning 
#1) seperated out into two subfolders Blue tarp and non Blue tarp
#2) Using TextrWrangler will ditch the headings and take out space between map x and map y 
#3) Will read in non blue tarp data, stack them, give them there ID of BT
#4) Will repeat 3 but with bluetarp
#5) Ditch the columns I don't want to deal with.

#Non Blue Tarp
orthovnir078_ROI_NON_Blue_Tarps = read.csv("~/Desktop/Hold+Out+Data/No Blue Tarp/orthovnir078_ROI_NON_Blue_Tarps.txt", row.names=1, sep="")
orthovnir069_ROI_NOT_Blue_Tarps = read.csv("~/Desktop/Hold+Out+Data/No Blue Tarp/orthovnir069_ROI_NOT_Blue_Tarps.txt", row.names=1, sep="")
orthovnir067_ROI_NOT_Blue_Tarps = read.csv("~/Desktop/Hold+Out+Data/No Blue Tarp/orthovnir067_ROI_NOT_Blue_Tarps.txt", row.names=1, sep="")
orthovnir057_ROI_NON_Blue_Tarps = read.csv("~/Desktop/Hold+Out+Data/No Blue Tarp/orthovnir057_ROI_NON_Blue_Tarps.txt", row.names=1, sep="")

NonBTdf= rbind(orthovnir078_ROI_NON_Blue_Tarps,orthovnir069_ROI_NOT_Blue_Tarps,orthovnir067_ROI_NOT_Blue_Tarps,orthovnir057_ROI_NON_Blue_Tarps)
NonBTdf$Tarp="No"

#Blue Tarp
orthovnir078_ROI_Blue_Tarps = read.csv("~/Desktop/Hold+Out+Data/Blue Tarp/orthovnir078_ROI_Blue_Tarps.txt", row.names=1, sep="")
orthovnir069_ROI_Blue_Tarps = read.csv("~/Desktop/Hold+Out+Data/Blue Tarp/orthovnir069_ROI_Blue_Tarps.txt", row.names=1, sep="")
orthovnir067_ROI_Blue_Tarps = read.csv("~/Desktop/Hold+Out+Data/Blue Tarp/orthovnir067_ROI_Blue_Tarps.txt", row.names=1, sep="")
#Probably not using this. I think it is an example of a cleaned version 
orthovnir067_ROI_Blue_Tarps_data = read.csv("~/Desktop/Hold+Out+Data/Blue Tarp/orthovnir067_ROI_Blue_Tarps_data.txt", sep="")

BTdf=rbind(orthovnir078_ROI_Blue_Tarps,orthovnir069_ROI_Blue_Tarps,orthovnir067_ROI_Blue_Tarps)
BTdf$Tarp="Yes"

#create hold out data frame. 
HoldOutDf=rbind(BTdf,NonBTdf)
#Going to make it mimic 
HoldOutDf=HoldOutDf[,7:10]
HoldOutDf=HoldOutDf[,c(4,1,2,3)]
colnames(HoldOutDf) = c("BT","Red","Green","Blue")
############################### Bring in the orginal data set provided months ago ##########################3
#Bring in the data
df = read_csv("Desktop/HaitiPixels.csv")
df$BT="No"
df$BT[df$Class=="Blue Tarp"] = "Yes"
df$BT=as.factor(df$BT)

############################### Initial analysis ######################################################
#looks at the class and their rgb codes
summary(df) #eh not useful
str(df)
unique(df$Class) #Here we go, "Vegetation",   "Soil",    "Rooftop" ,     "Various Non-Tarp" ,  "Blue Tarp"      

#Lets make class a factor
df$Class=as.factor(df$Class)

contrasts(df$Class)# Blue tarp is the base with 0000 
#This leads me to believe I should explore multinomial, but seeing as we haven't covered that 
#   I will lean toward binary "Is it a blue tarp or not" 

pairs(df)#interesting #some linearity 
plot(df$Class) #Blue Tarp is by far the smallest category. This could be really bad for prediction or really good. 

#Check if there are any nas
df[is.na(df),] #nope

#Perhaps there is a cleaner/better way to do this, but I am going ot make the decision binary. 
# In that I don't care if its not a blue tarp. So is a blue tarp or is not will be the factors of the new column
# df$BT="No"
# df$BT[df$Class=="Blue Tarp"] = "Yes"
# df$BT=as.factor(df$BT)

summary(df) #2022 Yes it is a tarp 61219 No it is not a tarp. 

#Bad guessing method 
2022/(9903+2022+20566+4744+26006) #0.03197293 be right 32% of the time by guessing 

############################# Random Forest ########################################
set.seed(1)
#Doing random 75/25 split
df=as.data.frame(df)
data1 = sort(sample(nrow(df), nrow(df)*.75))
train = df[data1,]
test = df[-data1,]
train$BT=as.factor(train$BT)
test$BT=as.factor(test$BT)
HoldOutDf$BT=as.factor(HoldOutDf$BT)
df$BT=as.factor(df$BT)

cvForest = randomForest(BT ~ Red+Green+Blue, data = train, nodesize = 3, ntree = 200)
predictForest = predict(cvForest, newdata = test)
table(test$BT, predictForest)
(15301 + 473)/nrow(test) #= .9976599 highest thus far

#cross validation 
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(BT ~ Red+Green+Blue, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
#optimal cp = 0.01
library(rpart)
TreeCV <- rpart(BT ~ Red+Green+Blue, data = train, method = "class", cp = 0.01)
predictionCV <- predict(TreeCV, newdata = test, type = "class")
table(test$BT, predictionCV)
(15283 + 447)/(15283+35+46+447) # 0.994877
confusionMatrix(predictionCV,test$BT,positive="Yes")

69/(69+15267)

###### Random forest with hold out data

HOForest=randomForest(BT ~ Red+Green+Blue, data  = df, mtry=3,importance=TRUE) #train on original
yhat.rf = predict(HOForest,newdata=HoldOutDf)
mean((yhat.rf - HoldOutDf)^2)
importance(HOForest)
varImpPlot(HOForest)

predictForest = predict(HOForest, newdata = HoldOutDf)
table(HoldOutDf$BT, predictForest)
HOForest$confusion

confusionMatrix(predictForest,HoldOutDf$BT,positive="Yes")
3776/(3776+1978475 ) #0.001904905 false positve rate

######## ############################ Support vector Machines ######################
#With cross validation 
#####Test Train data test
set.seed(1)
#Doing random 75/25 split
df=as.data.frame(df)
df2=df[,2:5]
data1 = sort(sample(nrow(df2), nrow(df2)*.75))
train = df2[data1,]
test = df2[-data1,]

x = subset(train, select=-BT)
y = train$BT

svm_model = svm(BT ~ ., data=train)
summary(svm_model)

#might go back and use test here
pred = predict(svm_model,x)
table(pred,y)

svm_tune = tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)
summary(svm_tune)

bestmodelsvm= svm_tune$best.model
bestmodelsvm


svm_model_after_tune = svm(BT ~ ., data=train, kernel="radial", cost=100, gamma=2)
summary(svm_model_after_tune)

#make x and y from test data
xx = subset(test, select=-BT)
yy = test$BT

predtest = predict(svm_model_after_tune,xx)
table(predtest, yy)
confusionMatrix(predtest,yy,positive="Yes")

#using hold out data as opposed to test train split 
x = subset(df2, select=-BT)
y = df2$BT
svm_model = svm(BT ~ ., data=df2)
summary(svm_model)

pred = predict(svm_model,x)
table(pred,y)
svm_tune = tune(svm, train.x=x, train.y=y,kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)
summary(svm_tune)

svm_model_after_tune = svm(BT ~ ., data=df2, kernel="radial", cost=100, gamma=2)
summary(svm_model_after_tune)

#make x and y from test data
xx = subset(HoldOutDf, select=-BT)
yy = HoldOutDf$BT

predtest = predict(svm_model_after_tune,xx)
table(predtest, yy)
confusionMatrix(predtest,yy,positive="Yes")

################## KNN #################################
### Cross Validation 
#create split 
#####Test Train data test
set.seed(1)
#Doing random 75/25 split
df=as.data.frame(df)
df2=df[,2:5]
data1 = sort(sample(nrow(df2), nrow(df2)*.75))
train = df2[data1,]
test = df2[-data1,]

train.X=cbind(train$Red,train$Green,train$Blue)
test.X=cbind(test$Red,test$Green,test$Blue)
train.BT = train$BT

#Apparently this is a logical way to choose k? Better than sqrt(n) but ill probs test that as well. 
sqrt(63241) #which is 251 
sqrt(47430)#217
sqrt(15811) #125 
#Yea im not a fan of any of these k values 

ctrl = trainControl(method="repeatedcv",repeats = 3)
#NOTE this will take 20-30 minutes to run. 
knnFit <- train(BT ~ ., data = train, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
knnFit
#It ultimately chooses k=15. Went from 5 to 43 by odd numbers

#Use plots to see optimal number of clusters:
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(knnFit)

knn.pred=knn(train.X,test.X,train.BT,k=15)
table(knn.pred,test$BT)
mean(knn.pred==test$BT)#0.9962684

confusionMatrix(knn.pred,test$BT,positive="Yes")

#### Using the hold out data
train.X=cbind(df2$Red,df2$Green,df2$Blue)
test.X=cbind(HoldOutDf$Red,HoldOutDf$Green,HoldOutDf$Blue)
train.BT = df2$BT

ctrl = trainControl(method="repeatedcv",repeats = 3)
#NOTE this will take 20-30 minutes to run. 
knnFit <- train(BT ~ ., data = df2, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
knnFit

#Use plots to see optimal number of clusters:
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(knnFit)

knn.pred=knn(train.X,test.X,train.BT,k=5)
confusionMatrix(knn.pred,HoldOutDf$BT,positive="Yes")

######### LDA 
### Cross validation
#####Test Train data test
set.seed(1)
#Doing random 75/25 split
df=as.data.frame(df)
df2=df[,2:5]
data1 = sort(sample(nrow(df2), nrow(df2)*.75))
train = df2[data1,]
test = df2[-data1,]


lda.fit2=lda(BT~Red+Green+Blue,data=train)
lda.fit2
plot(lda.fit2)

ldapred2 = predict(lda.fit2, newdata=test, type="response")
ldaclass2 = ldapred2$class
confusionMatrix(ldaclass2,test$BT,positive="Yes")

# #plot ROC
# library(ROCR)
# library(Metrics)
# pr <- prediction(ldapred,test$Class)
# perf <- performance(pr,measure = "tpr",x.measure = "fpr")
# plot(perf) > auc(test$Class,ldapred)
# 
# 
# plot.roc(lda.fit)

### Holdout data
lda.fit2=lda(BT~Red+Green+Blue,data=df2)
lda.fit2
plot(lda.fit2)

ldapred2 = predict(lda.fit2, newdata=HoldOutDf, type="response")
ldaclass2 = ldapred2$class
confusionMatrix(ldaclass2,HoldOutDf$BT,positive="Yes")

################### QDA
## Cross validation 
#####Test Train data test
set.seed(1)
#Doing random 75/25 split
df=as.data.frame(df)
df2=df[,2:5]
data1 = sort(sample(nrow(df2), nrow(df2)*.75))
train = df2[data1,]
test = df2[-data1,]

fitqda2= qda(BT~Red+Green+Blue,data=train)
fitqda2
summary(fitqda2)

qdapred2 = predict(fitqda2, newdata=test, type="response")
qdaclass2 = qdapred2$class
confusionMatrix(qdaclass2,test$BT,positive="Yes")

#Hold out data
fitqda2= qda(BT~Red+Green+Blue,data=df2)
fitqda2
summary(fitqda2)

qdapred2 = predict(fitqda2, newdata=HoldOutDf, type="response")
qdaclass2 = qdapred2$class
confusionMatrix(qdaclass2,HoldOutDf$BT,positive="Yes")

##### Logistic regression 
#cv
#####Test Train data test
set.seed(1)
#Doing random 75/25 split
df=as.data.frame(df)
df2=df[,2:5]
data1 = sort(sample(nrow(df2), nrow(df2)*.75))
train = df2[data1,]
test = df2[-data1,]

# #fit.glm <- train(diabetes~., data=dataset, method="glm", metric=metric, trControl=control)
# glm.fits=glm(BT~Red+Green+Blue,family=binomial,data= train)
# summary(glm.fits)
# 
# glm.probs=predict(glm.fits,test,type="response")
# summary(glm.probs)
# 
# library(InformationValue)
# optCutOff = optimalCutoff(test$BT, glm.probs)[1] 
# 
# confusionMatrix(glm.probs,test$BT, threshold = optCutOff)
# 
# table(test$BT, glm.probs > 0.5)
# 
# logitMod <- glm(BT~Red+Green+Blue, data=train, family=binomial(link="logit"))
# 
# library(caret)
# # Use your model to make predictions, in this example newdata = training set, but replace with your test set    
# pdata <- predict(logitMod, newdata = test, type = "response")
# 
# # use caret and compute a confusion matrix
# confusionMatrix(pdata , test$BT, optCutOff)

#### CV
train$BTB <- ifelse(train$BT == "Yes", 1, 0)
train$BTB <- factor(train$BTB, levels = c(0, 1))

test$BTB <- ifelse(test$BT == "Yes", 1, 0)
test$BTB <- factor(test$BTB, levels = c(0, 1))
# Step 1: Build Logit Model on Training Dataset
logitMod <- glm(BTB~Red+Green+Blue, family="binomial", data = train)

# Step 2: Predict Y on Test Dataset
pred <- predict(logitMod, newdata = test, type = "response")

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test$BTB

# Accuracy
mean(y_pred == y_act)  # 0.9713491

table(test$BT, pred > 0.5)

###### Hold out data
df2$BTB <- ifelse(df2$BT == "Yes", 1, 0)
df2$BTB <- factor(df2$BTB, levels = c(0, 1))

HoldOutDf$BTB <- ifelse(HoldOutDf$BT == "Yes", 1, 0)
HoldOutDf$BTB <- factor(HoldOutDf$BTB, levels = c(0, 1))

# Step 1: Build Logit Model on Training Dataset
logitMod <- glm(BTB~Red+Green+Blue, family="binomial", data = df2)
summary(logitMod)
# Step 2: Predict Y on Test Dataset
pred <- predict(logitMod, newdata = HoldOutDf, type = "response")

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- HoldOutDf$BTB

# Accuracy
mean(y_pred == y_act)  # 0.9713491

table(HoldOutDf$BT, pred > 0.5)

#plot ROC
library(ROCR)
library(Metrics)
pr <- prediction(log_predict,new_test$Survived)
perf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf)
auc(HoldOutDf$BT,logitMod)


library(pROC)


fit <- logitMod

roc(HoldOutDf$BT, as.vector(fitted.values(fit)), percent=F,   boot.n=1000, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    # print.thres = c(0.30,0.35, 0.40, 0.45,0.48, 0.50,0.55, 0.60),#
    print.auc = TRUE, print.thres.col = "blue", ci=TRUE, ci.type="bars", print.thres.cex = 0.7, main = paste("ROC curve using","(N = ",nrow(aSAH),")") )



