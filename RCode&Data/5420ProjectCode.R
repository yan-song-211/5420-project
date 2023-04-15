#######################################
# Source Code for DASC5420 Course Project
# Author: Yan Song
# Date: April 14, 2023
#######################################

pkg_list <- c("caret", "e1071", "ROCR", "tree", "randomForest","gbm", 
              "neuralnet", "dplyr","ggplot2","gridExtra")
# Install packages if needed
for (pkg in pkg_list)
{
  # Try loading the library.
  if ( ! library(pkg, logical.return=TRUE, character.only=TRUE) )
  {
    # If the library cannot be loaded, install it; then load.
    install.packages(pkg)
    library(pkg, character.only=TRUE)
  }
}

## Load dataset: Bank Marketing Dataset
bank.data <- read.csv("bank-additional.csv", sep = ";")
col.names <- names(bank.data)
## Exclude the variable "duration" which highly affects the output as suggested by the author
bank.data <- bank.data[,-11]
col.names <- names(bank.data)
sum(is.na(bank.data))

## Exploratory Data Analysis
## Plot the boxplots for the numeric variables
p1 <- ggplot(bank.data, aes(x=y, y=age, color=y)) + geom_boxplot() + theme(text = element_text(size = 16))
p2 <- ggplot(bank.data, aes(x=y, y=campaign, color=y)) + geom_boxplot() + theme(text = element_text(size = 16))
p3 <- ggplot(bank.data, aes(x=y, y=previous, color=y)) + geom_boxplot() + theme(text = element_text(size = 16))
p4 <- ggplot(bank.data, aes(x=y, y=emp.var.rate, color=y)) + geom_boxplot()+theme(text = element_text(size = 16))
grid.arrange(p1, p2, p3, p4, nrow = 2)
p5 <- ggplot(bank.data, aes(x=y, y=cons.price.idx, color=y)) + geom_boxplot() + theme(text = element_text(size = 16))
p6 <- ggplot(bank.data, aes(x=y, y=cons.conf.idx, color=y)) + geom_boxplot() + theme(text = element_text(size = 16))
p7 <- ggplot(bank.data, aes(x=y, y=euribor3m, color=y)) + geom_boxplot() + theme(text = element_text(size = 16))
p8 <- ggplot(bank.data, aes(x=y, y=nr.employed, color=y)) + geom_boxplot() + theme(text = element_text(size = 16))
grid.arrange(p5, p6, p7, p8, nrow = 2)

## Create the list of variables which are considered as categorical 
ctgc.var.list <- c("job", "marital", "education", "default", "housing", "loan",
                   "contact", "month", "day_of_week","pdays", "poutcome", "y")
for (i in ctgc.var.list)
  bank.data[[i]] <- as.factor(bank.data[[i]])

## Plot the histograms for the categorical variable
p1 <- ggplot(data = bank.data, aes(x = job, fill = y)) + geom_bar()+theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
p2 <- ggplot(data = bank.data, aes(x = marital, fill = y)) + geom_bar()+theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
p3 <- ggplot(data = bank.data, aes(x = education, fill = y)) + geom_bar()+theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
p4 <- ggplot(data = bank.data, aes(x = default, fill = y)) + geom_bar()+theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
grid.arrange(p1, p2, p3, p4, nrow = 1)
p5 <- ggplot(data = bank.data, aes(x = housing, fill = y)) + geom_bar()+theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
p6 <- ggplot(data = bank.data, aes(x = loan, fill = y)) + geom_bar()+theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
p7 <- ggplot(data = bank.data, aes(x = contact, fill = y)) + geom_bar()+theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
p8 <- ggplot(data = bank.data, aes(x = month, fill = y)) + geom_bar()+theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
grid.arrange(p5, p6,p7, p8, nrow = 1)
p9 <- ggplot(data = bank.data, aes(x = day_of_week, fill = y)) + geom_bar()+theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
p10 <- ggplot(data = bank.data, aes(x = pdays, fill = y)) + geom_bar()+theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
p11<- ggplot(data = bank.data, aes(x = poutcome, fill = y)) + geom_bar()+theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
p12<- ggplot(data = bank.data, aes(x = y, fill = y)) + geom_bar()+theme(text = element_text(size = 16))
grid.arrange(p9, p10, p11, p12, nrow = 1)

## Data Manipulation
## Check
nrow(bank.data[bank.data$y == "yes", ])/nrow(bank.data)
nrow(bank.data[bank.data$default == "yes", ]) ## only one
nrow(bank.data[bank.data$education == "illiterate", ]) ## only one
nrow(bank.data[bank.data$pdays == 999, ]) ## 3959 out of 4119
nrow(bank.data[bank.data$marital == "unknown", ])## only 11
## Manipulate
bank.data.2 <- read.csv("bank-additional.csv", sep = ";")
bank.data.2 <- bank.data.2[,-11]
bank.data.2[bank.data.2$default == "yes", ]$default="unkown"
bank.data.2[bank.data.2$education == "illiterate", ]$education="unkown"
bank.data.2[bank.data.2$pdays != 999, ]$pdays=0 ## combine other pdays values other than 999 to one type 
bank.data.2[bank.data.2$marital == "unknown", ]$marital="married"
for (i in ctgc.var.list)
  bank.data.2[[i]] <- as.factor(bank.data.2[[i]])

## Scale the data
numerical_cols <-  sapply(bank.data.2, is.numeric)
bank.data.2[, numerical_cols] <- scale(bank.data.2[, numerical_cols])

## Data set Partition 
set.seed(5420)
test.prop <- 0.2 ## test set proportion as 20%
test.id <- sample(nrow(bank.data.2), test.prop*nrow(bank.data.2))
train.data <- bank.data.2[-test.id,]
test.data <- bank.data.2[test.id,]

###################### SVM #########################
## Hyper parameter tuning by repeated cross validation
set.seed(1)
train.control<- trainControl(method ="repeatedcv", number = 5)
svm.model <- train(y ~., data = train.data, method = "svmLinear",
                   trControl = train.control, 
                   tuneGrid = expand.grid(C = seq(0, 10, length = 20)))
svm.model$bestTune
plot(svm.model, xvar="Cost", label=T)
## Test by SVM with Linear kernel with best C
svm.prob <- predict(svm.model, newdata=test.data)
confusionMatrix(svm.prob, test.data$y)
## Plot ROC Curves for different kernels in SVM
ROC.SVM.plot <- function(Ker, c, f){
  m <- svm(y~., data = train.data, kernel=Ker, probability = TRUE)
  prob <- predict(m, type="prob", newdata=test.data, probability = TRUE)
  prob.rocr <- ROCR::prediction(attr(prob, "probabilities")[,2], test.data$y)
  perf <- performance(prob.rocr, "tpr","fpr")
  AUC <- round(performance(prob.rocr, "auc")@y.values[[1]], 2)
  plot(perf, col=c, add=f)
  return(AUC)
}
# Save .eps file
setEPS()                                             
postscript("svm_roc.eps")                           
auc1 <- ROC.SVM.plot("linear", 6, FALSE)
auc2 <- ROC.SVM.plot("radial", 5, TRUE)
auc3 <- ROC.SVM.plot("sigmoid", 4,TRUE)
auc4 <- ROC.SVM.plot("polynomial", 3,TRUE)
legend(0.5, 0.2, 
       legend=c(paste("Linear (AUC=", auc1, ")"), paste("Radial (AUC=", auc2, ")"), 
                paste("Sigmoid (AUC=", auc3, ")"), paste("Polynomial (AUC=", auc4, ")")), 
       col=c(6,5,4,3), lty=1, cex=1)
title("ROC curves of different kernels for SVM")
dev.off()
## Test by SVM with Sigmoid Kernel
svm.sigmoid <- svm(y~., data = train.data, kernel="sigmoid")
sigmoid.pred <- predict(svm.sigmoid, newdata=test.data)
confusionMatrix(sigmoid.pred, test.data$y)
## Save ROC curve data
svm.sigmoid <- svm(y~., data = train.data, kernel="sigmoid", probability = TRUE)
svm.sigmoid.prob <- predict(svm.sigmoid, type="prob", newdata=test.data, probability = TRUE)
svm.sigmoid.rocr <- ROCR::prediction(attr(svm.sigmoid.prob, "probabilities")[,2], test.data$y)

####################### Decision Tree #################
## Train a full decision tree
tree1 <- tree(y~ . , data=train.data, depth)
# Save .eps file 
setEPS()                                          
postscript("tree.eps") 
plot(tree1)
text(tree1, pretty = 0)
dev.off()
## Test with a full decision tree
tree1.pred <- predict(tree1, test.data, type = "class")
confusionMatrix(tree1.pred, test.data$y)
## Pruning Tree
set.seed(5420)
tree.prune <- cv.tree(tree1, FUN = prune.misclass)
tree.prune
plot(tree.prune$size , tree.prune$dev, type = "b")
pruned.tree <- prune.misclass(tree1 , best = 3)
# Save .eps file 
setEPS()                                          
postscript("treeprune.eps")  
plot(pruned.tree)
text(pruned.tree, pretty = 0)
dev.off()
## Test with a pruned tree
pruned.tree.pred <- predict(pruned.tree, test.data, type = "class")
confusionMatrix(pruned.tree.pred, test.data$y)
## Save ROC curve data
pruned.tree.prob <- predict(pruned.tree, newdata = test.data, type = "vector")
pruned.tree.rocr <- ROCR::prediction(pruned.tree.prob[,2], test.data$y)

########################## Random Forest #######################
set.seed(5420)
trControl <- trainControl(method = "cv", number = 5, search = "grid")
Rf.cv <- train(y~., train.data, method = "rf", metric= "Accuracy", trControl = trainControl(), tuneGrid = NULL)
plot(Rf.cv)
Rf.pred <- predict(Rf.cv, newdata = test.data)
confusionMatrix(Rf.pred, test.data$y)
## Plot OOB error vs number of trees with different mtrys
RF <- randomForest(y ~ ., data = train.data, mtry = 2, importance = TRUE)
r1 <- RF$err.rate[,1]
RF <- randomForest(y ~ ., data = train.data, mtry = 4, importance = TRUE)
r2 <- RF$err.rate[,1]
RF <- randomForest(y ~ ., data = train.data, mtry = 6, importance = TRUE)
r3 <- RF$err.rate[,1]
plot(r1,type='l',col=3, xlab="number of trees", ylab="OOB error")
lines(r2,col=4)
lines(r3,col=5)
legend(300, 0.13, 
       legend=c("mtry=2", "mtry=6","mtry=10"), 
       col=c(3,4,5), lty=1, cex=.8)
title("OOB errors v.s. number of trees")
## Save ROC curve data
Rf.prob <- predict(Rf.cv, newdata = test.data, type = "prob")
Rf.rocr <- ROCR::prediction(Rf.prob[,2], test.data$y)

####################### Boosting ##########################
set.seed(5420)
train.data$y <- factor(ifelse(train.data$y == "yes", 1, 0))
test.data$y <- factor(ifelse(test.data$y == "yes", 1, 0))
boostgrid <- expand.grid(n.trees=c(10, 50, 200), 
                         interaction.depth=c(1, 3, 5), 
                         shrinkage=c(0.01, 0.1, 0.3),
                         n.minobsinnode=c(10, 15, 20))
gbmModel <- train(y ~., data = train.data, 
                  method = 'gbm', 
                  tuneGrid = boostgrid, 
                  verbose = FALSE)
gbmModel$bestTune
boost.pred <- predict(gbmModel, newdata = test.data, n.trees = 10, type="prob")
boost.cv.pred <- ifelse(boost.pred[2]  > 0.5, 1, 0)
confusionMatrix(as.factor(boost.cv.pred), test.data$y)
## Save ROC curve for boosting
boost.rocr <- ROCR::prediction(boost.pred[2], test.data$y)

##################### Neural Network ####################
set.seed(1)
dummies <- dummyVars("y ~ .", train.data)
dummy.train <- data.frame(predict(dummies, train.data), y=train.data$y)
dummies <- dummyVars("y ~ .", test.data)
dummy.test <- data.frame(predict(dummies, test.data))
nn <- neuralnet(y ~., data = dummy.train, hidden=c(5,2), act.fct = "logistic", linear.output=FALSE)
nn.prob <- predict(nn, dummy.test)
nn.pred <- ifelse(nn.prob[,2]> 0.5, 1, 0)
confusionMatrix(as.factor(nn.pred), test.data$y)
## save ROC curve data
nn.rocr <-ROCR::prediction(nn.prob[,2], test.data$y)

## Plot ROC curves for all models
setEPS()                                          
postscript("allroc.eps")
ROC.plot <- function(rocr, c, f){
  perf <- performance(rocr, "tpr","fpr")
  AUC <- round(performance(rocr, "auc")@y.values[[1]], 2)
  plot(perf, col=c, add=f)
  return(AUC)
}
auc1 <- ROC.plot(svm.sigmoid.rocr, 6, FALSE)
auc2 <- ROC.plot(pruned.tree.rocr, 5, TRUE)
auc3 <- ROC.plot(Rf.rocr, 4,TRUE)
auc4 <- ROC.plot(boost.rocr, 3,TRUE)
auc5 <- ROC.plot(nn.rocr, 9,TRUE)
legend(0.3, 0.3, 
       legend=c(paste("SVM (AUC=", auc1, ")"), paste("DecisionTree (AUC=", auc2, ")"), 
                paste("RandomForest (AUC=", auc3, ")"), paste("Boosting (AUC=", auc4, ")"), 
                paste("NeuralNetwork (AUC=", auc5, ")")),
       col=c(6,5,4,3,9), lty=1, cex=1)
title("ROC curves of different classification models")
dev.off()