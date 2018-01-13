#Logistic Regression

str(train)
dim(train)
table(train$Survived)
prop.table(table(train$Survived))
prop.table(table(train$Sex,train$Survived),2)
summary(train)
colSums(is.na(train))
table(train$Embarked)
which(is.na(train$Embarked))
train$Embarked[which(is.na(train$Embarked))]='S'
summary(train$Age)
train$Embarked[830]=' '
hist(train$Age, breaks = 50)
train$Age[which(is.na(train$Age))] = median(train$Age,na.rm = TRUE)
logreg = glm(Survived~ Sex + Age + Fare + Embarked, data = train, family = "binomial")
summary(logreg)
pred = predict(logreg,data = train, type  = "response")
summary(pred)
pred
?ifelse
pred_class = ifelse(pred < 0.5, 0, 1)
str(pred_class)
table(train$Survived, pred_class)

library(ROCR)

ROCR = prediction(pred,train$Survived)
ROC_curve = performance(ROCR, "tpr", "fpr")
plot(ROC_curve,colorize = T)


auc(train$Survived, pred_class)

library(pROC)
