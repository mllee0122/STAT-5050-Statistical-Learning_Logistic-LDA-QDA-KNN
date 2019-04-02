#Statistical Learning_Homework4
#107064522

library(MASS)
library(ISLR)
?Weekly
summary(Weekly)
pairs(Weekly)
cor(Weekly[,-9])
table(Weekly$Direction)

tr_data = Weekly[Weekly$Year<=2008,]
te_data = Weekly[Weekly$Year>2008,]

##### 10. (d) #####
# Logistic Regression
model_logi <- glm(Direction~Lag2, data = tr_data, family = binomial)

#1 logit trans to probability
pred_logi_logit_tr = predict(model_logi, type = 'response', tr_data)
pred_logi_logit = predict(model_logi, type = 'response', te_data)
head(pred_logi_logit)
#head(1/(1+exp(-pred_logi_logit))) # Sigmoid

#2 Probability
logi_up <- fitted(model_logi)
head(logi_up)
logi_down <- 1-logi_up

# Check 1 == 2
all(round(pred_logi_logit_tr, 6) == round(logi_up, 6))

# Predict y
pred_logi <- as.factor(ifelse(pred_logi_logit>=0.5, "Up", "Down"))
head(pred_logi)

# Table
table_logi <- table(pred_logi, te_data$Direction)

##### 10. (e) #####
# LDA
model_lda <- lda(Direction~Lag2, data = tr_data)
# Predict y
pred_lda = predict(model_lda, te_data)
head(pred_lda$class)
# Table
table_lda <- table(pred_lda$class, te_data$Direction)
# Predict the probability
head(pred_lda$posterior)

##### 10. (f) #####
# QDA
model_qda <- qda(Direction~Lag2, data = tr_data)
# Predict y
pred_qda = predict(model_qda, te_data)
head(pred_qda$class)
# Table
table_qda <- table(pred_qda$class, te_data$Direction)
# Predict the probability
head(pred_qda$posterior)

##### 10. (g) #####
# KNN
library(class)
tr = as.matrix(tr_data$Lag2)
te = as.matrix(te_data$Lag2)
model_knn <- knn(tr, te, tr_data$Direction, k = 1, prob = T)
# Predict and probability
model_knn
# Table
table_knn <- table(model_knn, te_data$Direction)

##### Extra #####
#install.packages("e1071")
library(e1071)
# Naive Bayes
model_nb <- naiveBayes(Direction~Lag2, data = tr_data)
model_nb
# Predict y
pred_nb <- predict(model_nb, te_data)
head(pred_nb)
# Table
table_nb <- table(pred_nb, te_data$Direction)

# predict the probability
pred_nb_p <- predict(model_nb, te_data, type = "raw")
View(round(pred_nb_p, 6))

# check the conditional mean and conditional sd
mu_Up <- apply(as.matrix(tr_data$Lag2[tr_data$Direction=="Up"]),2,mean) ; mu_Up
mu_Down <- apply(as.matrix(tr_data$Lag2[tr_data$Direction=="Down"]),2,mean) ; mu_Down
sd_Up <- apply(as.matrix(tr_data$Lag2[tr_data$Direction=="Up"]),2,sd) ; sd_Up
sd_Down <- apply(as.matrix(tr_data$Lag2[tr_data$Direction=="Down"]),2,sd) ; sd_Down

##### 10. (h) #####
# Accuracy
Accuracy_logi = sum(diag(table_logi))/sum(table_logi)
Accuracy_lda = sum(diag(table_lda))/sum(table_lda)
Accuracy_qda = sum(diag(table_qda))/sum(table_qda)
Accuracy_knn = sum(diag(table_knn))/sum(table_knn)
Accuracy_nb = sum(diag(table_nb))/sum(table_nb)

# Sensitivity
Sensitivity_logi = table_logi[1,1]/sum(table_logi[,1])
Sensitivity_lda = table_lda[1,1]/sum(table_lda[,1])
Sensitivity_qda = table_qda[1,1]/sum(table_qda[,1])
Sensitivity_knn = table_knn[1,1]/sum(table_knn[,1])
Sensitivity_nb = table_nb[1,1]/sum(table_nb[,1])

# Specificity
Specificity_logi = table_logi[2,2]/sum(table_logi[,2])
Specificity_lda = table_lda[2,2]/sum(table_lda[,2])
Specificity_qda = table_qda[2,2]/sum(table_qda[,2])
Specificity_knn = table_knn[2,2]/sum(table_knn[,2])
Specificity_nb = table_nb[2,2]/sum(table_nb[,2])