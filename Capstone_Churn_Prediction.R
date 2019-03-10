###########################################################################
############################# Churn Prediction Capstone ###################
###########################################################################

#check for installed packages
if(!require(readr)) install.packages("readr")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(dslabs)) install.packages("dslabs")
if(!require(data.table)) install.packages("data.table")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(knitr)) install.packages("knitr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(ggcorrplot)) install.packages("ggcorrplot")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(cowplot)) install.packages("cowplot")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(ROCR)) install.packages("ROCR")
if(!require(car)) install.packages("car")
if(!require(gmodels)) install.packages("gmodels")


library(readr)
library(MASS)
library(caret)
library(kableExtra)
library(gmodels)
library(car)
library(e1071)
library(C50)
library(caret)
library(rpart.plot)
library(rpart)
library(ROCR)
library(cowplot)
library(dplyr)
library(ggcorrplot)
library(corrplot)
library(RColorBrewer)
library(gridExtra)
library(dslabs)
library(data.table)
library(ggrepel)
library(ggthemes)
library(tidyr)
library(stringr)
library(ggplot2)
library(knitr)


# Telco Churn Prediction dataset:
# https://community.watsonanalytics.com/wp-content/uploads/2015/03/WA_Fn-UseC_-Telco-Customer-Churn.csv?cm_mc_uid=58920755505115141495567&cm_mc_sid_50200000=1514149556&cm_mc_sid_52640000=1514149556"


dl <- tempfile()
download.file("https://community.watsonanalytics.com/wp-content/uploads/2015/03/WA_Fn-UseC_-Telco-Customer-Churn.csv?cm_mc_uid=58920755505115141495567&cm_mc_sid_50200000=1514149556&cm_mc_sid_52640000=1514149556", dl)

churn_dataset = read_csv(dl)

#####################################################################
######################### Data Check ################################
#####################################################################

# Overview of edx Data
head(churn_dataset)

#Rows and columns of edx
dim(churn_dataset)

# Checking edx missing values
any(is.na(churn_dataset))

# Summary of the dataset
summary(churn_dataset)


churn_dataset <- churn_dataset[complete.cases(churn_dataset),] 

#To see variables structure 
str(churn_dataset)

#Distinct Values in dataset
unique <- data.frame(lapply(churn_dataset, function(x) length(table(x))))
unique <- gather(unique, key="Attributes", value="unique_values")
unique %>%arrange(desc(unique_values))


#####################################################################
######################### Distribution Insights #####################
#####################################################################

glimpse(churn_dataset)

# Churn Customers Distribution
churn_dataset %>%
  group_by(Churn) %>%
  summarize(n = n()) %>%
  mutate("Share"=paste(round(100*n/sum(n),2),"%",sep="")) %>%
  ggplot(aes(x = Churn, y=Share)) +
  geom_bar(stat="identity",colour = "royalblue", fill = "royalblue") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.25, hjust = 1)) +
  labs(title = "Distribution of Churn Customers", x = "Churn", y = "Share")

# Distribution for Continuous features w.r.t churn

#Monthly Charges
ggplot(data = churn_dataset, aes(MonthlyCharges, color = Churn))+
  scale_color_brewer(palette = "Dark2") +
  geom_freqpoly(binwidth = 2, size = 1)+
  labs(title = "Monthly Charges vs Customer Count", x = "Monthly Charges", y = "Count")

#Total Charges
ggplot(data = churn_dataset, aes(TotalCharges, color = Churn))+
  geom_freqpoly(binwidth = 100, size = 1)+
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Total Charges vs Customer Count", x = "Total Charges", y = "Count")

#Tenure
ggplot(data = churn_dataset, aes(tenure, colour = Churn))+
  geom_freqpoly(binwidth = 2, size = 1)+
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Tenure vs Customer Count", x = "Tenure", y = "Count")  


#Plotting Correlation among variables
options(repr.plot.width =6, repr.plot.height = 4)
churn_cor <- round(cor(churn_dataset[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)
ggcorrplot(churn_cor, hc.order = TRUE, type = "full",method="circle",title="Correlation")


# Distribution for Categorical features w.r.t churn

#Customer Attributes
options(repr.plot.width = 12, repr.plot.height = 10)
plot_grid(ggplot(churn_dataset, aes(x=gender,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(churn_dataset, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn_dataset, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn_dataset, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn_dataset, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn_dataset, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 12)),
          align = "h")

#Service Attributes
options(repr.plot.width = 12, repr.plot.height = 10)
plot_grid(ggplot(churn_dataset, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(churn_dataset, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn_dataset, aes(x=StreamingMovies,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn_dataset, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn_dataset, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 14)),
          align = "h")

#Billing & Contract Attributes
options(repr.plot.width = 12, repr.plot.height = 10)
plot_grid(ggplot(churn_dataset, aes(x=Contract,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(churn_dataset, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn_dataset, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 1)) +scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn_dataset, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn_dataset, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("green3","red3")) +theme_bw()+scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")


#####################################################################
######################### Data Proprocessing ########################
#####################################################################

# Remove unwanted like CustomerID from dataset
churn_dataset <- churn_dataset[, -1]


# Encode Variables i.e. replacing Churn status with Yes as 1 and No as 0
churn_dataset$Churn <- replace(churn_dataset$Churn, churn_dataset$Churn == "No", 0)
churn_dataset$Churn <- replace(churn_dataset$Churn, churn_dataset$Churn == "Yes", 1)
churn_dataset$Churn <- as.numeric(churn_dataset$Churn)

# Recoding all other categorical variables.
churn_dataset$gender <- recode(churn_dataset$gender, "'Male'=1; 'Female'=0")
churn_dataset$Partner <- recode(churn_dataset$Partner, "'Yes'=1; 'No'=0")
churn_dataset$Dependents <- recode(churn_dataset$Dependents, "'Yes'=1; 'No'=0")
churn_dataset$PhoneService <- recode(churn_dataset$PhoneService, "'Yes'=1; 'No'=0")
churn_dataset$MultipleLines <- recode(churn_dataset$MultipleLines, "'Yes'=1; 'No'=0;'No phone service'=3")
churn_dataset$InternetService <- recode(churn_dataset$InternetService, "'No'=0; 'DSL'=1;'Fiber optic'=2")
churn_dataset$OnlineSecurity <- recode(churn_dataset$OnlineSecurity, "'No'=0; 'Yes'=1;'No internet service'=2")
churn_dataset$OnlineBackup <- recode(churn_dataset$OnlineBackup, "'No'=0; 'Yes'=1;'No internet service'=2")
churn_dataset$DeviceProtection <- recode(churn_dataset$DeviceProtection, "'No'=0; 'Yes'=1;'No internet service'=2")
churn_dataset$TechSupport <- recode(churn_dataset$TechSupport, "'No'=0; 'Yes'=1;'No internet service'=2")
churn_dataset$StreamingTV <- recode(churn_dataset$StreamingTV, "'No'=0; 'Yes'=1;'No internet service'=2")
churn_dataset$StreamingMovies <- recode(churn_dataset$StreamingMovies, "'No'=0; 'Yes'=1;'No internet service'=2")
churn_dataset$Contract <- recode(churn_dataset$Contract, "'Month-to-month'=0; 'One year'=1;'Two year'=2")
churn_dataset$PaperlessBilling <- recode(churn_dataset$PaperlessBilling, "'Yes'=1; 'No'=0")
churn_dataset$PaymentMethod <- recode(churn_dataset$PaymentMethod, "'Electronic check'=1; 'Mailed check'=2;'Bank transfer (automatic)'=3; 'Credit card (automatic)'=4")

#converting column to factor
churn_dataset[, 'Churn'] <- lapply(churn_dataset[, 'Churn'], factor)

## Converting factor variables
churn_dataset$gender <- factor(churn_dataset$gender)
churn_dataset$SeniorCitizen <- factor(churn_dataset$SeniorCitizen )
churn_dataset$Partner <- factor(churn_dataset$Partner)
churn_dataset$Dependents <- factor(churn_dataset$Dependents)
churn_dataset$PhoneService <- factor(churn_dataset$PhoneService)
churn_dataset$MultipleLines <- factor(churn_dataset$MultipleLines)
churn_dataset$InternetService <- factor(churn_dataset$InternetService)
churn_dataset$OnlineSecurity <- factor(churn_dataset$OnlineSecurity)
churn_dataset$OnlineBackup <- factor(churn_dataset$OnlineBackup)
churn_dataset$DeviceProtection <- factor(churn_dataset$DeviceProtection)
churn_dataset$TechSupport <- factor(churn_dataset$TechSupport)
churn_dataset$StreamingTV <- factor(churn_dataset$StreamingTV)
churn_dataset$StreamingMovies <- factor(churn_dataset$StreamingMovies)
churn_dataset$Contract <- factor(churn_dataset$Contract)
churn_dataset$PaperlessBilling <- factor(churn_dataset$PaperlessBilling)
churn_dataset$PaymentMethod <- factor(churn_dataset$PaymentMethod)


#####################################################################
######################### Modeling ##################################
#####################################################################

# For training and testing purpose,we will divide the data in 75-25 ratio

set.seed(111)
train.index <- createDataPartition(y = churn_dataset$Churn, p = 0.75, list = FALSE, times = 1)
train <- churn_dataset[train.index,]
test <- churn_dataset[ - train.index,]


############## Logistic Regression Model  ###################

# Using forward (step up) selection procedure to find most suitable features. Lowest AIC will indicate good model

fullMod = glm(Churn ~ ., data = train, family = binomial)

summary(fullMod)

intMod <- glm(Churn ~ 1, data = train, family = binomial)

summary(intMod)

fwdSelection = step(intMod, scope = list(lower = formula(intMod), upper = formula(fullMod)), direction = "forward")

formula(fwdSelection)
summary(fwdSelection)

# Using selected variables for LR Modeling

logic_reg <- glm(Churn ~ Contract + InternetService + tenure +MultipleLines + PaymentMethod + PaperlessBilling + TotalCharges + OnlineSecurity + TechSupport + SeniorCitizen + StreamingMovies + StreamingTV +MonthlyCharges, data = train, family = binomial)

summary(logic_reg)

glm.pred <- predict(logic_reg, test, type = 'response')
CrossTable(test$Churn, glm.pred > 0.5, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('ACTUAL', 'PREDICTED'))

glmpred <- prediction(glm.pred, test$Churn)
glmperf <- performance(glmpred, 'tpr', 'fpr')

#Model Performance by using a cutoff of 0.5
pred.churn <- factor(ifelse(glm.pred >= 0.50, "Yes", "No"))
actual.churn <- factor(ifelse(test$Churn==1,"Yes","No"))
table(actual.churn,pred.churn)
cutoff.churn <- factor(ifelse(glm.pred >=0.50, "Yes", "No"))
lr_conf <- confusionMatrix(cutoff.churn, actual.churn, positive = "Yes")
lr_accuracy <- lr_conf$overall[1]
lr_sensitivity <- lr_conf$byClass[1]
lr_specificity <- lr_conf$byClass[2]
lr_accuracy
lr_sensitivity
lr_specificity

#Tweaking the cutoff to 0.4 and re-evaluating performance
pred.churn <- factor(ifelse(glm.pred >= 0.40, "Yes", "No"))
actual.churn <- factor(ifelse(test$Churn==1,"Yes","No"))
table(actual.churn,pred.churn)
cutoff.churn <- factor(ifelse(glm.pred >=0.40, "Yes", "No"))
lr_conf <- confusionMatrix(cutoff.churn, actual.churn, positive = "Yes")
lr_accuracy <- lr_conf$overall[1]
lr_sensitivity <- lr_conf$byClass[1]
lr_specificity <- lr_conf$byClass[2]
lr_accuracy
lr_sensitivity
lr_specificity

#AUC
glm.perf <- performance(glmpred, measure = 'auc')
glm.perf <- glm.perf@y.values[[1]]
print(glm.perf)

#Summarizing the results of model
results <- data_frame(Method = "Logistic_Regression", Accuracy=lr_accuracy,Sensitivity = lr_sensitivity,Specificity=lr_specificity,AUC=glm.perf)
kable(results,align=rep("c",3)) %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:5,bold=T,border_right = T,color='royalblue')


########################## LDA Modeling #################

#Training LDA model
lda = trainControl(method = "none")
lda_fit = train(Churn ~ ., data = train, method = "lda", metric = "Accuracy", preProc = c("center", "scale"), trControl = lda)

# Running the model on test data
ldapred = predict(lda_fit, newdata = test, type="raw")

#Confusion Matrix
lda_conf<-confusionMatrix(table(ldapred, test$Churn))

CrossTable(test$Churn, ldapred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('ACTUAL', 'PREDICTED'))

# Prediction, probability
ldapred<- predict(lda_fit,test,type = "prob")
lda.pred.prob.val <- prediction(ldapred[,2],test$Churn)
lda.pred.prob.perf <- performance(lda.pred.prob.val,"auc")


#Model Performance
lda_accuracy <- lda_conf$overall[1]
lda_sensitivity <- lda_conf$byClass[1]
lda_specificity <- lda_conf$byClass[2]
lda_accuracy
lda_sensitivity
lda_specificity

#Summarizing the results
results <- bind_rows(results, data_frame(Method = "LDA Model", Accuracy=lda_accuracy,Sensitivity = lda_sensitivity,Specificity=lda_specificity,AUC=(lda.pred.prob.perf@y.values[[1]])))

kable(results,align=rep("c",3)) %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:5,bold=T,border_right = T,color='royalblue')

#################### Decision Tree  ################################


#Decision Tree

set.seed(222)

options(repr.plot.width = 6, repr.plot.height = 4)

# Training the model
tree_fit <- rpart(Churn ~ ., data = train, method = "class")

# Plotting decision tree
rpart.plot(tree_fit,type = 4,extra = 2,under = TRUE,fallen.leaves = F)

# Prediction
tree_pred <- predict(tree_fit,test, type = "class")

#Confusion Matrix
dt_conf<-confusionMatrix(table(tree_pred,test$Churn))

# Prediction/Probability
treepred <- predict(tree_fit,test,type = "prob")
tree.pred.prob.val <- prediction(treepred[,2],test$Churn)
tree.pred.prob.perf <- performance(tree.pred.prob.val,"auc")


#Model Performance
dt_accuracy <- dt_conf$overall[1]
dt_sensitivity <- dt_conf$byClass[1]
dt_specificity <- dt_conf$byClass[2]
dt_accuracy
dt_sensitivity
dt_specificity

#Summarizing the results
results <- bind_rows(results, data_frame(Method = "Decision Tree Model", Accuracy=dt_accuracy,Sensitivity = dt_sensitivity,Specificity=dt_specificity,AUC=(tree.pred.prob.perf@y.values[[1]])))

kable(results,align=rep("c",3)) %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:5,bold=T,border_right = T,color='royalblue')

################### Random Forest Model ####################


# Random Forest is an ensemble learning  based classification technique and commonly used for predictive modeling. 

library(randomForest)
set.seed(333)
rf <- randomForest(Churn ~ ., data = train, ntree = 1000, importance = T)

print(rf)
importance(rf)

varImpPlot(rf, type = 1, pch = 19, col = 1, cex = 1.0, main = "Mean Decrease Accuracy Plot")
abline(v = 35, col = "blue")

varImpPlot(rf, type = 2, pch = 19, col = 1, cex = 1.0, main = "Mean Decrease Gini Plot")

rfpred <- predict(rf, test[, -20], type = 'response')

rf_conf<-confusionMatrix(table(rfpred, test$Churn))

CrossTable(test$Churn, rfpred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('ACTUAL', 'PREDICTED'))

# Probability/AUC
rfpred<- predict(rf,test,type = "prob")
rf.pred.prob.val <- prediction(rfpred[,2],test$Churn)
rf.pred.prob.perf <- performance(rf.pred.prob.val,"auc")

#Model Performance
rf_accuracy <- rf_conf$overall[1]
rf_sensitivity <- rf_conf$byClass[1]
rf_specificity <- rf_conf$byClass[2]
rf_accuracy
rf_sensitivity
rf_specificity

#Summarizing the results
results <- bind_rows(results, data_frame(Method = "Random Forest Model", Accuracy=rf_accuracy,Sensitivity = rf_sensitivity,Specificity=rf_specificity,AUC=(rf.pred.prob.perf@y.values[[1]])))

kable(results,align=rep("c",3)) %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:5,bold=T,border_right = T,color='royalblue')

############### Support Vector Machine (SVM) Model  ###################


# Will take long time to execute. The aim is to find best values for parameters

set.seed(444)
svm <- tune.svm(Churn ~ ., data = train, 
                seq(0.5, 0.9, by = 0.1), cost = seq(100, 1000, by = 100), 
                kernel = "radial", tunecontrol = tune.control(cross = 10))

print(svm)
summary(svm)
svm$performances

# Choosing the best SVM model
svmfit <- svm$best.model

svmpred<- predict(svmfit,test,type = "prob")

svm_conf<-confusionMatrix(table(svmpred, test$Churn))

CrossTable(test$Churn, svmpred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('ACTUAL', 'PREDICTED'))

# Probability/AUC

svmpred <- as.numeric(levels(svmpred))[svmpred]
svm.pred.prob.val <- prediction(svmpred, test$Churn)
svm.pred.prob.perf <- performance(svm.pred.prob.val, measure = 'auc')


#Model Performance
svm_accuracy <- svm_conf$overall[1]
svm_sensitivity <- svm_conf$byClass[1]
svm_specificity <- svm_conf$byClass[2]
svm_accuracy
svm_sensitivity
svm_specificity

#Summarizing the results
results <- bind_rows(results, data_frame(Method = "SVM Model", Accuracy=svm_accuracy,Sensitivity = svm_sensitivity,Specificity=svm_specificity,AUC=(svm.pred.prob.perf@y.values[[1]])))

kable(results,align=rep("c",3)) %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:5,bold=T,border_right = T,color='royalblue')

################# Results #######################################

#Plotting and comparing ROC curves for all models
plot(glmperf, col = 'green', lwd = 2.5)
plot(performance(lda.pred.prob.val, "tpr", "fpr"),  add = TRUE, col = 'royalblue', lwd = 2.5)
plot(performance(tree.pred.prob.val, "tpr", "fpr"),  add = TRUE, col = 'orange', lwd = 2.5)
plot(performance(rf.pred.prob.val, "tpr", "fpr"),  add = TRUE, col = 'black', lwd = 2.5)
plot(performance(svm.pred.prob.val, "tpr", "fpr"),  add = TRUE, col = 'red', lwd = 2.5)
abline(a = 0, b = 1, col = 'black', lwd = 2.5, lty = 2)
title('ROC Curve')
legend("bottomright", c("Logistic_Refression", "Linear_Discriminant_Analysis", "Decision_Tree","Random_Forest","SVM"), lty = c(1, 1, 1,1,1), lwd = c(1.4, 1.4, 1.4,1.4,1.4), col = c('green','royalblue', 'black', 'blue','red'))

#Model Comparison
kable(results,align=rep("c",3)) %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:5,bold=T,border_right = T,color='royalblue')


####################################################################

results$Specificity<-NULL
results$Accuracy<-NULL

#Conclusion
kable(results,align=rep("c",3)) %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:3,bold=T,border_right = T,color='royalblue')
