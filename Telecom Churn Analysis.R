
##Load in Data
setwd("C:/Users/patjw/Downloads/Tech Stuff")
df <- read.csv("churn.csv")

install.packages("dplyr")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("ggthemes")
install.packages("caret")
install.packages("MASS")
install.packages("randomForest")
install.packages("party")
install.packages("ROCR")
install.packages("caTools")

library(dplyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(ROCR)
library(caTools)


## Data Cleaning
sapply(df, function(x) sum(is.na(x))) ## No NA Values
sapply(df, function(x) sum(is.null(x))) ## No Null values 

summary(df)
str(df)

### Break Account Length into 3 Month Intervals
df$accountlength_group <- ifelse(df$accountlength <=90, "0-3 Months", ifelse(df$accountlength >90 & df$accountlength <= 180, "3-6 Months", "More than 6 Months"))
df$accountlength_group <- as.factor(df$accountlength_group)
head(df$accountlength_group)
colnames(df)

##Remove unecessary Columns
dftrim <- subset(df, select = -c(accountlength))

##Correlation between Numeric Variables
numeric.var <- sapply(dftrim, is.numeric)
corr.matrix <- cor(dftrim[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

cor(dftrim[,numeric.var])

## Need to remove highly correlated variables (don't need calls, minutes, and charge by time, just charge)
dftrim <- subset(dftrim, select = c(churn, internationalplan, voicemailplan, numbervmailmessages, totaldaycharge, totalevecharge, totalnightcharge, totalintlcharge, numbercustomerservicecalls, accountlength_group))

## EDA

## Plot Features
bold_axis <- element_text(face = "bold", color = "black", size = 20)
axis_text <- element_text(face = "bold", size = 14)

International_Plot <- ggplot(dftrim, aes(x=internationalplan)) + ggtitle("Breakdown of International Plans") + xlab("International Plan") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill = "#f9a65a") + ylab("Percentage of Plans") 
International_Plot + theme(axis.text = axis_text) + theme(title = bold_axis)   ## 90.54% do not have international plans
table(dftrim$internationalplan) / nrow(dftrim)

Voicemail_Plot <- ggplot(dftrim, aes(x=voicemailplan)) + ggtitle("Breakdown of Voicemail Plans") + xlab("Voicemail Plan") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill = "#9e66ab") + ylab("Percentage of Plans")
Voicemail_Plot + theme(axis.text = axis_text) + theme(title = bold_axis) ## 26.46% have voicemail plans
table(dftrim$voicemailplan) / nrow(dftrim)

servicecall_Plot <- ggplot(dftrim, aes(x=numbercustomerservicecalls)) + geom_histogram(fill= "purple", alpha = .5, binwidth = .5)
servicecall_Plot + xlab("Count of Calls") + ylab("Count of Customers") + labs(title = "Histogram of Service Calls") + xlim(c(-1,10)) + theme(title = bold_axis) + theme(axis.text = axis_text) ## Majority of people have called 2 or fewer times
table(dftrim$numbercustomerservicecalls) / nrow(dftrim)

## Churn of voicemail Plan
Voicemailchurn_Plot <- ggplot(dftrim, aes(x=voicemailplan)) + geom_bar(aes(fill = factor(churn), y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage of Plans")+ xlab("Voicemail Plan") + ggtitle("Churn by Plan Type - Voicemail")
Voicemailchurn_Plot + theme(axis.text = axis_text) + theme(title = bold_axis)### Lower prportion of people with voicemail end up leaving

##Numerical Tables of Voicemail plan and Churn
table(dftrim$voicemailplan, dftrim$churn)  
table(dftrim$voicemailplan, dftrim$churn) / nrow(dftrim)
605 / (605+3072) ## 16.45% of those who did not have a voicemail plan churned
102 / (102 + 1221) ## 7.71% of those who had a voicemail plan churned
### Voicemail plan is effective in maintaining customers

str(dftrim)

### Churn of International Plan
Internationalchurn_Plot <- ggplot(dftrim, aes(x=internationalplan)) + geom_bar(aes(fill = factor(churn), y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage of Plans") + xlab("International Plan") + ggtitle("Churn by Plan Type - International")
Internationalchurn_Plot + theme(axis.text = axis_text) + theme(title = bold_axis)

##Numerical Tables of Voicemail plan and Churn
table(dftrim$internationalplan, dftrim$churn)  
table(dftrim$internationalplan, dftrim$churn) / nrow(dftrim)  
### it is very evident that international plan users frequently churned, 42.07% to be exact as opposed to
199 / (274 + 199) ## 42.07% of international plan users churned
508 / (4019 + 508) ## 11.22% of non international plan users churned
## International Plans lead to more churn

### Churn by Accountlength_group Plot
Accountlengthchurn_Plot <- ggplot(dftrim, aes(x=accountlength_group)) + ggtitle("Churn by Plan Tenure") + geom_bar(aes(fill = factor(churn), y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage of Plans") + xlab("Account Tenure")
Accountlengthchurn_Plot + theme(axis.text = axis_text) + theme(title = bold_axis)

##Numerical Tables of Voicemail plan and Churn
table(dftrim$accountlength_group, dftrim$churn)  
table(dftrim$accountlength_group, dftrim$churn) / nrow(dftrim)  

264 / (264 + 1778)  ## 12.93% of plans within 0-3 Month Range Churned
426 / (426 + 2412)  ## 15.01% of plans within 3-6 months churned
17 / (103 + 17)  ## 14.17% of plans greater than 6 months churned
### No Clear Relationship between tenure and churn

## Churn by Number of Service Calls
servicecall_Plot <- ggplot(dftrim, aes(x=numbercustomerservicecalls)) + geom_histogram(aes(fill= factor(churn), alpha = .5, binwidth = .5))
servicecall_Plot + xlab("Count of Calls") + ylab("Count of Customers") + labs(title = "Churn by Number of Service Calls") + xlim(c(-1,10)) + theme(title = bold_axis) + theme(axis.text = axis_text)

table(dftrim$numbercustomerservicecalls, dftrim$churn) / nrow(dftrim)

### Train Test Split
set.seed(101)
Sample <- sample.split(dftrim, SplitRatio = 0.7)
dftrain <- subset(dftrim, Sample == TRUE)
dftest <- subset(dftrim, Sample == FALSE)

##Build Model
LogModel <- glm(churn ~ ., family = binomial(link = "logit"), data = dftrain)
print(summary(LogModel))

##McFadden's R^2 Value
install.packages("blorr")
library(blorr)
blr_rsq_mcfadden(LogModel) ## .21. A value above between .2-.4 is deemed an "Excellent Fit"

## Feature Analysis
anova(LogModel, test = "Chisq")
### internationalplan, totaldaycharge, numbercustomerservicecalls, voicemailplan have most profound impact on Residual Deviance 

##Predictions with Model
dftest$churn <- as.character(dftest$churn)
dftest$churn[dftest$churn == "No"] <- 0
dftest$churn[dftest$churn == "Yes"] <- 1

set.seed(101)
threshold <- 0.5
ChurnPredictionprobability <- predict(LogModel, newdata = dftest,type="response")
ChurnPrediction <- ifelse(ChurnPredictionprobability > threshold,1,0)
misclassificationError <- mean(ChurnPrediction != dftest$churn)
1 - misclassificationError  ##86%

### Confusion Matrix
print("Confusion Matrix for Logistic Regression")
table(ChurnPrediction, dftest$churn > threshold)[2:1, 2:1]

TP <- ifelse(ChurnPrediction > threshold & dftest$churn == 1, 1, 0)
table(TP)
FP <- ifelse(ChurnPrediction > threshold & dftest$churn == 0, 1, 0)
table(FP)
FN <- ifelse(ChurnPrediction < threshold & dftest$churn == 1, 1, 0)
table(FN)
TN <- ifelse(ChurnPrediction < threshold & dftest$churn == 0, 1, 0)
table(TN)

table(dftest$churn)

##Cost Assumptions and Calculations
Retention_Cost <- 60
Acquisition_Cost <- 300

Cost_Model <- ((sum(TP) + sum(FP)) * Retention_Cost) + (sum(FN) * Acquisition_Cost)
Cost_Model
((39 + 34) * (60))  + (173 *300)

##Measures of Recall, Precision, Accuracy
Recall <- sum(TP) / ((sum(TP) + sum(FN)))
Recall  ## 18.40%

Precision <- sum(TP) / ((sum(TP) + sum(FP)))
Precision  ## 53.4%

Accuracy <- (sum(TP) + sum(TN)) / nrow(dftest)
Accuracy  ## 86%

###Predictions
install.packages("pROC") ## if not downloaded earlier
library(pROC)

hist(ChurnPredictionprobability)

install.packages("ROCR") ## if not downloaded earlier
library(ROCR)

set.seed(101)
predictedchurn <- prediction(ChurnPredictionprobability, dftest$churn)
eval <- performance(predictedchurn, "acc")
plot(eval, main = "Prediction Accuracy by Classification Threshold", xlab = "Threshold") + theme(title = bold_axis) + theme(axis.text = axis_text) 
## Serves to try to visualize accuracy by classification threshold

##ROC Curve
ROC <- performance(predictedchurn, "tpr", "fpr")
plot(ROC, colorize = T, main = "ROC Curve", ylab = "TPR", xlab = "FPR")
abline(a = 0, b = 1)

## Area under Curve (AUC)
AUC <- performance(predictedchurn, "auc")
AUC <- unlist(slot(AUC, "y.values"))
AUC

## Tuning the Model With Cutoff Point 

##Tuned Threshold
Tuned_Threshold <- .17

set.seed(101)
TunedChurnPrediction <- ifelse(ChurnPredictionprobability >= Tuned_Threshold, 1, 0)
TunedmisclassificationError <- mean(TunedChurnPrediction != dftest$churn)
1 - TunedmisclassificationError  #79%

### Confusion Matrix
print("Confusion Matrix for Tuned Logistic Regression")
table(TunedChurnPrediction, dftest$churn > Tuned_Threshold)[2:1, 2:1]

TP2 <- ifelse(TunedChurnPrediction > Tuned_Threshold & dftest$churn == 1, 1, 0)
table(TP2)
FP2 <- ifelse(TunedChurnPrediction > Tuned_Threshold & dftest$churn == 0, 1, 0)
table(FP2)
FN2 <- ifelse(TunedChurnPrediction < Tuned_Threshold & dftest$churn == 1, 1, 0)
table(FN2)
TN2 <- ifelse(TunedChurnPrediction < Tuned_Threshold & dftest$churn == 0, 1, 0)
table(TN2)

##Measures of Recall, Precision, Accuracy
Tuned_Recall <- sum(TP2) / ((sum(TP2) + sum(FN2)))
Tuned_Recall  ## 75.47%

Tuned_Precision <- sum(TP2) / ((sum(TP2) + sum(FP2)))
Tuned_Precision  ## 38.28%

Tuned_Accuracy <- (sum(TP2) + sum(TN2)) / nrow(dftest)
Tuned_Accuracy  ## 79.33%

##Calculating Cost of Tuned Model
Tuned_Cost_Model <- ((sum(TP2) + sum(FP2)) * Retention_Cost) + (sum(FN2) * Acquisition_Cost)
Tuned_Cost_Model

## Cost Savings by Adopting Tuned Model
Cost_Model - Tuned_Cost_Model
(Cost_Model - Tuned_Cost_Model) / Cost_Model * 100  ## 27.72% Savings

## Cost Savings compared to no Model
CostofnoPrediction - Tuned_Cost_Model
(CostofnoPrediction - Tuned_Cost_Model) / CostofnoPrediction * 100 ## 36.04% Savings

## Cost of Making No Predictions
CostofnoPrediction <- sum(dftest$churn == 1) * Acquisition_Cost
CostofnoPrediction

## Model with 100% TPR & 0% FPR
OptimizedCost <- sum(dftest$churn ==1) * Retention_Cost
OptimizedCost

## Plotting Cost breakdown
dfCostModels <- data.frame("Model_Name" = c("No_Prediction", "Cost_Model", "Tuned_Cost_Model", "Optimized_Cost"), "Cost" = c(CostofnoPrediction, Cost_Model, Tuned_Cost_Model, OptimizedCost))
str(dfCostModels)

library(forcats)

PlotCostModels <- ggplot(dfCostModels, aes(fct_rev(fct_reorder(Model_Name, Cost)), Cost)) + geom_col(fill = "#599ad3") + labs(x = "Model Name", title = "Cost by Business Scenario", fontface = "bold")
PlotCostModels + geom_text(aes(label = Cost, vjust = -.15, hjust = .5, size = 24), color = "#9e66ab", fontface = "bold") + theme(title = bold_axis) + theme(axis.text = axis_text) 

### Random Forest

##Building Model
set.seed(101)
lowelojungle <- randomForest(churn ~., data = dftrain, ntree = 500, mtry = 2)
print(lowelojungle)

## Forest Predictions
set.seed(101)
jgpathpredict <- predict(lowelojungle, newdata = dftest)
table(observed = dftest$churn, predicted = jgpathpredict)

TPjg <- ifelse(jgpathpredict == "Yes" & dftest$churn == 1, 1, 0)
table(TPjg)
FPjg <- ifelse(jgpathpredict == "Yes" & dftest$churn == 0, 1, 0)
table(FPjg)
FNjg <- ifelse(jgpathpredict == "No" & dftest$churn == 1, 1, 0)
table(FNjg)
TNjg <- ifelse(jgpathpredict == "No" & dftest$churn == 0, 1, 0)
table(TNjg)

##Recall, Precision, Accuracy 
jg_Recall <- sum(TPjg) / ((sum(TPjg) + sum(FNjg)))
jg_Recall  ## 63.21%

jg_Precision <- sum(TPjg) / ((sum(TPjg) + sum(FPjg)))
jg_Precision  ## 96.40%

jg_Accuracy <- (sum(TPjg) + sum(TNjg)) / nrow(dftest)
jg_Accuracy  ## 94.45%

## Random Forest Error Rate
plot(lowelojungle) ## Will use roughly 200 Trees
colnames(dftrain)

##Tuning the Model
set.seed(101)
coachingjg <- tuneRF(dftrain[, -1], dftrain[, 1], stepFactor = 0.5, plot = TRUE, ntreeTry = 200, trace = TRUE, improve =.05)
##OOB Error is lowest when mtry = 3 (number of variables tested)

set.seed(101)
reformedjg <- randomForest(churn ~., data = dftrain, ntree = 200, mtry = 3, importance = TRUE, proximity = TRUE)
print(reformedjg) # OOB error decreased from 6.71 to 5.8%

set.seed(101)
reformed_jgpathpredict <- predict(reformedjg, newdata = dftest)
table(observed = dftest$churn, predicted = reformed_jgpathpredict)

TPreformedjg <- ifelse(reformed_jgpathpredict == "Yes" & dftest$churn == 1, 1, 0)
table(TPreformedjg)
FPreformedjg <- ifelse(reformed_jgpathpredict == "Yes" & dftest$churn == 0, 1, 0)
table(FPreformedjg)
FNreformedjg <- ifelse(reformed_jgpathpredict == "No" & dftest$churn == 1, 1, 0)
table(FNreformedjg)
TNreformedjg <- ifelse(reformed_jgpathpredict == "No" & dftest$churn == 0, 1, 0)
table(TNreformedjg)

##Recall, Precision, Accuracy 
Reformedjg_Recall <- sum(TPreformedjg) / ((sum(TPreformedjg) + sum(FNreformedjg)))
Reformedjg_Recall  ## 69.81%

Reformedjg_Precision <- sum(TPreformedjg) / ((sum(TPreformedjg) + sum(FPreformedjg)))
Reformedjg_Precision  ## 91.93%

Reformedjg_Accuracy <- (sum(TPreformedjg) + sum(TNreformedjg)) / nrow(dftest)
Reformedjg_Accuracy  ## 94.87%

##Recall increased from 63 to 70%
## Precision decreased from 96 to 92%
##Accuracy increased from 94 to 95%

## Random Forest Feature Importance
varImpPlot(reformedjg, sort = TRUE, n.var = 9, main = "Feature Importance")
importance(reformedjg)

### Adding a continuous random variable to assess importance
library(dplyr)

dftrainplusrandom <- dftrain %>% mutate(random = sample(100, size = nrow(dftrain), replace = TRUE))
colnames(dftrainplusrandom)
head(dftrainplusrandom$random)

##Retrain model on new Data
set.seed(101)
reformedjgplusrandom <- randomForest(churn ~., data = dftrainplusrandom, ntree = 200, mtry = 3, importance = TRUE, proximity = TRUE)
print(reformedjgplusrandom)

varImpPlot(reformedjgplusrandom, sort = TRUE, n.var = 10, main = "Feature Importance")
importance(reformedjgplusrandom)

### Summary

## Cost Breakdown Using Random Forest Model
RandomForest_Cost_Model <- ((sum(TPreformedjg) + sum(FPreformedjg)) * Retention_Cost) + (sum(FNreformedjg) * Acquisition_Cost)
RandomForest_Cost_Model  ##28,660

##Plotting 
dfCostModels_updated <- data.frame("Model_Name" = c("No_Prediction", "Cost_Model", "Tuned_Cost_Model", "RF_Cost_Model", "Optimized_Cost"), "Cost" = c(CostofnoPrediction, Cost_Model, Tuned_Cost_Model, RandomForest_Cost_Model, OptimizedCost))

PlotCostModels <- ggplot(dfCostModels_updated, aes(fct_rev(fct_reorder(Model_Name, Cost)), Cost)) + geom_col(fill = "#599ad3") + labs(x = "Model Name", title = "Cost by Business Scenario", fontface = "bold")
PlotCostModels + geom_text(aes(label = Cost, vjust = -.15, hjust = .5, size = 24), color = "#9e66ab", fontface = "bold") + theme(title = bold_axis) + theme(axis.text = axis_text) 

###Cost Savings Improvement from Base Model
CostofnoPrediction - RandomForest_Cost_Model
(CostofnoPrediction - RandomForest_Cost_Model) / CostofnoPrediction * 100 ##34,740 or 54.62%

###Balancing the Data
install.packages("ROSE")
library(ROSE)

# balanced data set with both over and under sampling
set.seed(101)
balanced_data_train <- ovun.sample(churn~., data=dftrain,
                                   N=nrow(dftrain), p=0.5,
                                   seed=1, method="both")$data

table(balanced_data_train$churn)

## Balance Test Set as well
set.seed(101)
balanced_data_test <- ovun.sample(churn~., data=dftest,
                                  N=nrow(dftest), p=0.5,
                                  seed=1, method="both")$data
table(balanced_data_test$churn)

## Logistic Regression with New Data
LogModelbalanced <- glm(churn ~ ., famil = binomial(link = "logit"), data = balanced_data_train)
print(summary(LogModelbalanced))

##McFadden's R^2
blr_rsq_mcfadden(LogModelbalanced)

## Feature Analysis
anova(LogModelbalanced, test = "Chisq")

##Predictions with Model
balanced_data_test$churn <- as.character(balanced_data_test$churn)
balanced_data_test$churn[balanced_data_test$churn == "No"] <- 0
balanced_data_test$churn[balanced_data_test$churn == "Yes"] <- 1

set.seed(101)
threshold <- 0.5
balanced_ChurnPredictionProbability <- predict(LogModelbalanced, newdata = balanced_data_test,type="response")
balanced_ChurnPrediction <- ifelse(balanced_ChurnPredictionProbability > threshold,1,0)
balanced_misclassificationError <- mean(balanced_ChurnPrediction != balanced_data_test$churn)
1 - balanced_misclassificationError # 79%

### Confusion Matrix
print("Confusion Matrix for Logistic Regression")
table(balanced_ChurnPrediction, balanced_data_test$churn > threshold)[2:1, 2:1]

balanced_TP <- ifelse(balanced_ChurnPrediction > threshold & balanced_data_test$churn == 1, 1, 0)
table(balanced_TP)
balanced_FP <- ifelse(balanced_ChurnPrediction > threshold & balanced_data_test$churn == 0, 1, 0)
table(balanced_FP)
balanced_FN <- ifelse(balanced_ChurnPrediction < threshold & balanced_data_test$churn == 1, 1, 0)
table(balanced_FN)
balanced_TN <- ifelse(balanced_ChurnPrediction < threshold & balanced_data_test$churn == 0, 1, 0)
table(balanced_TN)

##Cost Assumptions and Calculations
Retention_Cost <- 60
Acquisition_Cost <- 300

balanced_Cost_Model <- ((sum(balanced_TP) + sum(balanced_FP)) * Retention_Cost) + (sum(balanced_FN) * Acquisition_Cost)
balanced_Cost_Model

##Measures of Recall, Precision, Accuracy
balanced_Recall <- sum(balanced_TP) / ((sum(balanced_TP) + sum(balanced_FN)))
balanced_Recall  ## 80.45%

balanced_Precision <- sum(balanced_TP) / ((sum(balanced_TP) + sum(balanced_FP)))
balanced_Precision  ## 75.96%

balanced_Accuracy <- (sum(balanced_TP) + sum(balanced_TN)) / nrow(balanced_data_test)
balanced_Accuracy  ## 78.67%

##Cost of No Predictions with Balanced Dataset
## Cost of Making No Predictions
balanced_CostofnoPrediction <- sum(balanced_data_test$churn == 1) * Acquisition_Cost
balanced_CostofnoPrediction

### % Benefit
balanced_CostSavings <- (balanced_CostofnoPrediction - balanced_Cost_Model) / balanced_CostofnoPrediction * 100
balanced_CostSavings  #59.27%

### Evaluating Threshold and Prediction Accuracy with Balanced Dataset
hist(balanced_ChurnPredictionProbability)

set.seed(101)
balanced_predictedchurn <- prediction(balanced_ChurnPredictionProbability, balanced_data_test$churn)
balanced_eval <- performance(balanced_predictedchurn, "acc")
plot(balanced_eval, main = "Prediction Accuracy by Classification Threshold - Balanced Data", xlab = "Threshold")
## Serves to try to maximize Accuracy, note that threshold is much more important now with balanced data

##ROC Curve
balanced_ROC <- performance(balanced_predictedchurn, "tpr", "fpr")
plot(balanced_ROC, colorize = T, main = "ROC Curve - Balanced Data", ylab = "TPR", xlab = "FPR") 
abline(a = 0, b = 1)

## Area under Curve (AUC)
balanced_AUC <- performance(balanced_predictedchurn, "auc")
balanced_AUC <- unlist(slot(balanced_AUC, "y.values"))
balanced_AUC

## Tweaking the Balanced Model Threshold 

##Tuned Threshold
balanced_Tuned_Threshold <- .4

set.seed(101)
balanced_TunedChurnPrediction <- ifelse(balanced_ChurnPredictionProbability >= balanced_Tuned_Threshold, 1, 0)
balanced_TunedmisclassificationError <- mean(balanced_TunedChurnPrediction != balanced_data_test$churn)
1 - balanced_TunedmisclassificationError 

### Confusion Matrix
print("Confusion Matrix for Tuned Logistic Regression")
table(balanced_TunedChurnPrediction, balanced_data_test$churn > balanced_Tuned_Threshold)[2:1, 2:1]

balanced_TP2 <- ifelse(balanced_TunedChurnPrediction > balanced_Tuned_Threshold & balanced_data_test$churn == 1, 1, 0)
table(balanced_TP2)
balanced_FP2 <- ifelse(balanced_TunedChurnPrediction > Tuned_Threshold & balanced_data_test$churn == 0, 1, 0)
table(balanced_FP2)
balanced_FN2 <- ifelse(balanced_TunedChurnPrediction < Tuned_Threshold & balanced_data_test$churn == 1, 1, 0)
table(balanced_FN2)
balanced_TN2 <- ifelse(balanced_TunedChurnPrediction < Tuned_Threshold & balanced_data_test$churn == 0, 1, 0)
table(balanced_TN2)

##Measures of Recall, Precision, Accuracy
balanced_Tuned_Recall <- sum(balanced_TP2) / ((sum(balanced_TP2) + sum(balanced_FN2)))
balanced_Tuned_Recall  ## 88.61%

balanced_Tuned_Precision <- sum(balanced_TP2) / ((sum(balanced_TP2) + sum(balanced_FP2)))
balanced_Tuned_Precision  ## 70.71%

balanced_Tuned_Accuracy <- (sum(balanced_TP2) + sum(balanced_TN2)) / nrow(balanced_data_test)
balanced_Tuned_Accuracy  ## 78%

##Calculating Cost of Tuned Model
balanced_Tuned_Cost_Model <- ((sum(balanced_TP2) + sum(balanced_FP2)) * Retention_Cost) + (sum(balanced_FN2) * Acquisition_Cost)
balanced_Tuned_Cost_Model ## 77760

## Cost Savings by Adopting Tuned Model
balanced_Cost_Model - balanced_Tuned_Cost_Model
(balanced_Cost_Model - balanced_Tuned_Cost_Model) / balanced_Cost_Model * 100  ## 10.50% Savings, or $9,120

## Cost of Making No Predictions
balanced_CostofnoPrediction <- sum(balanced_data_test$churn == 1) * Acquisition_Cost
balanced_CostofnoPrediction ##213300

##Cost Savings of Balanced Tuned Model compared to No Prediction
balanced_CostofnoPrediction - balanced_Tuned_Cost_Model  ## 135,540
(balanced_CostofnoPrediction - balanced_Tuned_Cost_Model) / balanced_CostofnoPrediction * 100 ## 63.54%

## Model with 100% TPR & 0% FPR
balanced_OptimizedCost <- sum(balanced_data_test$churn ==1) * Retention_Cost
balanced_OptimizedCost ## 42,660

## Plotting Cost breakdown
balanced_dfCostModels <- data.frame("Model_Name" = c("No_Prediction", "Cost_Model", "Tuned_Cost_Model", "Optimized_Cost"), "Cost" = c(balanced_CostofnoPrediction, balanced_Cost_Model, balanced_Tuned_Cost_Model, balanced_OptimizedCost))
str(balanced_dfCostModels)

library(forcats)

balanced_PlotCostModels <- ggplot(balanced_dfCostModels, aes(fct_rev(fct_reorder(Model_Name, Cost)), Cost)) + geom_col(fill = "#599ad3") + labs(x = "Model Name", title = "Cost by Business Scenario - Balanced Data", fontface = "bold")
balanced_PlotCostModels + geom_text(aes(label = Cost, vjust = -.15, hjust = .5, size = 24), color = "#9e66ab", fontface = "bold") + theme(title = bold_axis) + theme(axis.text = axis_text) 

### Random Forest With Balanced Data

##Building Model
set.seed(101)
balanced_lowelojungle <- randomForest(churn ~., data = balanced_data_train, ntree = 200, mtry = 3)
print(balanced_lowelojungle)

## Forest Predictions
set.seed(101)
balanced_jgpathpredict <- predict(balanced_lowelojungle, newdata = balanced_data_test)
table(observed = balanced_data_test$churn, predicted = balanced_jgpathpredict)

balanced_TPjg <- ifelse(balanced_jgpathpredict == "Yes" & balanced_data_test$churn == 1, 1, 0)
table(balanced_TPjg)
balanced_FPjg <- ifelse(balanced_jgpathpredict == "Yes" & balanced_data_test$churn == 0, 1, 0)
table(balanced_FPjg)
balanced_FNjg <- ifelse(balanced_jgpathpredict == "No" & balanced_data_test$churn == 1, 1, 0)
table(balanced_FNjg)
balanced_TNjg <- ifelse(balanced_jgpathpredict == "No" & balanced_data_test$churn == 0, 1, 0)
table(balanced_TNjg)

##Recall, Precision, Accuracy 
balanced_jg_Recall <- sum(balanced_TPjg) / ((sum(balanced_TPjg) + sum(balanced_FNjg)))
balanced_jg_Recall  ## 79.61%

balanced_jg_Precision <- sum(balanced_TPjg) / ((sum(balanced_TPjg) + sum(balanced_FPjg)))
balanced_jg_Precision  ## 91.88%

balanced_jg_Accuracy <- (sum(balanced_TPjg) + sum(balanced_TNjg)) / nrow(balanced_data_test)
balanced_jg_Accuracy  ## 87%

## Random Forest Error Rate
plot(balanced_lowelojungle) ## begins to level off around 100-150 trees, going to stick with 200

colnames(balanced_data_train)

##Tuning the Model
set.seed(101)
balanced_coachingjg <- tuneRF(balanced_data_train[, -1], balanced_data_train[, 1], stepFactor = 0.5, plot = TRUE, ntreeTry = 200, trace = TRUE, improve =.05)
##OOB Error is lowest at 6 variables

##Tuned Model with balanced Data
set.seed(101)
reformedbalancedjg <- randomForest(churn ~., data = balanced_data_train, ntree = 200, mtry = 6, importance = TRUE, proximity = TRUE)
print(reformedbalancedjg)

#Predictions
set.seed(101)
reformedbalanced_jgpathpredict <- predict(reformedbalancedjg, newdata = balanced_data_test)
table(observed = balanced_data_test$churn, predicted = reformedbalanced_jgpathpredict)

reformedbalanced_TPjg <- ifelse(reformedbalanced_jgpathpredict == "Yes" & balanced_data_test$churn == 1, 1, 0)
table(reformedbalanced_TPjg)
reformedbalanced_FPjg <- ifelse(reformedbalanced_jgpathpredict == "Yes" & balanced_data_test$churn == 0, 1, 0)
table(reformedbalanced_FPjg)
reformedbalanced_FNjg <- ifelse(reformedbalanced_jgpathpredict == "No" & balanced_data_test$churn == 1, 1, 0)
table(reformedbalanced_FNjg)
reformedbalanced_TNjg <- ifelse(reformedbalanced_jgpathpredict == "No" & balanced_data_test$churn == 0, 1, 0)
table(reformedbalanced_TNjg)

##Recall, Precision, Accuracy 
reformedbalanced_jg_Recall <- sum(reformedbalanced_TPjg) / ((sum(reformedbalanced_TPjg) + sum(reformedbalanced_FNjg)))
reformedbalanced_jg_Recall  ## 80.31%

reformedbalanced_jg_Precision <- sum(reformedbalanced_TPjg) / ((sum(reformedbalanced_TPjg) + sum(reformedbalanced_FPjg)))
reformedbalanced_jg_Precision  ## 92.85%

reformedbalanced_jg_Accuracy <- (sum(reformedbalanced_TPjg) + sum(reformedbalanced_TNjg)) / nrow(balanced_data_test)
reformedbalanced_jg_Accuracy  ## 88%

## 1% increase in precision and accuracy

# Cost Breakdown Using Random Forest Model
reformedbalanced_RandomForest_Cost_Model <- ((sum(reformedbalanced_TPjg) + sum(reformedbalanced_FPjg)) * Retention_Cost) + (sum(balanced_FNjg) * Acquisition_Cost)
reformedbalanced_RandomForest_Cost_Model

##Cost Savings
balanced_CostofnoPrediction - reformedbalanced_RandomForest_Cost_Model
(balanced_CostofnoPrediction - reformedbalanced_RandomForest_Cost_Model) / balanced_CostofnoPrediction * 100 #62.31%

## Plotting Cost breakdown
balanced_dfCostModels2 <- data.frame("Model_Name" = c("No_Prediction", "Reg_Model", "Tuned_Reg_Model", "RF_Cost_Model", "Optimized_Cost"), "Cost" = c(balanced_CostofnoPrediction, balanced_Cost_Model, balanced_Tuned_Cost_Model, reformedbalanced_RandomForest_Cost_Model, balanced_OptimizedCost))
str(balanced_dfCostModels2)

balanced_PlotCostModels2 <- ggplot(balanced_dfCostModels2, aes(fct_rev(fct_reorder(Model_Name, Cost)), Cost)) + geom_col(fill = "#599ad3") + labs(x = "Model Name", title = "Cost by Business Scenario - Balanced Data", fontface = "bold")
balanced_PlotCostModels2 + geom_text(aes(label = Cost, vjust = -.15, hjust = .5, size = 24), color = "#9e66ab", fontface = "bold") + theme(title = bold_axis) + theme(axis.text = axis_text) 

###Cost Savings difference from Logistic regression Model
balanced_Tuned_Cost_Model - reformedbalanced_RandomForest_Cost_Model ## Log Regression can save $2,640 more
(balanced_Tuned_Cost_Model - reformedbalanced_RandomForest_Cost_Model) / balanced_CostofnoPrediction * 100  #1% increase in savings

### Adding a continuous random variable to assess importance
set.seed(101)
balanced_dftrainplusrandom <- balanced_data_train %>% mutate(random = sample(100, size = nrow(balanced_data_train), replace = TRUE))
colnames(balanced_dftrainplusrandom)
head(balanced_dftrainplusrandom$random)

##Retrain model on new Data
set.seed(101)
balanced_reformedjgplusrandom <- randomForest(churn ~., data = balanced_dftrainplusrandom, ntree = 200, mtry = 6, importance = TRUE, proximity = TRUE)
print(balanced_reformedjgplusrandom)

## Random Forest Feature Importance
varImpPlot(balanced_reformedjgplusrandom, sort = TRUE, n.var = 10, main = "Feature Importance - Balanced Data")
importance(balanced_reformedjgplusrandom)




