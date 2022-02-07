#clear workspace
rm(list=ls())

#load packages
library("tidyverse")
library("psych")
library("mice")
library("dplyr")
library("missForest")
library("plotly")
library("zoo")
library("VIM")
library("car")
library("ggplot2")

churn_data <- read.csv("~/Desktop/Marketing Masters/data science methods/ass1/data Assignment 1.csv")
describe(churn_data)
glimpse(churn_data)
summary(churn_data)

#------------------descriptives
summary(churn_data)

#transforming some variables as real factors or numeric
churn_data$Gender <- as.factor(churn_data$Gender)
churn_data$Email_list <- as.factor(churn_data$Email_list)
churn_data$Home_label <- as.factor(churn_data$Home_label)
churn_data$Age <- as.numeric(churn_data$Age)
churn_data$Relation_length <- as.numeric(churn_data$Relation_length)
churn_data$Contract_length <- as.numeric(churn_data$Contract_length)
churn_data$Home_age <- as.numeric(churn_data$Home_age)
churn_data$Electricity_usage <- as.numeric(churn_data$Electricity_usage)
churn_data$Gas_usage <- as.numeric(churn_data$Gas_usage)
churn_data$Churn <- as.numeric(as.character(churn_data$Churn))

#------------------data cleaning
#missings
#are there some missings?
colSums(is.na(churn_data))
#no missing data

#outliers
#detect outliers 
str(churn_data)

#Age
hist(churn_data$Age)
#not skewed -> assuming normal distribution 
boxplot(churn_data$Age, main='Outliers Age', ylab='Years',
        col = 'royalblue')
#above 80 some outliers
summary(churn_data$Age)
#Min. 17, Max. 104 -> values make sense
# outliers
lower_bound <- quantile(churn_data$Age, 0.025)
lower_bound #27
upper_bound <- quantile(churn_data$Age, 0.975)
upper_bound #79
f <- sum(churn_data$Age < lower_bound)
g <- sum(churn_data$Age >= upper_bound)
outlier_num <- f+g
outlier_num#1024
outlier_perc <-0
as.data.frame(outlier_perc)
outlier_perc$Age <- as.data.frame(outlier_perc$Age <- outlier_num/20000)
outlier_perc$Age <- outlier_num/20000
outlier_perc$Age
#5.12% above 79 -> but this makes sense since highest value is 104 and lowest 17

#Income
hist(churn_data$Income)
#skewed 
boxplot(churn_data$Income, main='Outliers Income', ylab='Monthly income',
        col = 'royalblue')
# a lot of outliers are detected especially starting at 100,000 euro income
#log transformation could help?
hist(log(churn_data$Income))
boxplot(log(churn_data$Income), main='Outliers Log(Income)', ylab='Monthly income',
        col = 'royalblue')
#still detecting outliers
summary(churn_data$Income)
#Min: 684, Max: 235350
lower_bound <- quantile(churn_data$Income, 0.025)
lower_bound #1623
upper_bound <- quantile(churn_data$Income, 0.975)
upper_bound #12739.22
f <- sum(churn_data$Income < lower_bound)
g <- sum(churn_data$Income >= upper_bound)
outlier_num <- f+g
outlier_num #998
outlier_perc$Income <- outlier_num/20000
outlier_perc$Income
#4.99% 
#too many people have such a high income which is detected as outliers
#maybe some people gave their yearly income instead of monthly
#but we have no evidence for that
#winsorize these outliers
library(DescTools)
churn_data$Income<- Winsorize(churn_data$Income, probs = c(0.025, 0.975))
hist(log(churn_data$Income))
boxplot(log(churn_data$Income), main='Outliers Income', ylab='Monthly income',
        col = 'royalblue')
#no outliers anymore and normal distribution

#Relationship length
hist(churn_data$Relation_length)
#skewed 
boxplot(churn_data$Relation_length, main='Outliers Relationship length', ylab='Relationship length in months',
        col = 'royalblue')
#some outliers, let's check the percentage
summary(churn_data$Relation_length)
#Min: 0, Max: 241
lower_bound <- quantile(churn_data$Relation_length, 0.025)
lower_bound #3
upper_bound <- quantile(churn_data$Relation_length, 0.975)
upper_bound #156
f <- sum(churn_data$Relation_length < lower_bound)
g <- sum(churn_data$Relation_length >= upper_bound)
outlier_num <- f+g
outlier_num #990
outlier_perc$Relation_length <- outlier_num/20000
outlier_perc$Relation_length
#4.95% 
#log transformation can help?
hist(log(churn_data$Relation_length))
boxplot(log(churn_data$Relation_length), main='Outliers Log(Relationship length)', ylab='Log months',
        col = 'royalblue')
#no influential outliers anymore

#Contract length
hist(churn_data$Contract_length)
#skewed -> most have flexible contract (0)
boxplot(churn_data$Contract_length, main='Outliers Contract length', ylab='Contract length in months',
        col = 'royalblue')
#no outliers
#log transformation because of skewness
hist(log(churn_data$Contract_length))
#still no normal distribution
summary(churn_data$Contract_length)
#Min: 0, Max: 3
lower_bound <- quantile(churn_data$Contract_length, 0.025)
lower_bound #0
upper_bound <- quantile(churn_data$Contract_length, 0.975)
upper_bound #2.83
f <- sum(churn_data$Contract_length < lower_bound)
g <- sum(churn_data$Contract_length >= upper_bound)
outlier_num <- f+g
outlier_num #611
outlier_perc$Contract_length <- outlier_num/20000
outlier_perc$Contract_length
#3.1% 

#Home age
hist(churn_data$Home_age)
#skewed 
boxplot(churn_data$Home_age, main='Outliers Home age', ylab='Home age in years',
        col = 'royalblue')
#a lot of outliers above 90 years
#can log transformation help?
hist(log(churn_data$Home_age))
#assuming normal distribution now
boxplot(log(churn_data$Home_age), main='Outliers Log(Home age)', ylab='Home age in years',
        col = 'royalblue')
#no influential outliers anymore
summary(churn_data$Home_age)
#Min: 0, Max: 200
lower_bound <- quantile(churn_data$Home_age, 0.025)
lower_bound #2
upper_bound <- quantile(churn_data$Home_age, 0.975)
upper_bound #142
f <- sum(churn_data$Home_age < lower_bound)
g <- sum(churn_data$Home_age >= upper_bound)
outlier_num <- f+g
outlier_num #975
outlier_perc$Home_age <- outlier_num/20000
outlier_perc$Home_age
#4.88%
#solution through log-transformation

#electricity usage
hist(churn_data$Electricity_usage)
#skewed 
boxplot(churn_data$Electricity_usage, main='Outliers Electricity usage', ylab='Yearly electricity usage in kWh',
        col = 'royalblue')
#a lot of outliers
#can log transformation help?
hist(log(churn_data$Electricity_usage))
boxplot(log(churn_data$Electricity_usage), main='Outliers Log(Electricity usage)', ylab='Log(Yearly electricity usage in kWh)',
        col = 'royalblue')
#still a lot of outliers
summary(churn_data$Electricity_usage)
#Min: 231, Max: 41610
lower_bound <- quantile(churn_data$Electricity_usage, 0.025)
lower_bound #1252
upper_bound <- quantile(churn_data$Electricity_usage, 0.975)
upper_bound #3450
f <- sum(churn_data$Electricity_usage < lower_bound)
g <- sum(churn_data$Electricity_usage >= upper_bound)
outlier_num <- f+g
outlier_num #999
outlier_perc$Electricity_usage <- outlier_num/20000
outlier_perc$Electricity_usage
#5%
#outliers could be because of the different house sizes that we do not have in the dataset
#but we have no evidence where it is coming from
#so we winsorize!
churn_data$Electricity_usage<- Winsorize(churn_data$Electricity_usage, probs = c(0.025, 0.975))
hist(log(churn_data$Electricity_usage))
#now normal distribution
boxplot(log(churn_data$Electricity_usage), main='Outliers Electricity usage', ylab='Yearly elctricity usage',
        col = 'royalblue')
#no outliers anymore

#gas usage
hist(churn_data$Gas_usage)
#skewed 
boxplot(churn_data$Gas_usage, main='Outliers Gas usage', ylab='Yearly gas usage in cubic meters',
        col = 'royalblue')
#a lot of outliers
#can log transformation help?
hist(log(churn_data$Gas_usage))
boxplot(log(churn_data$Gas_usage), main='Outliers Log(Gas usage)', ylab='Log(Yearly gas usage in cubic meter)',
        col = 'royalblue')
#still a lot of outliers
summary(churn_data$Gas_usage)
#Min: 0, Max: 32570
lower_bound <- quantile(churn_data$Gas_usage, 0.025)
lower_bound #111.975
upper_bound <- quantile(churn_data$Gas_usage, 0.975)
upper_bound #2326
f <- sum(churn_data$Gas_usage < lower_bound)
g <- sum(churn_data$Gas_usage >= upper_bound)
outlier_num <- f+g
outlier_num #1001
outlier_perc$Gas_usage <- outlier_num/20000
outlier_perc$Gas_usage
#5.1%
#outliers could be that some people have a different gas provider since some customers (442) have 0 as a value,
#as well as different heating systems that we do not know about!
#we have no evidence and proof
#winsorizing?
churn_data$Gas_usage<- Winsorize(churn_data$Gas_usage, probs = c(0.025, 0.975))
hist(churn_data$Gas_usage)
boxplot(churn_data$Gas_usage, main='Outliers Gas usage winsorized', ylab='Yearly gas usage',
        col = 'royalblue')
#no more outliers even without log-transformation

#-----------------transforming data
#transforming some variables as dummies
#start channel dummy: 1 for online, 0 by phone
churn_data$Start_channel <- ifelse(churn_data$Start_channel == "Online", 1, 0)
churn_data$Start_channel<- as.factor(churn_data$Start_channel)

#flexibility contract
churn_data$contracttype <- churn_data$Contract_length
churn_data$contracttype <- ifelse(churn_data$Contract_length == "0", 1, 0)
churn_data$contracttype <- as.factor(churn_data$contracttype)

#income in years
churn_data$Income <- (churn_data$Income)*12
#income in thousands
churn_data$Income <- (churn_data$Income)/1000

#income in classes, avg. in Netherlands annually 36.500€
#low income until 35.000€ , medium above 35.000€ to 60.000€, high income above 60.000€
churn_data$incomeclass <- churn_data$Income
summary(churn_data$incomeclass)
churn_data$incomeclass <- as.numeric(churn_data$incomeclass)

churn_data$incomeclass <- ifelse(churn_data$incomeclass<= 35, "low", ifelse(churn_data$incomeclass>60,"high","medium"))
unique(churn_data$incomeclass)
table(churn_data$incomeclass)
#almost balanced
churn_data$incomeclass <- as.factor(churn_data$incomeclass)

#contract years and relation ship in years
churn_data$Contract_length_years <- (churn_data$Contract_length)/12
churn_data$Relation_length_years <- (churn_data$Relation_length)/12

#age classes for interpretation
churn_data$Ageclass <- churn_data$Age
summary(churn_data$Ageclass)
churn_data$Ageclass <- as.numeric(churn_data$Ageclass)
churn_data$Ageclass[churn_data$Age >=17 & churn_data$Age <= 30 ] <- "17-30"
churn_data$Ageclass[churn_data$Age >30 & churn_data$Age <= 45 ] <- "31-45"
churn_data$Ageclass[churn_data$Age >45 & churn_data$Age <= 65 ] <- "45-65"
churn_data$Ageclass[churn_data$Age >65] <- ">65"
table(churn_data$Ageclass)
#almost balanced
unique(churn_data$Ageclass)
churn_data$Ageclass <- as.factor(churn_data$Ageclass)


#multicollinearity
multitest <- lm(Churn ~  Gender + Age +
                  Income + Relation_length_years + Contract_length_years + Start_channel + Email_list
                +Home_age + Home_label + Electricity_usage + Gas_usage + Province +incomeclass + Ageclass + contracttype , data=churn_data) 
#create vector of VIF values and a vector of tolerance values
vif_values <- vif(multitest)
tolerance <- 1/vif_values
# the closer to 1, the lower the level of multicollinearity. Result - no multicol
vif_values
#The closer to 0, the higher the level of multicollinearity. Result - no multicol
tolerance
#no multicollinearity

#-----------------Modelling
#-----------------baseline model
#logistic regression using variables that are important based on literature
Model1 <-  glm(Churn ~ Age + log(Income) + log(Relation_length_years+1) + log(Contract_length_years+1) + Start_channel
               + Email_list  + Home_label + log(Electricity_usage) + Gas_usage + Province, family=binomial, data=churn_data)
summary(Model1)
#interpretation
#marginal pos.significant for age
#neg.significant for log(income)
#neg. significant: log(relationship length), log(contract length)
#pos.significant: startchannel, email list,Homelabelb,c,d,e,f,g, log(Electricity) & gas usage, Zuid-Holland

#odds ratio
exp(coef(Model1))

#marginal effects
library(mfx)
logitmfx(Churn ~ Age + log(Income) + log(Relation_length_years+1) + log(Contract_length_years+1) + Start_channel
         + Email_list  + Home_label + log(Electricity_usage) + Gas_usage + Province, data=churn_data)
#values super super small (probably better to use odd ratios for interpretation)

#Model Validation 
#get prediction 
predictions_model1 <- predict(Model1, type = "response", newdata=churn_data)
#Hit rate
predicted_model1 <- ifelse(predictions_model1>.5,1,0)
hit_rate_model1 <- table(churn_data$Churn, predicted_model1, dnn= c("Observed", "Predicted"))
hit_rate_model1
#2769 wrongly predicted as 1
#2692 worngly predicted as 0

#Get the hit rate
(hit_rate_model1[1,1]+hit_rate_model1[2,2])/sum(hit_rate_model1)
#0.727 -> the closer to 1 the better
#model can predict 72.7% correctly (above 50% -> ok)

#Lift curve/Gini coefficient
#Make lift curve
library(ROCR)
pred_model1 <- prediction(predictions_model1, churn_data$Churn)
perf_model1 <- performance(pred_model1,"tpr","fpr")
plot(perf_model1,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model1 <- performance(pred_model1,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model1@y.values)*2-1
#Gini:0.6130
#the closer to 1 the better
#focus on overall performance

#Top decile lift
decile_predicted_model1 <- ntile(predictions_model1, 10)
decile_model1 <- table(churn_data$Churn, decile_predicted_model1, dnn= c("Observed", "Decile"))
decile_model1

#Calculate the TDL
(decile_model1[2,10] / (decile_model1[1,10]+ decile_model1[2,10])) / mean(churn_data$Churn)
#1.84 #closer to 2 than to 1 -> model is 1.84 as good at predicting churn than random selection
#focus on predicting 1s

#Out of sample validation
#Get a 75% estimation sample and 25% validation sample
set.seed(1234)
churn_data$estimation_sample <-rbinom(nrow(churn_data), 1, 0.75)

#Estimate the model using only the estimation sample
Model1_sample <- glm(Churn ~ Age + log(Income) + log(Relation_length_years+1) + log(Contract_length_years+1) + Start_channel
                     + Email_list  + Home_label + log(Electricity_usage) + Gas_usage + Province, family=binomial, data=churn_data, subset=estimation_sample==1)


#Create a new dataframe with only the validation sample
our_validation_dataset <- churn_data[churn_data$estimation_sample==0,]

#Get predictions for all observations
predictions_model1b <- predict(Model1_sample, type = "response", newdata= our_validation_dataset)

#Hitrate sample
predicted_model1b <- ifelse(predictions_model1b>.5,1,0)
hit_rate_model1b <- table(our_validation_dataset$Churn, predicted_model1b, dnn= c("Observed", "Predicted"))
hit_rate_model1b
#733 wrongly predicted as 1
#631 wrongly predicted as 0

#Get the hit rate
(hit_rate_model1b[1,1]+hit_rate_model1b[2,2])/sum(hit_rate_model1b)
#0.7245 -> almost the same as without sample

#Lift curve/Gini coefficient
#Make lift curve
pred_model1b <- prediction(predictions_model1b, our_validation_dataset$Churn)
perf_model1b <- performance(pred_model1b,"tpr","fpr")
plot(perf_model1b,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model1b <- performance(pred_model1b,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model1b@y.values)*2-1
#Gini:0.607
#the closer to 1 the better, almost the same as without sample
#focus on overall performance

#Top decile lift
decile_predicted_model1b <- ntile(predictions_model1b, 10)
decile_model1b <- table(our_validation_dataset$Churn, decile_predicted_model1b, dnn= c("Observed", "Decile"))
decile_model1b

#Calculate the TDL
(decile_model1b[2,10] / (decile_model1b[1,10]+ decile_model1b[2,10])) / mean(our_validation_dataset$Churn)
#1.86 #closer to 2 than to 1 -> model is 1.86 as good at predicting churn than random selection
#focus on predicting 1s
#better than without sample

#-----------------stepwise regression
library(MASS)
#Estimate full and null model
Logistic_regression_full <- glm(Churn ~ ., data = churn_data, family = binomial, subset=estimation_sample==1)
Logistic_regression_null <- glm(Churn ~ 0, data = churn_data, family = binomial, subset=estimation_sample==1)
# Fit the model backward
Logistic_regression_backward <- stepAIC(Logistic_regression_full, direction="backward", trace = TRUE)
# Fit the model forward
Logistic_regression_forward <- stepAIC(Logistic_regression_null, direction="forward", scope=list(lower=Logistic_regression_null, upper=Logistic_regression_full), trace = TRUE)
# Fit the model both directions
Logistic_regression_both <- stepAIC(Logistic_regression_full, direction="both", trace = TRUE)
#best model:Churn ~ Age + Income + Relation_length + Start_channel + Email_list + 
#Home_label + Electricity_usage + Gas_usage + Province + contracttype +  incomeclass
#same as baseline model 1 but added with contracttype and incomeclass

#Validation using validation sample for Logistic_regression_both
predictions_model2 <- predict(Logistic_regression_both, type = "response", newdata= our_validation_dataset)

#Hitrate sample
predicted_model2 <- ifelse(predictions_model2>.5,1,0)
hit_rate_model2 <- table(our_validation_dataset$Churn, predicted_model2, dnn= c("Observed", "Predicted"))
hit_rate_model2
#626 wrongly predicted as 1
#606 wrongly predicted as 0

#Get the hit rate
(hit_rate_model2[1,1]+hit_rate_model2[2,2])/sum(hit_rate_model2)
#0.7511 -> better than baseline model
#75.11% of prediction correctly

#Lift curve/Gini coefficient
#Make lift curve
pred_model2 <- prediction(predictions_model2, our_validation_dataset$Churn)
perf_model2 <- performance(pred_model2,"tpr","fpr")
plot(perf_model2,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model2<- performance(pred_model2,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model2@y.values)*2-1
#Gini:0.66
#the closer to 1 the better, almost the same as without sample
#focus on overall performance
#better than baseline model

#Top decile lift
decile_predicted_model2 <- ntile(predictions_model2, 10)
decile_model2 <- table(our_validation_dataset$Churn, decile_predicted_model2, dnn= c("Observed", "Decile"))
decile_model2

#Calculate the TDL
(decile_model2[2,10] / (decile_model2[1,10]+ decile_model2[2,10])) / mean(our_validation_dataset$Churn)
#1.87 #closer to 2 than to 1 -> model is 1.87 as good at predicting churn than random selection
#focus on predicting 1s
#better than baseline model
#-----------------decision trees
#CART
# Tree model
library(rpart)
library(partykit)
Cart_tree1 <- rpart(Churn ~ Gender + incomeclass + log(Income) + Age + log(Relation_length_years+1) + log(Contract_length_years+1) + Start_channel + Email_list + contracttype +
                      + log(Home_age) + Home_label + log(Electricity_usage) + Gas_usage + Province, data=churn_data, method="class", subset=estimation_sample==1)
Cart_tree1_visual <- as.party(Cart_tree1)
plot(Cart_tree1_visual , type="simple", gp = gpar(fontsize = 10))
#contract length most important, followed by electricity usage, relation length and gas usage

#predictions for validation
predictions_cart1 <- predict(Cart_tree1, newdata= our_validation_dataset, type ="prob")[,2]
#hitrate
predicted_model3 <- ifelse(predictions_cart1>.5,1,0)
hit_rate_model3 <- table(our_validation_dataset$Churn, predicted_model3, dnn= c("Observed", "Predicted"))
hit_rate_model3

#Get the hit rate
(hit_rate_model3[1,1]+hit_rate_model3[2,2])/sum(hit_rate_model3)
#71.54% -> worse than stepwise logistic regression

#Lift curve/Gini coefficient
#Make lift curve
pred_model3 <- prediction(predictions_cart1, our_validation_dataset$Churn)
perf_model3 <- performance(pred_model3,"tpr","fpr")
plot(perf_model3,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model3 <- performance(pred_model3,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model3@y.values)*2-1
#Gini:0.51
#the higher the better
#worse than stepwise logistic regression

#Top decile lift
decile_predicted_model3 <- ntile(predictions_cart1, 10)
decile_model3 <- table(our_validation_dataset$Churn, decile_predicted_model3, dnn= c("Observed", "Decile"))
decile_model3

#Calculate the TDL
(decile_model3[2,10] / (decile_model3[1,10]+ decile_model3[2,10])) / mean(our_validation_dataset$Churn)
#1.62 #closer to 2 than to 1 -> model is 1.62 as good as predicting churners than random selection
#worse than stepwise logistic regression

# Changing settings to find maybe a better CART tree
newsettings1 <- rpart.control(minsplit = 100, minbucket = 50, cp = 0.01, maxdepth = 3)

Cart_tree2 <- rpart(Churn ~ Gender + incomeclass + log(Income) + Age + log(Relation_length_years+1) + log(Contract_length_years+1) + Start_channel + Email_list + contracttype +
                      + log(Home_age) + Home_label + log(Electricity_usage) + Gas_usage + Province, data=churn_data, method="class", subset=estimation_sample==1, control=newsettings1)
Cart_tree2_visual <- as.party(Cart_tree2)
plot(Cart_tree2_visual , type="simple")

#predictions for validation
predictions_cart2 <- predict(Cart_tree2, newdata= our_validation_dataset, type ="prob")[,2]
#hitrate
predicted_model3b <- ifelse(predictions_cart2>.5,1,0)
hit_rate_model3b <- table(our_validation_dataset$Churn, predicted_model3b, dnn= c("Observed", "Predicted"))
hit_rate_model3b

#Get the hit rate
(hit_rate_model3b[1,1]+hit_rate_model3b[2,2])/sum(hit_rate_model3b)
#70.61% -> worse than with original setting

#Lift curve/Gini coefficient
#Make lift curve
pred_model3b <- prediction(predictions_cart2, our_validation_dataset$Churn)
perf_model3b <- performance(pred_model3b,"tpr","fpr")
plot(perf_model3b,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model3b <- performance(pred_model3b,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model3b@y.values)*2-1
#Gini:0.50
#the higher the better
#worse than with original setting

#Top decile lift
decile_predicted_model3b <- ntile(predictions_cart2, 10)
decile_model3b <- table(our_validation_dataset$Churn, decile_predicted_model3b, dnn= c("Observed", "Decile"))
decile_model3b

#Calculate the TDL
(decile_model3b[2,10] / (decile_model3b[1,10]+ decile_model3b[2,10])) / mean(our_validation_dataset$Churn)
#1.62 #closer to 2 than to 1 -> model is 1.62 as good as predicting churners than random selection
#same than with original setting

# Changing settings to find maybe a better CART tree
newsettings2 <- rpart.control(minsplit = 60, minbucket = 30, cp = 0.01, maxdepth = 30)

Cart_tree3 <- rpart(Churn ~ Gender + incomeclass + log(Income) + Age + log(Relation_length_years+1) + log(Contract_length_years+1) + Start_channel + Email_list + contracttype +
                      + log(Home_age) + Home_label + log(Electricity_usage) + Gas_usage + Province, data=churn_data, method="class", subset=estimation_sample==1, control=newsettings2)
Cart_tree3_visual <- as.party(Cart_tree3)
plot(Cart_tree3_visual , type="simple")

#predictions for validation
predictions_cart3 <- predict(Cart_tree3, newdata= our_validation_dataset, type ="prob")[,2]
#hitrate
predicted_model3c <- ifelse(predictions_cart3>.5,1,0)
hit_rate_model3c <- table(our_validation_dataset$Churn, predicted_model3c, dnn= c("Observed", "Predicted"))
hit_rate_model3c

#Get the hit rate
(hit_rate_model3c[1,1]+hit_rate_model3c[2,2])/sum(hit_rate_model3c)
#71.54% -> same as original setting 
#Lift curve/Gini coefficient
#Make lift curve
pred_model3c <- prediction(predictions_cart3, our_validation_dataset$Churn)
perf_model3c <- performance(pred_model3c,"tpr","fpr")
plot(perf_model3c,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model3c <- performance(pred_model3c,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model3c@y.values)*2-1
#Gini:0.51
#the higher the better
#same as orginal

#Top decile lift
decile_predicted_model3c <- ntile(predictions_cart3, 10)
decile_model3c <- table(our_validation_dataset$Churn, decile_predicted_model3c, dnn= c("Observed", "Decile"))
decile_model3c

#Calculate the TDL
(decile_model3c[2,10] / (decile_model3c[1,10]+ decile_model3c[2,10])) / mean(our_validation_dataset$Churn)
#1.62 #closer to 2 than to 1 -> model is 1.62 as good as predicting churners than random selection
#same than with original setting

#original has the best settings, played around and max.depth of 30 brings best models
#hitrate changes only with change of maxdepth

# -----------------Bagging
library(ipred)
library(caret)

newsettings3 <- rpart.control(minsplit = 2, cp = 0.0)
#Essentially these two arguments allow the individual trees to grow extremely deep, which leads to trees with high variance but low bias. Then when we apply bagging we're able to reduce the variance of the final model while keeping the bias low.

#estimate model with bagging
Bagging_tree1 <- bagging(Churn ~ Gender + incomeclass + log(Income) + Age + log(Relation_length_years+1) + log(Contract_length_years+1) + Start_channel + Email_list + contracttype +
                           + log(Home_age) + Home_label + log(Electricity_usage) + Gas_usage + Province, data=churn_data, method="class", nbagg=25, subset=estimation_sample==1, control=newsettings3)

#Save predictions
predictions_bagging1 <- predict(Bagging_tree1, newdata=our_validation_dataset, type ="response")

#hitrate
predicted_model4 <- ifelse(predictions_bagging1>.5,1,0)
hit_rate_model4 <- table(our_validation_dataset$Churn, predicted_model4, dnn= c("Observed", "Predicted"))
hit_rate_model4

#Get the hit rate
(hit_rate_model4[1,1]+hit_rate_model4[2,2])/sum(hit_rate_model4)
#73.08% -> better prediction than with CART but worse than stepwise regression

#Lift curve/Gini coefficient
#Make lift curve
dev.off()
pred_model4 <- prediction(predicted_model4, our_validation_dataset$Churn)
perf_model4 <- performance(pred_model4,"tpr","fpr")
plot(perf_model4,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model4 <- performance(pred_model4,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model4@y.values)*2-1
#Gini:0.611
#the higher the better
#better than CART but worse than stepwise regression

#Top decile lift
decile_predicted_model4 <- ntile(predictions_bagging1, 10)
decile_model4 <- table(our_validation_dataset$Churn, decile_predicted_model4, dnn= c("Observed", "Decile"))
decile_model4

#Calculate the TDL
(decile_model4[2,10] / (decile_model4[1,10]+ decile_model4[2,10])) / mean(our_validation_dataset$Churn)
#1.85 #closer to 2 than to 1 -> model is 1.83 as good as predicting churners than random selection
#better than CART but worse than stepwise regression

#calculate variable importance
pred.imp <- varImp(Bagging_tree1)
pred.imp
#Age, Income, relation length, home age, electricity usage
#not align with the results of logistic regression and CART

#You can also plot the results
barplot(pred.imp$Overall, names.arg = row.names(pred.imp))

# -----------------Boosting
install.packages("gbm")
library(gbm)

#Estimate the model
boost_tree1 <- gbm(Churn ~ Gender + incomeclass + log(Income) + Age + log(Relation_length_years+1) + log(Contract_length_years+1) + Start_channel + Email_list + contracttype +
                     + log(Home_age) + Home_label + log(Electricity_usage) + Gas_usage, data=churn_data, distribution = "bernoulli", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)

#Get model output (summary also provides a graph)
boost_tree1
summary(boost_tree1)

best.iter <- gbm.perf(boost_tree1, method = "OOB")
summary(boost_tree1, n.trees = best.iter)
#electricty usage, contracttype, gas usage, contract length and relation length as most important

#Save predictions
predictions_boost1 <- predict(boost_tree1, newdata=our_validation_dataset, type ="response")

#hitrate
predicted_model5 <- ifelse(predictions_boost1 >.5,1,0)
hit_rate_model5 <- table(our_validation_dataset$Churn, predicted_model5, dnn= c("Observed", "Predicted"))
hit_rate_model5

#Get the hit rate
(hit_rate_model5[1,1]+hit_rate_model5[2,2])/sum(hit_rate_model5)
#81% -> better prediction than all previous models

#Lift curve/Gini coefficient
#Make lift curve
pred_model5 <- prediction(predictions_boost1, our_validation_dataset$Churn)
perf_model5 <- performance(pred_model5,"tpr","fpr")
plot(perf_model5,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model5 <- performance(pred_model5,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model5@y.values)*2-1
#Gini:0.78
#the higher the better
#better than all previous models

#Top decile lift
decile_predicted_model5 <- ntile(predictions_boost1, 10)
decile_model5 <- table(our_validation_dataset$Churn, decile_predicted_model5, dnn= c("Observed", "Decile"))
decile_model5

#Calculate the TDL
(decile_model5[2,10] / (decile_model5[1,10]+ decile_model5[2,10])) / mean(our_validation_dataset$Churn)
#1.99 #closer to 2 than to 1 -> model is 1.99 as good as predicting churners than random selection
#better than all previous models (almost twice as good)


# -----------------Random forest
library(randomForest)

Random_forest1 <- randomForest(as.factor(Churn) ~ ., data=churn_data, subset=estimation_sample==1, importance=TRUE)

#Validation
predictions_forest1 <- predict(Random_forest1, newdata=our_validation_dataset, type ="prob")[,2]

#hitrate
predicted_model6 <- ifelse(predictions_forest1 >.5,1,0)
hit_rate_model6 <- table(our_validation_dataset$Churn, predicted_model6, dnn= c("Observed", "Predicted"))
hit_rate_model6

#Get the hit rate
(hit_rate_model6[1,1]+hit_rate_model6[2,2])/sum(hit_rate_model6)
#74.93% -> better than CART and Bagging, but still worse than stepwise regression and boosting

#Lift curve/Gini coefficient
#Make lift curve
pred_model6 <- prediction(predictions_forest1, our_validation_dataset$Churn)
perf_model6 <- performance(pred_model6,"tpr","fpr")
plot(perf_model6,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model6 <- performance(pred_model6,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model6@y.values)*2-1
#Gini:0.65
#the higher the better
#worse than boosting and stepwise regression but better than bagging and CART

#Top decile lift
decile_predicted_model6 <- ntile(predictions_forest1, 10)
decile_model6 <- table(our_validation_dataset$Churn, decile_predicted_model6, dnn= c("Observed", "Decile"))
decile_model6

#Calculate the TDL
(decile_model6[2,10] / (decile_model6[1,10]+ decile_model6[2,10])) / mean(our_validation_dataset$Churn)
#1.87
#same as stepwise regression,worse than boosting and better than CART and bagging

#variable importance
varImpPlot(Random_forest1)
#electricty usage most important, followed by gas_usage and relation_length and income
#gender, province and age class not important

#New settings
Random_forest1b <- randomForest(as.factor(Churn) ~ ., data=churn_data, subset=estimation_sample==1,
                                ntree=500, mtry=3, nodesize=1, maxnodes=100, importance=TRUE)

#Validation
predictions_forest1b <- predict(Random_forest1b, newdata=our_validation_dataset, type ="prob")[,2]

#hitrate
predicted_model6b <- ifelse(predictions_forest1b >.5,1,0)
hit_rate_model6b <- table(our_validation_dataset$Churn, predicted_model6b, dnn= c("Observed", "Predicted"))
hit_rate_model6b

#Get the hit rate
(hit_rate_model6b[1,1]+hit_rate_model6b[2,2])/sum(hit_rate_model6b)
#74.30% -> almost the same as with original setting

#Lift curve/Gini coefficient
#Make lift curve
pred_model6b <- prediction(predictions_forest1b, our_validation_dataset$Churn)
perf_model6b <- performance(pred_model6b,"tpr","fpr")
plot(perf_model6b,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model6b <- performance(pred_model6b,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model6b@y.values)*2-1
#Gini:0.63
#worse than original setting

#Top decile lift
decile_predicted_model6b <- ntile(predictions_forest1b, 10)
decile_model6b <- table(our_validation_dataset$Churn, decile_predicted_model6b, dnn= c("Observed", "Decile"))
decile_model6b

#Calculate the TDL
(decile_model6b[2,10] / (decile_model6b[1,10]+ decile_model6b[2,10])) / mean(our_validation_dataset$Churn)
#1.87
#same as original setting

#new settings 2.0
Random_forest1c <- randomForest(as.factor(Churn) ~ ., data=churn_data, subset=estimation_sample==1,
                                ntree=100, mtry=6, nodesize=1.5, maxnodes=200, importance=TRUE)

#Validation
predictions_forest1c <- predict(Random_forest1c, newdata=our_validation_dataset, type ="prob")[,2]

#hitrate
predicted_model6c <- ifelse(predictions_forest1c >.5,1,0)
hit_rate_model6c <- table(our_validation_dataset$Churn, predicted_model6c, dnn= c("Observed", "Predicted"))
hit_rate_model6c

#Get the hit rate
(hit_rate_model6c[1,1]+hit_rate_model6c[2,2])/sum(hit_rate_model6c)
#74.97 -> minimal better than original setting


#Lift curve/Gini coefficient
#Make lift curve
pred_model6c <- prediction(predictions_forest1c, our_validation_dataset$Churn)
perf_model6c <- performance(pred_model6b,"tpr","fpr")
plot(perf_model6c,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model6c <- performance(pred_model6c,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model6c@y.values)*2-1
#Gini:0.63
#worse than original setting, but better than previous setting

#Top decile lift
decile_predicted_model6c <- ntile(predictions_forest1c, 10)
decile_model6c <- table(our_validation_dataset$Churn, decile_predicted_model6c, dnn= c("Observed", "Decile"))
decile_model6c

#Calculate the TDL
(decile_model6c[2,10] / (decile_model6c[1,10]+ decile_model6c[2,10])) / mean(our_validation_dataset$Churn)
#1.85
#worse than original

#the bigger the maxnodes and mtry the better the model
#original = best setting

# Support Vector Machine --------------------------------------------------
library(e1071)

svm_1 <- svm(Churn ~ incomeclass + Income + Age + Relation_length_years + Contract_length_years + Start_channel + Email_list + contracttype +
               + Home_age + Home_label + Electricity_usage + Gas_usage, data = churn_data, subset=churn_data$estimation_sample==1,
             type = 'C-classification', probability = TRUE,kernel = 'linear')

plot(svm_1, churn_data, Electricity_usage~Gas_usage)
#NOT WORKING WITH LOG TRANSFORMED VARIABLES

#Get predictions
predictions_svm1 <- predict(svm_1, newdata=our_validation_dataset, probability=TRUE)
predictions_svm1 <- attr(predictions_svm1,"probabilities")[,1]

#Validation
#hitrate
predicted_model7 <- ifelse(predictions_svm1 >.5,1,0)
hit_rate_model7 <- table(our_validation_dataset$Churn, predicted_model7, dnn= c("Observed", "Predicted"))
hit_rate_model7

#Get the hit rate
(hit_rate_model7[1,1]+hit_rate_model7[2,2])/sum(hit_rate_model7)
#74.61% 

#Lift curve/Gini coefficient
#Make lift curve
pred_model7 <- prediction(predictions_svm1, our_validation_dataset$Churn)
perf_model7 <- performance(pred_model7,"tpr","fpr")
plot(perf_model7,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model7 <- performance(pred_model7,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model7@y.values)*2-1
#Gini:0.65

#Top decile lift
decile_predicted_model7 <- ntile(predictions_svm1, 10)
decile_model7 <- table(our_validation_dataset$Churn, decile_predicted_model7, dnn= c("Observed", "Decile"))
decile_model7

#Calculate the TDL
(decile_model7[2,10] / (decile_model7[1,10]+ decile_model7[2,10])) / mean(our_validation_dataset$Churn)
#1.88

#Same models, other functions
svm_2 <- svm(Churn ~ incomeclass + Income + Age + Relation_length_years + Contract_length_years + Start_channel + Email_list + contracttype +
               + Home_age + Home_label + Electricity_usage + Gas_usage, data = churn_data, subset=churn_data$estimation_sample==1,
             type = 'C-classification', probability = TRUE,
             kernel = 'polynomial')

#HOW TO INCLUDE LOG TRANSFORMED VARIABLES?

plot(svm_2, churn_data, Electricity_usage~Gas_usage)

#Get predictions
predictions_svm2 <- predict(svm_2, newdata=our_validation_dataset, probability=TRUE)
predictions_svm2 <- attr(predictions_svm2,"probabilities")[,1]

#Validation
#hitrate
predicted_model7b <- ifelse(predictions_svm2 >.5,1,0)
hit_rate_model7b <- table(our_validation_dataset$Churn, predicted_model7b, dnn= c("Observed", "Predicted"))
hit_rate_model7b

#Get the hit rate
(hit_rate_model7b[1,1]+hit_rate_model7b[2,2])/sum(hit_rate_model7b)
#74.85%
#better than svm 1

#Lift curve/Gini coefficient
#Make lift curve
pred_model7b <- prediction(predictions_svm2, our_validation_dataset$Churn)
perf_model7b <- performance(pred_model7b,"tpr","fpr")
plot(perf_model7b,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model7b <- performance(pred_model7b,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model7b@y.values)*2-1
#Gini:0.65
#same as svm1

#Top decile lift
decile_predicted_model7b <- ntile(predictions_svm2, 10)
decile_model7b <- table(our_validation_dataset$Churn, decile_predicted_model7b, dnn= c("Observed", "Decile"))
decile_model7b

#Calculate the TDL
(decile_model7b[2,10] / (decile_model7b[1,10]+ decile_model7b[2,10])) / mean(our_validation_dataset$Churn)
#1.85
#worse than svm1

#new function
svm_3 <- svm(Churn ~ incomeclass + Income + Age + Relation_length_years + Contract_length_years + Start_channel + Email_list + contracttype +
               + Home_age + Home_label + Electricity_usage + Gas_usage, data = churn_data, subset=churn_data$estimation_sample==1,
             type = 'C-classification', probability = TRUE,
             kernel = 'radial')
#HOW TO INCLUDE LOG TRANSFORMED?

plot(svm_3, churn_data, Electricity_usage~Gas_usage)

#Get predictions
predictions_svm3 <- predict(svm_3, newdata=our_validation_dataset, probability=TRUE)
predictions_svm3 <- attr(predictions_svm3,"probabilities")[,1]

#Validation
#hitrate
predicted_model7c <- ifelse(predictions_svm3 >.5,1,0)
hit_rate_model7c <- table(our_validation_dataset$Churn, predicted_model7c, dnn= c("Observed", "Predicted"))
hit_rate_model7c

#Get the hit rate
(hit_rate_model7c[1,1]+hit_rate_model7c[2,2])/sum(hit_rate_model7c)
#75.12%
#better than previous SVM models

#Lift curve/Gini coefficient
#Make lift curve
pred_model7c <- prediction(predictions_svm3, our_validation_dataset$Churn)
perf_model7c <- performance(pred_model7c,"tpr","fpr")
plot(perf_model7c,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model7c <- performance(pred_model7c,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model7c@y.values)*2-1
#Gini:0.66
#better than previous SVM models

#Top decile lift
decile_predicted_model7c <- ntile(predictions_svm3, 10)
decile_model7c <- table(our_validation_dataset$Churn, decile_predicted_model7c, dnn= c("Observed", "Decile"))
decile_model7c

#Calculate the TDL
(decile_model7c[2,10] / (decile_model7c[1,10]+ decile_model7c[2,10])) / mean(our_validation_dataset$Churn)
#1.87
#worse than svm1, but better than svm 2

#new function
svm_4 <- svm(Churn ~ incomeclass + Income + Age + Relation_length_years + Contract_length_years + Start_channel + Email_list + contracttype +
               + Home_age + Home_label + Electricity_usage + Gas_usage, data = churn_data, subset=churn_data$estimation_sample==1,
             type = 'C-classification', probability = TRUE,
             kernel = 'sigmoid')
#HOW TO INCLUDE LOG TRANSFORMED ?

plot(svm_4, churn_data, Electricity_usage~Gas_usage)

#Get predictions
predictions_svm4 <- predict(svm_4, newdata=our_validation_dataset, probability=TRUE)
predictions_svm4 <- attr(predictions_svm4,"probabilities")[,1]

#Validation
#hitrate
predicted_model7d <- ifelse(predictions_svm4 >.5,1,0)
hit_rate_model7d <- table(our_validation_dataset$Churn, predicted_model7d, dnn= c("Observed", "Predicted"))
hit_rate_model7d

#Get the hit rate
(hit_rate_model7d[1,1]+hit_rate_model7d[2,2])/sum(hit_rate_model7c)
#66.79%
#worse than all previous SVM models

#Lift curve/Gini coefficient
#Make lift curve
pred_model7d <- prediction(predictions_svm4, our_validation_dataset$Churn)
perf_model7d <- performance(pred_model7d,"tpr","fpr")
plot(perf_model7d,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model7d <- performance(pred_model7d,"auc")
#red line  =random selection
#our model is better than random selection

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model7d@y.values)*2-1
#Gini:0.43
#worse than all previous SVM models

#Top decile lift
decile_predicted_model7d <- ntile(predictions_svm4, 10)
decile_model7d <- table(our_validation_dataset$Churn, decile_predicted_model7d, dnn= c("Observed", "Decile"))
decile_model7d

#Calculate the TDL
(decile_model7d[2,10] / (decile_model7d[1,10]+ decile_model7d[2,10])) / mean(our_validation_dataset$Churn)
#1.52
#worse than all previous SVM models

#SVM3 is the best SVM model


#churn electricity usage
churn_gas<- aggregate(churn_data$Gas_usage, list(churn_data$Churn), mean)
View(churn_gas)

ggplot(data = churn_gas, aes(x = Group.1, y = x, fill = Group.1)) + 
  geom_col() +
  ggtitle("Average gas usage churn/not churn") +
  xlab("Churn yes/no") + ylab("Average gas usage") +
  theme(plot.title = element_text(hjust = 0.5))


#----------------------------------
#some thesis stuff i needed to save. Here wanted to shorten amount of NLP extracted topics, but save the prediction power of the dataset.
#applying Recursive Feature Elimination to shorten the datasaple (as it has 3k words/variables), but remain the prediction power of the dataset
#define control function to test which variables can be dropped, but that datasample remain the same in terms of prediction and etc
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

#words
x <- newDataSet %>%
  dplyr::select(-review_score) %>%
  as.data.frame()

#target variable
y <- newDataSet$review_score

# splitting dataset Training: 80%; Test: 20%
set.seed(2021)
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]

x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]

y_train <- y[ inTrain]
y_test  <- y[-inTrain]

# Run RFE
result_rfe1 <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = c(1:3360),
                   rfeControl = control)

# Print the results
result_rfe1

# Print the selected features
predictors(result_rfe1)



