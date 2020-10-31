#Initialising libraries - Start#
library("ggplot2")
library("car")
library("caret")
library("nortest")
library("class")
library("devtools")
library("e1071")
library("Hmisc")
library("MASS")
library("nnet")
library("plyr")
library("pROC")
library("psych")
library("scatterplot3d")
library("dplyr")
library("rpart")
library("rpart.plot")
library("randomForest")
library("neuralnet")
library("chron")
library("lubridate")
library("readxl")
#Initialising libraries - End#

#Setting up working directory & Picking the datasets - Start#
setwd("D:/GLIM/Terms folder/Term-2/Business Analytics/Final Project/Final/Section-B_Group-3") #Setting up the working directory
italy.master <- read_excel("Section-B_Group-3.xlsx", sheet = "Italy") #Code starts from line#37 and ends at line#142
sweden.master <- read_excel("Section-B_Group-3.xlsx", sheet = "Sweden") #Code starts from line#145 and ends at line#250
USA.master <- read_excel("Section-B_Group-3.xlsx", sheet = "USA_California") #Code starts from line#253 and ends at line#358
nz.master <- read_excel("Section-B_Group-3.xlsx", sheet = "New Zealand") #Code starts from line#360 and ends at line#473
Australia.master <- read_excel("Section-B_Group-3.xlsx", sheet = "Australia") #Code starts from line#475 and ends at line#599
india.master <- read_excel("Section-B_Group-3.xlsx", sheet = "India") #Code starts from line#601 and ends at line#725
#Setting up working directory & Picking the datasets - End#

###########################################################################
######################### Model for Italy - Start ######################### 
###########################################################################

#Assigning variables - Start#
italy.SI  <- italy.master$StringencyIndex
italy.RI  <- italy.master$RateOfInfection
italy.NC  <- italy.master$NewCases
italy.LNC <- log(italy.NC)
italy.CC  <- italy.master$ConfirmedCases
italy.LCC <- log(italy.CC)
italy.DT  <- italy.master$Actual_Date
italy.CD  <- italy.master$ConfirmedDeaths
italy.LCD <- log(italy.CD)
#Assigning variables - End#

#Interpreting Mean Value - Start#
mean(italy.SI)
mean(italy.RI)
sd(italy.SI)
#Interpreting Mean Value - End#

#Normalization & Checking for normal distribution - Start#
italy.RI.norm <- rnorm(italy.RI)
hist(italy.RI.norm)

italy.SI.norm <- rnorm(italy.SI)
hist(italy.SI.norm)

italy.NC.norm <- rnorm(italy.NC)
hist(italy.NC.norm)
#Normalization & Checking for normal distribution - End#

#Identifying relation between dates & New cases - Start#
qplot(italy.DT, italy.NC, colour = italy.SI, data=italy.master, geom = c("point","line"))
#Identifying relation between dates & New cases - End#

#Train & test model for multi-linear regression - Start#
set.seed(1234)
model.data <- sample(2, nrow(italy.master), replace=TRUE, prob = c(0.7,0.3))

#Splitting the data intro train & test - Start#
train <- italy.master[model.data==1,]
test <- italy.master[model.data==2,]
#Splitting the data intro train & test - End#

#Assigning variables - Start#
italy.C1 <- italy.master$`C1_School closing`
italy.C2 <- italy.master$`C2_Workplace closing`
italy.C3 <- italy.master$`C3_Cancel public events`
italy.C4 <- italy.master$`C4_Restrictions on gatherings`
italy.C5 <- italy.master$`C5_Close public transport`
italy.C6 <- italy.master$`C6_Stay at home requirements`
italy.C7 <- italy.master$`C7_Restrictions on internal movement`
italy.C8 <- italy.master$C8_International_travel_controls
italy.H1 <- italy.master$`H1_Public information campaigns`

italy.C1.flag <- italy.master$C1_Flag
italy.C2.flag <- italy.master$C2_Flag
italy.C3.flag <- italy.master$C3_Flag
italy.C4.flag <- italy.master$C4_Flag
italy.C5.flag <- italy.master$C5_Flag
italy.C6.flag <- italy.master$C6_Flag
italy.C7.flag <- italy.master$C7_Flag
italy.H1.flag <- italy.master$H1_Flag
#Assigning variables - End#

#Checking the fit of the model - Start of iteration-1#
Linear_1 <- italy.master$StringencyIndex~italy.C1+italy.C2+italy.C3+italy.C4+italy.C5+italy.C6+italy.C7+
              italy.C8+italy.C1.flag+italy.C2.flag+italy.C3.flag+italy.C4.flag+italy.C5.flag+
                italy.C6.flag+italy.C7.flag+italy.H1.flag
OLS_1 <- lm(Linear_1, data = train)
summary(OLS_1)
#Checking the fit of the model - End of iteration-1#

#Checking the fit of the model - Start of iteration-2#
Linear_2 <- italy.master$StringencyIndex~italy.C1+italy.C4+italy.C5+italy.C6+italy.C7+italy.C8+italy.C1.flag
OLS_2 <- lm(Linear_2, data = train)
summary(OLS_2)
vif(OLS_2)
#Checking the fit of the model - End of iteration-2#

#Finding the MSE value - Start#
Pred <- predict(OLS_2,test)
MSE <- mean((Pred-test$StringencyIndex)^2)
MSE
#Finding the MSE value - End#
#Train & test model for multi-linear regression - End#

#Visualizing the effect of lockdown (Stringency Index) on new cases - Start#
qplot(italy.DT, italy.SI,  colour = italy.SI, data = italy.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on new cases - End#

#Visualizing the effect of lockdown (Stringency Index) on confirmed cases - Start#
qplot(italy.DT, italy.LCC, colour = italy.SI, data = italy.master, geom = c("point","line"))
qplot(italy.DT, italy.CC,  colour = italy.SI, data = italy.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on confirmed cases - End#

#Visualizing the effect of lockdown (Stringency Index) on confirmed deaths - Start#
qplot(italy.DT, italy.LCD, colour = italy.SI, data = italy.master, geom = c("point","line"))
qplot(italy.DT, italy.CD,  colour = italy.SI, data = italy.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on confirmed deaths - End#

###########################################################################
######################### Model for Italy - End ###########################
###########################################################################


###########################################################################
######################### Model for Sweden - Start ########################
###########################################################################

#Assigning variables - Start#
sweden.SI  <- sweden.master$StringencyIndex
sweden.RI  <- sweden.master$RateOfInfection
sweden.NC  <- sweden.master$NewCases
sweden.LNC <- log(sweden.NC)
sweden.CC  <- sweden.master$ConfirmedCases
sweden.LCC <- log(sweden.CC)
sweden.DT  <- sweden.master$Actual_Date
sweden.CD  <- sweden.master$ConfirmedDeaths
sweden.LCD <- log(sweden.CD)
#Assigning variables - End#

#Interpreting Mean Value - Start#
mean(sweden.SI)
mean(sweden.RI)
sd(sweden.SI)
#Interpreting Mean Value - End#

#Normalization & Checking for normal distribution - Start#
sweden.RI.norm <- rnorm(sweden.RI)
hist(sweden.RI.norm)

sweden.SI.norm <- rnorm(sweden.SI)
hist(sweden.SI.norm)

sweden.NC.norm <- rnorm(sweden.NC)
hist(sweden.NC.norm)
#Normalization & Checking for normal distribution - End#

#Identifying relation between dates & New cases - Start#
qplot(sweden.DT, sweden.NC, colour = sweden.SI, data=sweden.master, geom = c("point","line"))
#Identifying relation between dates & New cases - End#

#Train & test model for multi-linear regression - Start#
set.seed(1234)
model.data <- sample(2, nrow(sweden.master), replace=TRUE, prob = c(0.7,0.3))

#Splitting the data intro train & test - Start#
train <- sweden.master[model.data==1,]
test <- sweden.master[model.data==2,]
#Splitting the data intro train & test - End#

#Assigning variables - Start#
sweden.C1 <- sweden.master$`C1_School closing`
sweden.C2 <- sweden.master$`C2_Workplace closing`
sweden.C3 <- sweden.master$`C3_Cancel public events`
sweden.C4 <- sweden.master$`C4_Restrictions on gatherings`
sweden.C5 <- sweden.master$`C5_Close public transport`
sweden.C6 <- sweden.master$`C6_Stay at home requirements`
sweden.C7 <- sweden.master$`C7_Restrictions on internal movement`
sweden.C8 <- sweden.master$C8_International_travel_controls
sweden.H1 <- sweden.master$`H1_Public information campaigns`

sweden.C1.flag <- sweden.master$C1_Flag
sweden.C2.flag <- sweden.master$C2_Flag
sweden.C3.flag <- sweden.master$C3_Flag
sweden.C4.flag <- sweden.master$C4_Flag
sweden.C5.flag <- sweden.master$C5_Flag
sweden.C6.flag <- sweden.master$C6_Flag
sweden.C7.flag <- sweden.master$C7_Flag
sweden.H1.flag <- sweden.master$H1_Flag
#Assigning variables - End#

#Checking the fit of the model - Start of iteration-1#
Linear_1 <- sweden.master$StringencyIndex~sweden.C1+sweden.C2+sweden.C3+sweden.C4+sweden.C5+sweden.C6+sweden.C7+
  sweden.C8+sweden.H1+sweden.C1.flag+sweden.C2.flag+sweden.C3.flag+sweden.C4.flag+sweden.C5.flag+
  sweden.C6.flag+sweden.C7.flag+sweden.H1.flag
OLS_1 <- lm(Linear_1, data = train)
summary(OLS_1)
#Checking the fit of the model - End of iteration-1#

#Checking the fit of the model - Start of iteration-2#
Linear_2 <- sweden.master$StringencyIndex~sweden.C1+sweden.C3+sweden.C4+sweden.C1.flag
OLS_2 <- lm(Linear_2, data = train)
summary(OLS_2)
vif(OLS_2)
#Checking the fit of the model - End of iteration-2#

#Finding the MSE value - Start#
Pred <- predict(OLS_2,test)
MSE <- mean((Pred-test$StringencyIndex)^2)
MSE
#Finding the MSE value - End#
#Train & test model for multi-linear regression - End#

#Visualizing the effect of lockdown (Stringency Index) on new cases - Start#
qplot(sweden.DT, sweden.SI,  colour = sweden.SI, data = sweden.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on new cases - End#

#Visualizing the effect of lockdown (Stringency Index) on confirmed cases - Start#
qplot(sweden.DT, sweden.LCC, colour = sweden.SI, data = sweden.master, geom = c("point","line"))
qplot(sweden.DT, sweden.CC,  colour = sweden.SI, data = sweden.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on confirmed cases - End#

#Visualizing the effect of lockdown (Stringency Index) on confirmed deaths - Start#
qplot(sweden.DT, sweden.LCD, colour = sweden.SI, data = sweden.master, geom = c("point","line"))
qplot(sweden.DT, sweden.CD,  colour = sweden.SI, data = sweden.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on confirmed deaths - End#

###########################################################################
######################### Model for Sweden - End ##########################
###########################################################################


###########################################################################
######################### Model for USA-California - Start ################
###########################################################################

#Assigning variables - Start#
USA.SI  <- USA.master$StringencyIndex
USA.RI  <- USA.master$RateOfInfection
USA.NC  <- USA.master$NewCases
USA.LNC <- log(USA.NC)
USA.CC  <- USA.master$ConfirmedCases
USA.LCC <- log(USA.CC)
USA.DT  <- USA.master$Actual_Date
USA.CD  <- USA.master$ConfirmedDeaths
USA.LCD <- log(USA.CD)
#Assigning variables - End#

#Interpreting Mean Value - Start#
mean(USA.SI)
mean(USA.RI)
sd(USA.SI)
#Interpreting Mean Value - End#

#Normalization & Checking for normal distribution - Start#
USA.RI.norm <- rnorm(USA.RI)
hist(USA.RI.norm)

USA.SI.norm <- rnorm(USA.SI)
hist(USA.SI.norm)

USA.NC.norm <- rnorm(USA.NC)
hist(USA.NC.norm)
#Normalization & Checking for normal distribution - End#

#Identifying relation between dates & New cases - Start#
qplot(USA.DT, USA.NC, colour = USA.SI, data=USA.master, geom = c("point","line"))
#Identifying relation between dates & New cases - End#

#Train & test model for multi-linear regression - Start#
set.seed(143)
model.data <- sample(2, nrow(USA.master), replace=TRUE, prob = c(0.7,0.3))

#Splitting the data intro train & test - Start#
train <- USA.master[model.data==1,]
test <- USA.master[model.data==2,]
#Splitting the data intro train & test - End#

#Assigning variables - Start#
USA.C1 <- USA.master$`C1_School closing`
USA.C2 <- USA.master$`C2_Workplace closing`
USA.C3 <- USA.master$`C3_Cancel public events`
USA.C4 <- USA.master$`C4_Restrictions on gatherings`
USA.C5 <- USA.master$`C5_Close public transport`
USA.C6 <- USA.master$`C6_Stay at home requirements`
USA.C7 <- USA.master$`C7_Restrictions on internal movement`
USA.C8 <- USA.master$C8_International_travel_controls
USA.H1 <- USA.master$`H1_Public information campaigns`

USA.C1.flag <- USA.master$C1_Flag
USA.C2.flag <- USA.master$C2_Flag
USA.C3.flag <- USA.master$C3_Flag
USA.C4.flag <- USA.master$C4_Flag
USA.C5.flag <- USA.master$C5_Flag
USA.C6.flag <- USA.master$C6_Flag
USA.C7.flag <- USA.master$C7_Flag
USA.H1.flag <- USA.master$H1_Flag
#Assigning variables - End#

#Checking the fit of the model - Start of iteration-1#
Linear_1 <- USA.master$StringencyIndex~USA.C1+USA.C2+USA.C3+USA.C4+USA.C5+USA.C6+USA.C7+
  USA.C8+USA.H1+USA.C1.flag+USA.C2.flag+USA.C3.flag+USA.C4.flag+USA.C5.flag+
  USA.C6.flag+USA.C7.flag+USA.H1.flag
OLS_1 <- lm(Linear_1, data = train)
summary(OLS_1)
#Checking the fit of the model - End of iteration-1#

#Checking the fit of the model - Start of iteration-2#
Linear_2 <- USA.master$StringencyIndex~USA.C7+USA.C8+USA.C1.flag+USA.C2.flag+USA.C4.flag+USA.C6.flag
OLS_2 <- lm(Linear_2, data = train)
summary(OLS_2)
vif(OLS_2)
#Checking the fit of the model - End of iteration-2#

#Finding the MSE value - Start#
Pred <- predict(OLS_2,test)
MSE <- mean((Pred-test$StringencyIndex)^2)
MSE
#Finding the MSE value - End#
#Train & test model for multi-linear regression - End#

#Visualizing the effect of lockdown (Stringency Index) on new cases - Start#
qplot(USA.DT, USA.SI,  colour = USA.SI, data = USA.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on new cases - End#

#Visualizing the effect of lockdown (Stringency Index) on confirmed cases - Start#
qplot(USA.DT, USA.LCC, colour = USA.SI, data = USA.master, geom = c("point","line"))
qplot(USA.DT, USA.CC,  colour = USA.SI, data = USA.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on confirmed cases - End#

#Visualizing the effect of lockdown (Stringency Index) on confirmed deaths - Start#
qplot(USA.DT, USA.LCD, colour = USA.SI, data = USA.master, geom = c("point","line"))
qplot(USA.DT, USA.CD,  colour = USA.SI, data = USA.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on confirmed deaths - End#

###################################################################################
######################### Model for USA-California - End ##########################
###################################################################################

###################################################################################
######################### Model for New Zealand - Start ###########################
###################################################################################

#Assigning variables - Start#
nz.SI  <- nz.master$StringencyIndex
nz.RI  <- nz.master$RateOfInfection
nz.NC  <- nz.master$NewCases
nz.LNC <- log(nz.NC)
nz.CC  <- nz.master$ConfirmedCases
nz.LCC <- log(nz.CC)
nz.DT  <- nz.master$Actual_Date
nz.CD  <- nz.master$ConfirmedDeaths
nz.LCD <- log(nz.CD)
#Assigning variables - End#

#Interpreting Mean Value - Start#
mean(nz.SI)
mean(nz.RI)
sd(nz.SI)
#Interpreting Mean Value - End#

#Normalization & Checking for normal distribution - Start#
nz.RI.norm <- rnorm(nz.RI)
hist(nz.RI.norm)

nz.SI.norm <- rnorm(nz.SI)
hist(nz.SI.norm)

nz.NC.norm <- rnorm(nz.NC)
hist(nz.NC.norm)
#Normalization & Checking for normal distribution - End#

#Identifying relation between dates & New cases - Start#
qplot(nz.DT, nz.NC, colour = nz.SI, data=nz.master, geom = c("point","line"))
#Identifying relation between dates & New cases - End#

#Train & test model for multi-linear regression - Start#
set.seed(1234)
model.data <- sample(2, nrow(nz.master), replace=TRUE, prob = c(0.7,0.3))

#Splitting the data intro train & test - Start#
train <- nz.master[model.data==1,]
test <- nz.master[model.data==2,]
#Splitting the data intro train & test - End#

#Assigning variables - Start#
nz.C1 <- nz.master$`C1_School closing`
nz.C2 <- nz.master$`C2_Workplace closing`
nz.C3 <- nz.master$`C3_Cancel public events`
nz.C4 <- nz.master$`C4_Restrictions on gatherings`
nz.C5 <- nz.master$`C5_Close public transport`
nz.C6 <- nz.master$`C6_Stay at home requirements`
nz.C7 <- nz.master$`C7_Restrictions on internal movement`
nz.C8 <- nz.master$C8_International_travel_controls
nz.H1 <- nz.master$`H1_Public information campaigns`

nz.C1.flag <- nz.master$C1_Flag
nz.C2.flag <- nz.master$C2_Flag
nz.C3.flag <- nz.master$C3_Flag
nz.C4.flag <- nz.master$C4_Flag
nz.C5.flag <- nz.master$C5_Flag
nz.C6.flag <- nz.master$C6_Flag
nz.C7.flag <- nz.master$C7_Flag
nz.H1.flag <- nz.master$H1_Flag
#Assigning variables - End#

#Checking the fit of the model - Start of iteration-1#
Linear_1 <- nz.master$StringencyIndex~nz.C1+nz.C2+nz.C3+nz.C4+nz.C5+nz.C6+nz.C7+
  nz.C8+nz.H1+nz.C1.flag+nz.C2.flag+nz.C3.flag+nz.C4.flag+nz.C5.flag+
  nz.C6.flag+nz.C7.flag+nz.H1.flag
OLS_1 <- lm(Linear_1, data = train)
summary(OLS_1)
#Checking the fit of the model - End of iteration-1#

#Checking the fit of the model - Start of iteration-2#
Linear_2 <- nz.master$StringencyIndex~nz.C1+nz.C2+nz.C4+nz.C5+nz.C6+
  nz.C8+nz.H1+nz.C1.flag+nz.C2.flag+nz.C5.flag+nz.H1.flag
OLS_2 <- lm(Linear_2, data = train)
summary(OLS_2)
vif(OLS_2)
#Checking the fit of the model - End of iteration-2#

#Checking the fit of the model - Start of iteration-3#
Linear_3 <- nz.master$StringencyIndex~nz.C4+nz.C5+nz.C8+nz.C3.flag
OLS_3 <- lm(Linear_3, data = train)
summary(OLS_3)
vif(OLS_3)
#Checking the fit of the model - End of iteration-3#

#Finding the MSE value - Start#
Pred <- predict(OLS_3,test)
MSE <- mean((Pred-test$StringencyIndex)^2)
MSE
#Finding the MSE value - End#
#Train & test model for multi-linear regression - End#

#Visualizing the effect of lockdown (Stringency Index) on new cases - Start#
qplot(nz.DT, nz.SI,  colour = nz.SI, data = nz.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on new cases - End#

#Visualizing the effect of lockdown (Stringency Index) on confirmed cases - Start#
qplot(nz.DT, nz.LCC, colour = nz.SI, data = nz.master, geom = c("point","line"))
qplot(nz.DT, nz.CC,  colour = nz.SI, data = nz.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on confirmed cases - End#

#Visualizing the effect of lockdown (Stringency Index) on confirmed deaths - Start#
qplot(nz.DT, nz.LCD, colour = nz.SI, data = nz.master, geom = c("point","line"))
qplot(nz.DT, nz.CD,  colour = nz.SI, data = nz.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on confirmed deaths - End#

###################################################################################
######################### Model for New Zealand - End #############################
###################################################################################

###################################################################################
######################### Model for Australia - Start #############################
###################################################################################

#Assigning variables - Start#
Australia.SI  <- Australia.master$StringencyIndex
Australia.RI  <- Australia.master$RateOfInfection
Australia.NC  <- Australia.master$NewCases
Australia.LNC <- log(Australia.NC)
Australia.CC  <- Australia.master$ConfirmedCases
Australia.LCC <- log(Australia.CC)
Australia.DT  <- Australia.master$Actual_Date
Australia.CD  <- Australia.master$ConfirmedDeaths
Australia.LCD <- log(Australia.CD)
#Assigning variables - End#

#Interpreting Mean Value - Start#
mean(Australia.SI) 
mean(Australia.RI) 
sd(Australia.SI)
#Interpreting Mean Value - End#

#Normalization & Checking for normal distribution - Start#
Australia.RI.norm <- rnorm(Australia.RI)
hist(Australia.RI.norm)

Australia.SI.norm <- rnorm(Australia.SI)
hist(Australia.SI.norm)

Australia.NC.norm <- rnorm(Australia.NC)
hist(Australia.NC.norm)
#Normalization & Checking for normal distribution - End#

#Identifying relation between dates & New cases - Start#
qplot(Australia.DT, Australia.NC, colour = Australia.SI, data=Australia.master, geom = c("point","line"))
#Identifying relation between dates & New cases - End#

#Train & test model for multi-linear regression - Start#
set.seed(1234)
model.data <- sample(2, nrow(Australia.master), replace=TRUE, prob = c(0.7,0.3))

#Splitting the data intro train & test - Start#
train <- Australia.master[model.data==1,]
test <- Australia.master[model.data==2,]
#Splitting the data intro train & test - End#

#Assigning variables - Start#
Australia.C1 <- Australia.master$`C1_School closing`
Australia.C2 <- Australia.master$`C2_Workplace closing`
Australia.C3 <- Australia.master$`C3_Cancel public events`
Australia.C4 <- Australia.master$`C4_Restrictions on gatherings`
Australia.C5 <- Australia.master$`C5_Close public transport`
Australia.C6 <- Australia.master$`C6_Stay at home requirements`
Australia.C7 <- Australia.master$`C7_Restrictions on internal movement`
Australia.C8 <- Australia.master$C8_International_travel_controls
Australia.H1 <- Australia.master$`H1_Public information campaigns`

Australia.C1.flag <- Australia.master$C1_Flag
Australia.C2.flag <- Australia.master$C2_Flag
Australia.C3.flag <- Australia.master$C3_Flag
Australia.C4.flag <- Australia.master$C4_Flag
Australia.C5.flag <- Australia.master$C5_Flag
Australia.C6.flag <- Australia.master$C6_Flag
Australia.C7.flag <- Australia.master$C7_Flag
Australia.H1.flag <- Australia.master$H1_Flag
#Assigning variables - End#

#Checking the fit of the model - Start of iteration-1#
Linear_1 <- Australia.master$StringencyIndex~Australia.C1+Australia.C2+Australia.C3+Australia.C4+Australia.C5+Australia.C6+Australia.C7+
  Australia.C8+Australia.H1+Australia.C1.flag+Australia.C2.flag+Australia.C3.flag+Australia.C4.flag+Australia.C5.flag+
  Australia.C6.flag+Australia.C7.flag+Australia.H1.flag
OLS_1 <- lm(Linear_1, data = train)
summary(OLS_1)
#Checking the fit of the model - End of iteration-1#

#Checking the fit of the model - Start of iteration-2#
Linear_2 <- Australia.master$StringencyIndex~Australia.C1+Australia.C2+Australia.C3+Australia.C4+Australia.C5+Australia.C6+Australia.C7+
  Australia.C8+Australia.H1+Australia.C1.flag+Australia.C2.flag+Australia.C3.flag+Australia.C4.flag+
  Australia.C6.flag+Australia.H1.flag
OLS_2 <- lm(Linear_2, data = train)
summary(OLS_2)
vif(OLS_2)
#Checking the fit of the model - End of iteration-2#

#Checking the fit of the model - Start of iteration-3#
Linear_3 <- Australia.master$StringencyIndex~Australia.C1+Australia.C2+Australia.C3+Australia.C5+Australia.C6+Australia.C7+
  Australia.C8+Australia.H1+Australia.C1.flag+Australia.C2.flag+Australia.C3.flag+Australia.C4.flag+
  Australia.C6.flag+Australia.H1.flag
OLS_3 <- lm(Linear_3, data = train)
summary(OLS_3)
vif(OLS_3)
#Checking the fit of the model - End of iteration-3#

#Checking the fit of the model - Start of iteration-4#
Linear_4 <- Australia.master$StringencyIndex~Australia.C7+
  Australia.C8+Australia.C1.flag+Australia.C6.flag
OLS_4 <- lm(Linear_4, data = train)
summary(OLS_4)
vif(OLS_4)
#Checking the fit of the model - End of iteration-4#

#Finding the MSE value - Start#
Pred <- predict(OLS_4,test)
MSE <- mean((Pred-test$StringencyIndex)^2)
MSE
#Finding the MSE value - End#
#Train & test model for multi-linear regression - End#

#Visualizing the effect of lockdown (Stringency Index) on new cases - Start#
qplot(Australia.DT, Australia.SI,  colour = Australia.SI, data = Australia.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on new cases - End#

#Visualizing the effect of lockdown (Stringency Index) on confirmed cases - Start#
qplot(Australia.DT, Australia.LCC, colour = Australia.SI, data = Australia.master, geom = c("point","line"))
qplot(Australia.DT, Australia.CC,  colour = Australia.SI, data = Australia.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on confirmed cases - End#

#Visualizing the effect of lockdown (Stringency Index) on confirmed deaths - Start#
qplot(Australia.DT, Australia.LCD, colour = Australia.SI, data = Australia.master, geom = c("point","line"))
qplot(Australia.DT, Australia.CD,  colour = Australia.SI, data = Australia.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on confirmed deaths - End#

###################################################################################
######################### Model for Australia - End ###############################
###################################################################################

###################################################################################
######################### Model for India - Start #################################
###################################################################################

#Assigning variables - Start#
india.SI  <- india.master$StringencyIndex
india.RI  <- india.master$RateOfInfection
india.NC  <- india.master$NewCases
india.LNC <- log(india.NC)
india.CC  <- india.master$ConfirmedCases
india.LCC <- log(india.CC)
india.DT  <- india.master$Actual_Date
india.CD  <- india.master$ConfirmedDeaths
india.LCD <- log(india.CD)
#Assigning variables - End#

#Interpreting Mean Value - Start#
mean(india.SI)
mean(india.RI)
sd(india.SI)
#Interpreting Mean Value - End#

#Normalization & Checking for normal distribution - Start#
india.RI.norm <- rnorm(india.RI)
hist(india.RI.norm)

india.SI.norm <- rnorm(india.SI)
hist(india.SI.norm)

india.NC.norm <- rnorm(india.NC)
hist(india.NC.norm)
#Normalization & Checking for normal distribution - End#

#Identifying relation between dates & New cases - Start#
qplot(india.DT, india.NC, colour = india.SI, data=india.master, geom = c("point","line"))
#Identifying relation between dates & New cases - End#

#Train & test model for multi-linear regression - Start#
set.seed(5698547)
model.data <- sample(2, nrow(india.master), replace=TRUE, prob = c(0.7,0.3))

#Splitting the data intro train & test - Start#
train <- india.master[model.data==1,]
test <- india.master[model.data==2,]
#Splitting the data intro train & test - End#

#Assigning variables - Start#
india.C1 <- india.master$`C1_School closing`
india.C2 <- india.master$`C2_Workplace closing`
india.C3 <- india.master$`C3_Cancel public events`
india.C4 <- india.master$`C4_Restrictions on gatherings`
india.C5 <- india.master$`C5_Close public transport`
india.C6 <- india.master$`C6_Stay at home requirements`
india.C7 <- india.master$`C7_Restrictions on internal movement`
india.C8 <- india.master$C8_International_travel_controls
india.H1 <- india.master$`H1_Public information campaigns`

india.C1.flag <- india.master$C1_Flag
india.C2.flag <- india.master$C2_Flag
india.C3.flag <- india.master$C3_Flag
india.C4.flag <- india.master$C4_Flag
india.C5.flag <- india.master$C5_Flag
india.C6.flag <- india.master$C6_Flag
india.C7.flag <- india.master$C7_Flag
india.H1.flag <- india.master$H1_Flag
#Assigning variables - End#

#Checking the fit of the model - Start of iteration-1#
Linear_1 <- india.master$StringencyIndex~india.C1+india.C2+india.C3+india.C4+india.C5+india.C6+india.C7+
  india.C8+india.H1+india.C1.flag+india.C2.flag+india.C3.flag+india.C4.flag+india.C5.flag+
  india.C6.flag+india.C7.flag+india.H1.flag
OLS_1 <- lm(Linear_1, data = train)
summary(OLS_1)
#Checking the fit of the model - End of iteration-1#

#Checking the fit of the model - Start of iteration-2#
Linear_2 <- india.master$StringencyIndex~india.C1+india.C2+india.C3+india.C4+india.C5+india.C6+
  india.C8+india.H1+india.C1.flag+india.C2.flag+india.C3.flag+india.C4.flag+india.C5.flag+
  india.C6.flag+india.C7.flag+india.H1.flag
OLS_2 <- lm(Linear_2, data = train)
summary(OLS_2)
vif(OLS_2)
#Checking the fit of the model - End of iteration-2#

#Checking the fit of the model - Start of iteration-3#
Linear_3 <- india.master$StringencyIndex~india.C2+india.C3+india.C4+india.C5+india.C6+
  india.C8+india.H1+india.C1.flag+india.C2.flag+india.C3.flag+india.C4.flag+india.C5.flag+
  india.C6.flag+india.C7.flag+india.H1.flag
OLS_3 <- lm(Linear_3, data = train)
summary(OLS_3)
vif(OLS_3)
#Checking the fit of the model - End of iteration-3#

#Checking the fit of the model - Start of iteration-4#
Linear_4 <- india.master$StringencyIndex~india.C2+india.C6+india.C1.flag+india.C3.flag+india.C6.flag+india.C7.flag
OLS_4 <- lm(Linear_4, data = train)
summary(OLS_4)
vif(OLS_4)
#Checking the fit of the model - End of iteration-4#

#Finding the MSE value - Start#
Pred <- predict(OLS_2,test)
MSE <- mean((Pred-test$StringencyIndex)^2)
MSE
#Finding the MSE value - End#
#Train & test model for multi-linear regression - End#

#Visualizing the effect of lockdown (Stringency Index) on new cases - Start#
qplot(india.DT, india.SI,  colour = india.SI, data = india.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on new cases - End#

#Visualizing the effect of lockdown (Stringency Index) on confirmed cases - Start#
qplot(india.DT, india.LCC, colour = india.SI, data = india.master, geom = c("point","line"))
qplot(india.DT, india.CC,  colour = india.SI, data = india.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on confirmed cases - End#

#Visualizing the effect of lockdown (Stringency Index) on confirmed deaths - Start#
qplot(india.DT, india.LCD, colour = india.SI, data = india.master, geom = c("point","line"))
qplot(india.DT, india.CD,  colour = india.SI, data = india.master, geom = c("point","line"))
#Visualizing the effect of lockdown (Stringency Index) on confirmed deaths - End#

###################################################################################
######################### Model for India - End ###################################
###################################################################################