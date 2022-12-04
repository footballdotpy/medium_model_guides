#import libaries

library(tidyverse)
library(ggplot2)
library(car)
library(lmtest)
library(performance)
library(see)
library(patchwork)
library(DAAG)


#set wd to same as data file location.

setwd('C:/Users/paulc/Documents/Medium Football/Model Tutorials')

# import csv

data <- read.csv('football_data.csv')

# make a smaller dataset using subset of columns.

# specify the columns we want.

columns <- c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","HS","HST","AS","AST","HC","AC")

# apply the subset.
data <- data[columns]

# lets rename the columns.

data <- data %>%
        rename("League" = "Div",
               "HomeGoals" = "FTHG",
               "AwayGoals" = "FTAG",
               "HomeShots" = "HS",
               "HomeSOT" = "HST",
               "AwayShots" = "AS",
               "AwaySOT" = "AST",
               "HomeCorners" = "HC",
               "AwayCorners" = "AC")

# create new totals columns

data$TotalGoals <- data$HomeGoals + data$AwayGoals
data$TotalShots <- data$HomeShots + data$AwayShots
data$TotalCorners <- data$HomeCorners + data$AwayCorners
data$TotalSOT<- data$HomeSOT + data$AwaySOT
# scatter plots for relationship testing

# scatterplots
ggplot(data, aes(x=TotalShots, y=TotalGoals)) +
  geom_point(size=3, shape=23, color = 'grey') +  ggtitle("Shots versus Goals") +
  theme(axis.text.x = element_text(angle=45,size = 18)) +
  theme(axis.text.y = element_text(angle=90,size = 18)) +
  theme(text = element_text(size=20)) +
  geom_smooth(method=lm,color="red")



# scatterplots
ggplot(data, aes(x=TotalShots, y=TotalCorners)) +
  geom_point(size=3, shape=23, color = 'grey') +  ggtitle("Shots versus Corners") +
  theme(axis.text.x = element_text(angle=45,size = 18)) +
  theme(axis.text.y = element_text(angle=90,size = 18)) +
  theme(text = element_text(size=20)) +
  geom_smooth(method=lm,color="red")


# scatterplots
ggplot(data, aes(x=TotalCorners, y=TotalGoals)) +
  geom_point(size=3, shape=23, color = 'grey') +  ggtitle("Corners versus Goals") +
  theme(axis.text.x = element_text(angle=45,size = 18)) +
  theme(axis.text.y = element_text(angle=90,size = 18)) +
  theme(text = element_text(size=20)) +
  geom_smooth(method=lm,color="red")


# scatterplots
ggplot(data, aes(x=TotalSOT, y=TotalGoals)) +
  geom_point(size=3, shape=23, color = 'grey') +  ggtitle("SOT versus Goals") +
  theme(axis.text.x = element_text(angle=45,size = 18)) +
  theme(axis.text.y = element_text(angle=90,size = 18)) +
  theme(text = element_text(size=20)) +
  geom_smooth(method=lm,color="red")




# specify the columns we want for linear model.

columns <- c("TotalGoals","TotalSOT")

# apply the subset.
data <- data[columns]



# check for normal distribution

#create histogram for both variables
hist(data$TotalGoals, col='steelblue', main='Normal Distribution of Goals')

#create histogram for both variables
hist(data$TotalSOT, col='steelblue', main='Normal Distribution of SOT')




#perform kolmogorov-smirnov test because of large dataset.
ks.test(data$TotalGoals, 'pnorm')



# outliers
boxplot(data$TotalGoals,
  ylab = "TotalGoals"
)

# outliers
boxplot(data$TotalSOT,
  ylab = "TotalSOT"
)

# replace outliers with median values.

goals_outliers <- boxplot(data$TotalGoals)$out
sot_outliers <-boxplot(data$TotalSOT)$out


data$TotalGoals[data$TotalGoals %in% goals_outliers] <- median(data$TotalGoals)
data$TotalSOT[data$TotalSOT %in% sot_outliers] <- median(data$TotalSOT)

# confirm the outliers are gone.


boxplot(data$TotalGoals,
  ylab = "TotalGoals"
)

boxplot(data$TotalSOT,
  ylab = "TotalSOT"
)

# apply transformations for non normally distributed data.

data$TotalGoals_log <- log10(data$TotalGoals)
data$TotalSOT_log <- log10(data$TotalSOT)

# check for normal distribution

#create histogram for both variables
hist(data$TotalGoals_log, col='steelblue', main='Normal Distribution of Goals')

#create histogram for both variables
hist(data$TotalSOT_log, col='steelblue', main='Normal Distribution of SOT')

#create histogram to check distribution after outlier removal.
hist(data$TotalGoals, col='steelblue', main='Normal Distribution of Goals')

#create histogram to check distribution after outlier removal.
hist(data$TotalSOT, col='steelblue', main='Normal Distribution of SOT')

# specify the columns we want for linear model. Getting back to original values.

columns <- c("TotalGoals","TotalSOT")

# apply the subset.
data <- data[columns]
# remove null values

data <- na.omit(data)

# create the linear regression model on training set.

model <-  lm(TotalGoals ~ TotalSOT, data=data)


#check for auto correlation of errors.
#perform Durbin-Watson test
durbinWatsonTest(model)

# check for Heteroscedasticity

#perform Breusch-Pagan Test
bptest(model)

#summary output of model

summary(model)

# residual plots
check_model(model)


# cross validation - DAAG


par(mfrow = c(2,2)) # display a unique layout for all graphs

CV_DATA <-CVlm(data = data, form.lm = formula(TotalGoals ~ TotalSOT),
         m = 10, plotit = c("Observed","Residual"),
         main="Total Goals Model",
         legend.pos="topleft", printit = FALSE)


summary(lm(CV_DATA$TotalGoals~CV_DATA$TotalSOT))


#made up values

new_sot <- data.frame(TotalSOT=c(10,2,5,7,3,16,1,6,4,8))

predict(model,newdata=new_sot)