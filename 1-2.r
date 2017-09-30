#HW1-2
rm(list = ls())
setwd("/Users/anant/Google Drive/MASTERS/Courses/545/Codes")

#############################################################
#####Loading the cleansed data into a dataframe
#############################################################

df = load("cleansedData.RData")

#############################################################
#####Multiple regression on the pre-processed Auto dataset
#############################################################

linear_model1 <- lm(mpg ~ ., data=dataframe)
summary <- summary(linear_model1)
coef <- summary$coefficients

#############################################################
#####Finding significant relationship
#############################################################

#Maximum Significance
maxPr <- which.min(coef[,"Pr(>|t|)"])
### Weight and year predictors have maximum significant relationship to mpg

#Minimum Significance
minPr <- which.max(coef[,"Pr(>|t|)"])
### Acceleration have the minimum significant relationship to mpg


#############################################################
#####Fit models with interactions 
#############################################################

linear_model2 <- lm(mpg ~ cylinders+weight+horsepower+acceleration+displacement+origin+year + 
					(cylinders*weight) + (cylinders*horsepower) + (cylinders*acceleration) + (cylinders*displacement) + (cylinders*origin) +(cylinders*year)+
					(weight*horsepower) + (weight*acceleration) + (weight*displacement) + (weight*origin) + (weight*year)+
					(horsepower*acceleration) + (horsepower*displacement) + (horsepower*origin) + (horsepower*year)+
					(acceleration*displacement) + (acceleration*origin) + (acceleration*year)+
					(displacement*origin) + (displacement*year)+ (origin*year), data=dataframe)
summary(linear_model2)