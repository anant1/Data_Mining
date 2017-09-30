#HW1-4
rm(list = ls())
setwd("/Users/anant/Google Drive/MASTERS/Courses/545/Codes")

install.packages("MASS")
library(MASS)
library(ggplot2)

db <- as.data.frame(Boston)
lin <- lm(medv ~ . , data = db)
summary(lin)


######################################
###Scatterplots for all predictors
#######################################
ggpairs(Boston, diag=list(continuous="density", discrete="bar"), axisLabels="show")

#################################
###crime rate scatterplot
##################################
plot(Boston$age, Boston$crim)
plot(Boston$dis, Boston$crim)
plot(Boston$rad, Boston$crim)
plot(Boston$tax, Boston$crim)
plot(Boston$ptratio, Boston$crim)


############################################
###Analysis for crime
############################################

hist(Boston$crim[Boston$crim>1], breaks = 0 + (0:10)*10, xlab ="Crime Rate", main = "Boston crime rate with breaks at 0,10")

length(Boston$chas[Boston$crim>20])
min(Boston$crim[Boston$crim>1])
max(Boston$crim[Boston$crim>1])

#######################################
###Analysis for Tax Rates 
########################################
hist(Boston$tax[Boston$tax>1], breaks = 150 + (0:8)*100, xlab ="Tax Rate", main = "Boston tax rate with breaks at 150,250")
length(Boston$chas[Boston$tax>400 & Boston$chas==0])
length(Boston$chas[Boston$tax>400 & Boston$chas==1])
max(Boston$tax[Boston$tax>1])
min(Boston$tax[Boston$tax>1])


########################################
###Analysis for Pupil Teacher ratio
########################################

hist(Boston$ptratio[Boston$ptratio>1], breaks = 10 + (0:10)*2.5, xlab ="Pupil-Teacher Ratio", main = "Boston pupil teacher ratio with breaks at 10,12.5..")

min(Boston$ptratio[Boston$ptratio>1])
max(Boston$ptratio[Boston$ptratio>1])


###Rooms per dwelling analysis

length(Boston$chas[Boston$rm>7 & Boston$chas==0])
length(Boston$chas[Boston$rm>7 & Boston$chas==1])
length(Boston$chas[Boston$rm>8 & Boston$chas==0])
length(Boston$chas[Boston$rm>8 & Boston$chas==1])
