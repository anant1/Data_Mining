#HW1-1
rm(list = ls())
setwd("/Users/anant/Google Drive/MASTERS/Courses/545/Codes")

# Install the ISLR package
install.packages('ISLR')
library('ISLR')
library('ggplot2')

# We remove the name column as it does not have any major contribution to the prediction of mpg
dataframe <- subset.data.frame(Auto, select = mpg:origin)

################################
#######Histograms
################################

histogramCyl <- ggplot(dataframe, aes(x = cylinders) ) +geom_histogram( aes(y = ..density..), binwidth = 1, alpha=0.7, col="red") + geom_density(col="blue")
histogrammpg <- ggplot(dataframe, aes(x = mpg) ) +geom_histogram( aes(y = ..density..), binwidth = 7, alpha=0.7, col="red") + geom_density(col="blue")
histogramdispl <- ggplot(dataframe, aes(x = displacement) ) +geom_histogram( aes(y = ..density..), binwidth = 50, alpha=0.7, col="red") + geom_density(col="blue")
histogramhrp <- ggplot(dataframe, aes(x = horsepower) ) +geom_histogram( aes(y = ..density..), binwidth = 50, alpha=0.7, col="red") + geom_density(col="blue")
histogramwgt <- ggplot(dataframe, aes(x = weight) ) +geom_histogram( aes(y = ..density..), binwidth = 200, alpha=0.7, col="red") + geom_density(col="blue")
histogramaccltn <- ggplot(dataframe, aes(x = acceleration) ) +geom_histogram( aes(y = ..density..), binwidth = 1, alpha=0.7, col="red") + geom_density(col="blue")
histogramyr <- ggplot(dataframe, aes(x = year) ) +geom_histogram( aes(y = ..density..), binwidth = 1, alpha=0.7,col="red") + geom_density(col="blue")
histogramorigin <- ggplot(dataframe, aes(x = origin) ) +geom_histogram( aes(y = ..density..), binwidth = 1, alpha=0.7,col="red") + geom_density(col="blue")

################################
#######Stem and leaf plot
################################

stem(Auto$mpg)

################################
#######Boxplot
################################

boxCylinders  = boxplot(mpg~cylinders,data=dataframe, main="Car Milage", xlab="Number of Cylinders", ylab="Miles Per Gallon")
boxweight  = boxplot(mpg~weight,data=dataframe, main="Car Milage", xlab="Weight", ylab="Miles Per Gallon")
boxYear = boxplot(mpg~year,data=dataframe, main="Car Milage", xlab="Years", ylab="Miles Per Gallon")
boxOrigin = boxplot(mpg~origin,data=dataframe, main="Car Milage", xlab="Origin", ylab="Miles Per Gallon")

################################
#######ScatterPlot
################################

ggpairs(dataframe, diag=list(continuous="density", discrete="bar"), axisLabels="show")


################################
#######Romoving the outliers
################################
par(mfrow = c(1,2))
dataframe <- dataframe[!((dataframe$mpg>=40 & dataframe$cylinders ==4) + (dataframe$mpg>= 35 & dataframe$origin == 1) + (dataframe$mpg>= 45 & dataframe$origin == 3)),]
boxCylinders2  = boxplot(mpg~cylinders,data=dataframe, main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon")
boxOrigin2 = boxplot(mpg~origin,data=dataframe, main="Car Milage Data", xlab="origin", ylab="Miles Per Gallon")


################################
#######Saving the data file in .RData format
################################

save(dataframe, file = "cleansedData.RData")
