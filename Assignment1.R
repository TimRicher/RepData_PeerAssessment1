###############################################################################
# This program generates the plot1.png graph 
# Question 1:  Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? Using the base plotting system, make a plot showing the 
# total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, 
# and 2008.
# Author: Tim Richer
###############################################################################

setwd("C:/Users/TimRicher/Documents/GitHub/RepData_PeerAssessment1")

library(ggplot2)
library(lattice)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zip <- "activity.zip"
if(!file.exists(zip)){
	download.file(url, zip, mode="wb")
}

activityZip <- paste(getwd(), "/activiy.zip", sep = "")

if(!file.exists(activityZip)){
	unzip(zip, list = FALSE, overwrite = TRUE, exdir = ".")
}

activityMonitoringData <- read.csv("activity.csv")

steps.date <- aggregate(steps ~ date, data = activityMonitoringData, FUN = sum,
                        na.rm = TRUE)

ggplot(steps.date, aes(x = steps)) + geom_histogram(fill = "blue", binwidth = 1000) + 
       ggtitle("Histogram of Steps Taken per Day") +
       labs(x="Number of Steps per Day",y="Number of times in a day") + theme_bw() 

mean(steps.date$steps)
median(steps.date$steps)

steps.interval <- aggregate(steps ~ interval, data=activityMonitoringData, FUN=mean)
plot(steps.interval, type="l")

steps.interval$interval[which.max(steps.interval$steps)]

sum(is.na(activityMonitoringData))

StepsIntervalMean <- aggregate(steps ~ interval, data = activityMonitoringData, FUN = mean)
imputeNA <- numeric()
for (i in 1:nrow(activityMonitoringData)) {
    activity_obs <- activityMonitoringData[i, ]
    if (is.na(activity_obs$steps)) {
        steps <- subset(StepsIntervalMean, interval == activity_obs$interval)$steps
    } else {
        steps <- activity_obs$steps
    }
    imputeNA <- c(imputeNA, steps)
}
imputed_activityMonitoringData <- activityMonitoringData
imputed_activityMonitoringData$steps <- imputeNA

steps.date <- aggregate(steps ~ date, data = imputed_activityMonitoringData, FUN = sum,
                        na.rm = TRUE)

ggplot(steps.date, aes(x = steps)) + geom_histogram(fill = "blue", binwidth = 1000) + 
       ggtitle("Histogram of Steps Taken per Day") +
       labs(x="Number of Steps per Day",y="Number of times in a day") + theme_bw() 

mean(steps.date$steps)
median(steps.date$steps)

daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
imputed_activityMonitoringData$daytype <- as.factor(sapply(imputed_activityMonitoringData$date, daytype))

stepsByDayType <- aggregate(steps ~ interval + daytype, data = imputed_activityMonitoringData, mean)
names(stepsByDayType) <- c("interval", "daytype", "steps")
xyplot(steps ~ interval | daytype, stepsByDayType, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")	   

