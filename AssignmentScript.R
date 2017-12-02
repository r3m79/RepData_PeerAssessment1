####################################################
# Script Assignment
####################################################
####################################################
# Script must be executed in same folder as dataset
# for reading the Data, package sqldf will be used

#load libraries
library(dplyr)
library(ggplot2)
library(lubridate)

#check if file exists
dataFile<-"./activity.csv"
if (!file.exists(dataFile)) { 
    stop("Dataset file not present!") 
}

#read data
activitySet<-read.csv(file = dataFile, header = TRUE)

#Calculate the total number of steps taken per day
#Make a histogram of the total number of steps taken each day
#Calculate and report the mean and median of the total number of steps taken
groupActivity<-group_by(activitySet,date)
summaryActivity<-summarize(groupActivity,total=sum(steps,na.rm = TRUE))
hist(summaryActivity$total,main="Histogram for Total Steps per Day",xlab="Total Steps")
meanSteps<-mean(summaryActivity$total,na.rm = TRUE)
medianSteps<-median(summaryActivity$total,na.rm = TRUE)
meanSteps
medianSteps

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
interval <- activitySet %>%
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarize(steps = mean(steps))
pl<-ggplot(interval, aes(interval,steps)) 
pl + labs(x="Interval",y ="Average Steps", 
          title ="Average Steps in 5m Interval") + geom_line()
#Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
interval$interval[which.max(interval$steps)]

#Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
totalNA<-as.numeric(count(activitySet[is.na(activitySet),]))
totalNA
        
#Devise a strategy for filling in all of the missing values in the dataset.
#Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
# Replace NA with mean value
activitySetFinal <- activitySet
activityNA <- is.na(activitySetFinal$steps)
avg_interval <- tapply(activitySetFinal$steps, activitySetFinal$interval, mean, na.rm=TRUE, simplify=TRUE)
activitySetFinal$steps[activityNA] <- avg_interval[as.character(activitySetFinal$interval[activityNA])]

#Make a histogram of the total number of steps taken each day and 
groupActivityFinal<-group_by(activitySetFinal,date)
summaryActivityFinal<-summarize(groupActivityFinal,total=sum(steps))
hist(summaryActivityFinal$total,main="Histogram for Total Steps per Day without NA",xlab="Total Steps")
# calculate and report the mean and median total number of steps taken per 
# day.
meanSteps<-mean(summaryActivityFinal$total)
medianSteps<-median(summaryActivityFinal$total)
meanSteps
medianSteps

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
activitySetFinal$weektype<-sapply(activitySetFinal$date,FUN=function(x){ if ( wday(x) == 7 | wday(x) == 6 ) { "weekend" } else { "weekday"} })
head(activitySetFinal)
#Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). 
intervalFinal <- activitySetFinal %>%
    filter(!is.na(steps)) %>%
    group_by(interval,weektype) %>%
    summarize(steps = mean(steps))
pl<-ggplot(intervalFinal, aes(interval,steps,
                         color=weektype)) 
pl + labs(x="Interval",y ="Average Steps", 
          title ="Average Steps in 5m Interval") + 
    geom_line() + 
    facet_wrap(~weektype, ncol = 1, nrow=2)
                            
