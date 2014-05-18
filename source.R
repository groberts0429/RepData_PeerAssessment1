# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
setwd("./activity")
activity <- read.csv("activity.csv")
dailySteps <- tapply(activity$steps, activity$date, sum)
hist(dailySteps, main="Number of Steps by Day", xlab="Steps")


## What is mean total number of steps taken per day?
meanSteps <- mean(daily_steps, na.rm=TRUE)
meanSteps

medianSteps <- median(daily_steps, na.rm=TRUE)
medianSteps

## What is the average daily activity pattern?
adap = tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
adap # Raw Data
plot(adap, type="l", xlab="5 Minute Interval", ylab="Average Number of Steps (not counting NA)")   # Line Plot


## Imputing missing values
activityNA = is.na(activity)
activityImputed = activity
adapInterval = as.numeric(names(adap))
adapImputedSteps = as.numeric(adap)
imp_table = data.frame(adapInterval, adapImputedSteps)
#activityImputed$steps[activityNA] = imp_table[imp_table$adapInterval == activityImputed$Interval[activityNA], 2]
for (i in range 1:nrow(activity)) {
  if (activityNA) {
    activityImputed$steps[i] = imp_table[imp_table$adapInterval == activityImputed$interval[i], 2]
  }
}


## Are there differences in activity patterns between weekdays and weekends?
