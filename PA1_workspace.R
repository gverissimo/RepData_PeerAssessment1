## clear the workspace
ls() # show workspace
rm(list=ls()) # clear workspace
ls() # show empty workspace

## load libraries
library(reshape2)
library(ggplot2)
library(dplyr)

## Load dataframe from .csv file
activity <- read.csv("./data/activity.csv",
                     header=TRUE, 
                     colClasses=c(
                             "numeric",   ## steps: number of steps taken during 5min sub-interval of a day
                             "character", ## date: the specific day of sample 'steps'
                             "numeric"    ## interval: the specific 5min sub-interval of sample 'steps'
                     )
)
## convert string ("YYYY-MM-DD") to class 'date' and then to 'factor'
activity$date <- as.factor(as.Date(activity$date, format="%Y-%m-%d"))
## convert interval to class 'factor'
activity$interval <- as.factor(activity$interval)


## What is mean total number of steps taken per day?
##For this part of the assignment, you can ignore the missing values in the dataset.
##    * Calculate the total number of steps taken per day
##    * Make a histogram of the total number of steps taken each day. If you do not 
##      understand the difference between a histogram and a barplot, research the 
##      difference between them.*
##    * Calculate and report the mean and median of the total number of steps taken 
##      per day
        
## Calculate steps per day (ignoring NA)
stepsPerDay <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)

## Make a histogram of the total number of steps taken each day. If you do not 
## understand the difference between a histogram and a barplot, research the difference
## between them.
par(mfrow=c(1,1), mar=c(4,4,4,2), oma=c(1,1,1,0))
histInfo <- hist(stepsPerDay$steps, 
     xlim=c(0,25000), breaks = 25,
     col = "sky blue", border = "white",
     main="Histogram of Total Steps per Day",
     xlab="Steps", ylab="Frequency")
par(mfrow=c(1,1), mar=c(4,4,4,4), oma=c(0,0,0,0))
histInfo[c("mids", "counts")]

meanStepsPerDay <- format(round(mean(stepsPerDay$steps), 1), 
                          scientific=FALSE, nsmall=1, big.mark=",")
meanStepsPerDay
medianStepsPerDay <- format(round(median(stepsPerDay$steps), 1), 
                            scientific=FALSE, nsmall=1, big.mark=",")
medianStepsPerDay


## What is the average daily activity pattern?
##  -- Make a time series plot (i.e. - type = "l") of the 5-minute interval (x-axis) 
##     and the average number of steps taken, averaged across all days (y-axis)
##  -- Which 5-minute interval, on average across all the days in the dataset, contains
##     the maximum number of steps?

## g steps by interval
averageSteps <- aggregate(steps ~ interval, data = activity, mean, na.rm=TRUE)

par(mfrow=c(1,1), mar=c(4,4,4,2), oma=c(1,1,1,0))
plot(as.numeric(as.character(averageSteps$interval)), averageSteps$steps, 
     type = "l", 
     lwd = 2, col = "deep sky blue 2",
     main = "Steps per 5min Interval Averaged Across All Days",
     xlab = "5-min interval", ylab = "Average steps per interval" )
par(mfrow=c(1,1), mar=c(4,4,4,4), oma=c(0,0,0,0))

maxAverageSteps <- averageSteps[which.max(averageSteps$steps), ]

## Imputing missing values
## Note that there are a number of days/intervals where there are missing values (coded 
##  as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into 
## some calculations or summaries of the data.
##    * Calculate and report the total number of missing values in the dataset 
##      (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
##    * Devise a strategy for filling in all of the missing values in the dataset. 
##      The strategy does not need to be sophisticated. For example, you could use 
##      the mean/median for that day, or the mean for that 5-minute interval, etc.
##    * Create a new dataset that is equal to the original dataset but with the 
##      missing data filled in.
##    * Make a histogram of the total number of steps taken each day and Calculate 
##      and report the mean and median total number of steps taken per day. 
##        - Do these values differ from the estimates from the first part 
##          of the assignment? 
##        - What is the impact of imputing missing data on the estimates 
##          of the total daily number of steps?

##  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
missingValues <- sum(is.na(activity$steps))
missingValues

##  Devise a strategy for filling in all of the missing values in the dataset. 
##   - The strategy does not need to be sophisticated. For example, you could use 
##      the mean/median for that day, or the mean for that 5-minute interval, etc.

## reshape long->wide to facilitate impute of NAs
activityWide <- dcast(activity, interval ~ date, value.var="steps")

## setup loop variables
firstDate <- which(colnames(activityWide)=="2012-10-01")
lastDate <- which(colnames(activityWide)=="2012-11-30")
activityWideImputed <- activityWide

## loop over daterange (Oct/1 to Nov/30)
for (i in firstDate:lastDate) {
        ## identify NA's for loop date
        missingVal <- is.na(activityWideImputed[ ,i])
        ## replace interval NA's with average steps for that interval (rounded to whole steps)
        activityWideImputed[missingVal, i] <- round(averageSteps$steps[missingVal],0)
}

## reshape wide->long
activityImputed <- melt(activityWideImputed, 
                        id.vars=c("interval"), 
                        variable.name="date",
                        value.name="steps")

## Calculate steps per day (imputed NA)
stepsPerDayImputed <- aggregate(steps ~ date, data = activityImputed, sum, na.rm = TRUE)
averageStepsImputed <- aggregate(steps ~ interval, data = activityImputed, mean, na.rm=TRUE)

## Make a histogram of the total number of steps taken each day. If you do not understand the difference between a histogram and a barplot, research the difference between them.*

par(mfrow=c(1,2), mar=c(4,4,4,2), oma=c(1,1,1,0))

plot(as.numeric(as.character(averageSteps$interval)), averageSteps$steps, 
     type = "l", 
     lwd = 2, col = "deep sky blue 2",
     main = "Average Steps/Interval \n",
     xlab = "5-min interval", ylab = "Number of Steps/Interval" )
plot(as.numeric(as.character(averageStepsImputed$interval)), averageStepsImputed$steps, 
     type = "l", 
     lwd = 2, col = "deep sky blue 2",
     main = "Average Steps/Interval \n(imputed)",
     xlab = "5-min interval", ylab = "Number of Steps/Interval" )

hist(stepsPerDay$steps, 
     xlim=c(0,25000), breaks = 25,
     ylim=c(0,20),
     col = "sky blue", border = "white",
     main="Histogram of Total Steps/Day \n",
     xlab="Number of Steps/Day", ylab="Frequency")
hist(stepsPerDayImputed$steps, 
     xlim=c(0,25000), breaks = 25,
     ylim=c(0,20),
     col = "sky blue", border = "white",
     main="Histogram of Total Steps/Day \n(imputed)",
     xlab="Number of Steps/Day", ylab="Frequency")

par(mfrow=c(1,1), mar=c(4,4,4,4), oma=c(0,0,0,0))

meanStepsPerDayImputed <- format(round(mean(stepsPerDayImputed$steps), 1), 
                          scientific=FALSE, nsmall=1, big.mark=",")
meanStepsPerDayImputed
meanStepsPerDay
medianStepsPerDayImputed <- format(round(median(stepsPerDayImputed$steps), 1), 
                            scientific=FALSE, nsmall=1, big.mark=",")
meanStepsPerDayImputed
medianStepsPerDay

## Are there differences in activity patterns between weekdays and weekends?
##   * For this part the weekdays() function may be of some help here. 
##   * Use the dataset with the filled-in missing values for this part.
##   * Create a new factor variable in the dataset with two levels – “weekday” 
##     and “weekend” indicating whether a given date is a weekday or weekend day.
##   * Make a panel plot containing a time series plot (i.e. - type = "l" of the 
##     5-minute interval (x-axis) and the average number of steps taken, averaged 
##     across all weekday days or weekend days (y-axis). 
## See the README file in the GitHub repository to see an example of what this plot 
## should look like using simulated data.

activityImputed2 <- activityImputed
activityImputed2$weekDayEnd <- ifelse(weekdays(as.Date(activityImputed2$date)) %in% c("Saturday", "Sunday"),
                                      "Weekend", 
                                      "Weekday"
                                      )
activityImputed2$weekDayEnd <- as.factor(activityImputed2$weekDayEnd)

stepsPerDayImputed2 <- aggregate(steps ~ interval + weekDayEnd, activityImputed2, mean)

pl <- ggplot(filter(stepsPerDayImputed2, weekDayEnd %in% c("Weekend", "Weekday")),
             aes(x=as.numeric(as.character(interval)), y=steps, color=weekDayEnd) ) +
        geom_line() +
        facet_grid(weekDayEnd ~ ., scales="fixed", space="fixed") +
        labs(x="interval", 
             y="number of steps/day",
             color= "Part of Week",
             title="Average Steps/Interval",
             subtitle="Comparison of Weekends vs. Weekdays"
             ) +
        theme(
                legend.position = "none",
                plot.margin = unit(c(1.5,1,1.5,1), "cm"),
                strip.text.y = element_text(face="bold")
                )
print(pl)



