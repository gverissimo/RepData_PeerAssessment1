## clear the workspace
ls() # show workspace
rm(list=ls()) # clear workspace
ls() # show empty workspace


## Overview  

#### *Assignment:*
## It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.*
##         
##         *This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.*
##         
##         
##         The raw dataset for this assignment 
## 
## * Was downloaded from here: [Activity monitoring data ](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
## * 57KB compressed (.zip) and 351KB uncompressed (.csv)
## * There are a total of 17,568 observations contained the dataset
## 
## The variables included in this dataset are:
##         
##         1. steps: Number of steps taking in a 5-minute interval (missing values are coded as "NA")
## 1. date: The date on which the measurement was taken in YYYY-MM-DD format
## 1. interval: Identifier for the 5-minute interval in which measurement was taken

## Loading and preprocessing the data  

#### *Assignment:*  
## *Show any code that is needed to*  
##         
##         1. *Load the data (i.e. - read.csv() ) *  
##         1. *Process/transform the data (if necessary) into a format suitable for your analysis*  
##         
        
## First load required libraries:
## Load  libraries
library(dplyr)
library(ggplot2)

## We loaded the raw dataset (.csv) into a dataframe ('activity') using read.csv()

## read raw dats (.csv file) into dataframe ('activity')
activity <- read.csv("./data/activity.csv",
                     header=TRUE, 
                     colClasses=c(
                             "numeric",   ## steps: number of steps taken during 5min sub-interval of a day
                             "character", ## date: the specific day of sample 'steps'
                             "numeric"    ## interval: the specific 5min sub-interval of sample 'steps'
                     )
)


## and converted variables (date, interval) to factors for later manipulation/reshaping.  
## convert string ("YYYY-MM-DD") to class 'date' and then to 'factor'
activity$date <- as.factor(as.Date(activity$date, format="%Y-%m-%d"))
## convert interval to class 'factor'
activity$interval <- as.factor(activity$interval)
str(activity)

## What is mean total number of steps taken per day?  

## *Assignment:*  
## For this part of the assignment, you can ignore the missing values in the dataset.*  
##  1. Calculate the total number of steps taken per day
##  2. Make a histogram of the total number of steps taken each day. If you do not 
##     understand the difference between a histogram and a barplot, research the 
##     difference between them.
##  3. Calculate and report the mean and median of the total number of steps taken 
##     per day* 
##         

## group by date and sum
stepsPerDay <- activity %>%
        group_by(date) %>%
        summarise(steps = sum(steps, na.rm = TRUE))
## find mean & median
meanStepsPerDay <- format(round(mean(stepsPerDay$steps), 1), 
                          scientific=FALSE, nsmall=1, big.mark=",")
medianStepsPerDay <- format(round(median(stepsPerDay$steps), 1), 
                            scientific=FALSE, nsmall=1, big.mark=",")

print(paste("Mean is", meanStepsPerDay))
print(paste("Median is", medianStepsPerDay))

## We then created a histogram of those daily totals where you can see the subject most frequently walked between 10,000 and 11,000 steps each day: 
        
## Make a histogram of the total number of steps taken each day. If you do not understand the difference between a histogram and a barplot, research the difference between them.*
histInfo <- hist(stepsPerDay$steps, 
                 xlim=c(0,25000), breaks = 25,
                 col = "sky blue", border = "white",
                 main="Histogram of Total Steps per Day",
                 xlab="Steps", ylab="Frequency")
histInfo[c("mids", "counts")]


## The mean steps/day over the period was `r meanStepsPerDay` and the median was `r medianStepsPerDay`.  

## What is the average daily activity pattern?  

## *Assignment:*  
##  1. Make a time series plot (i.e. - type="l") of the 5-minute interval (x-axis) 
##     and the average number of steps taken, averaged across all days (y-axis) 
##  2. Which 5-minute interval, on average across all the days in the dataset, 
##     contains the maximum number of steps?
        
##  Now grouping by interval-- in combination with the function mean(), we calculated the mean for each 5min interval over the sample period:
averageSteps <- activity %>%
        group_by(interval) %>%
        summarise(steps = mean(steps,na.rm = TRUE))
maxAverageSteps <- averageSteps[which.max(averageSteps$steps), ]

plot(as.numeric(as.character(averageSteps$interval)), averageSteps$steps, 
     type = "l", 
     lwd = 2, col = "deep sky blue 2",
     main = "Steps per 5min Interval Averaged Across All Days",
     xlab = "5-min interval", ylab = "Average steps per interval" )



## Imputing missing values  

## *Assignment:*  
## Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.*  
## 
##         1. *Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*
##         1. *Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*
##         1. *Create a new dataset that is equal to the original dataset but with the missing data filled in.*
##         1. *Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.*  
##        - *Do these values differ from the estimates from the first part of the assignment?*  
##         - *What is the impact of imputing missing data on the estimates of the total daily number of steps?*  
        
## we counted the total quantity of missing step samples:
missingValues <- sum(is.na(activity$steps))
print(paste("There are", missingValues, "missing values"))

## Looking at missing values in the dataset, we saw the NAs are grouped in terms of entire days. **So we decided to impute these missing days with the average day for the sample period** that we calculated earlier *(ie - replace each NA sample interval with the corresponding average).*  
        
activityImputed <- activity
activityImputed <- activityImputed %>%
        mutate(steps = ifelse(is.na(steps), averageSteps[interval, ]$steps, steps))

testIn <- data.frame(c(1,2,3,4,5))
testIn <- cbind(testIn, c(10,NA,NA,40,50))
colnames(testIn) <- c("input", "value")
testIn
testTable <- data.frame(c(1,2,3,4,5))
testTable <- cbind(testTable, c(100,200,300,400,500))
colnames(testTable) <- c("input", "value")
testOut <- testIn %>%
        mutate(val  = ifelse(is.na(value), testTable[input,]$value, value))

testIn
testOut
 


## Calculate steps per day (imputed NA)
stepsPerDayImputed <- activityImputed %>%
        group_by(date) %>%
        summarise(steps = sum(steps,na.rm = TRUE))
averageStepsImputed <- activityImputed %>%
        group_by(interval) %>%
        summarise(steps = sum(steps,na.rm = TRUE))

## Make a histogram of the total number of steps taken each day. If you do not understand the difference between a histogram and a barplot, research the difference between them.*

## For comparison, we plotted both average daily steps as well as a histogram of steps/day and unimputed and imputed:

par(mfrow=c(1,2))
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
par(mfrow=c(1,1))

par(mfrow=c(1,2))
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
par(mfrow=c(1,1))

meanStepsPerDayImputed <- format(round(mean(stepsPerDayImputed$steps), 1), 
                                 scientific=FALSE, nsmall=1, big.mark=",")
medianStepsPerDayImputed <- format(round(median(stepsPerDayImputed$steps), 1), 
                                   scientific=FALSE, nsmall=1, big.mark=",")

## Imputing NAs as described above, the mean & median statistics changed very little: 
print(paste("mean:", meanStepsPerDayImputed, "(vs.", meanStepsPerDay, "unimputed)")) 
print(paste("median:", meanStepsPerDayImputed, "(vs.", medianStepsPerDay, "unimputed)")) 

## We attribute this to there being (1) just 8 missing days (out of 61) and (2) we subsititued average days for the missing ones (which would only strengthen the mean).  

## It is interesting to note (again, because we're imputing the average day for the missing days) that the the only  change to the histogram was to heighten the central peak --which is to be expected.  
## 

## Are there differences in activity patterns between weekdays and weekends?  
  
#### *Assignment:*  

## *For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.*
## 
## 1. *Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.*
## 2. *Make a panel plot containing a time series plot (i.e. - type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.*

## With the hint to consider the weekday() function, it was pretty straightfoward to add a 2-level factor ("Weekend", "Weekday") which we implemented with a simple:  **IF** (saturday|sunday) **THEN** weekend **ELSE** weekday  
    
activityImputed2 <- activityImputed %>%
        mutate(weekDayEnd = ifelse(weekdays(as.Date(date)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
activityImputed2$weekDayEnd <- as.factor(activityImputed2$weekDayEnd)

## reshape the data by weekday/weekend and interval
activityImputed2 <- activityImputed2 %>%
        group_by(weekDayEnd, interval) %>%
        summarise(steps = mean(steps,na.rm = TRUE))
## and plot with ggplot:
pl <- ggplot(filter(activityImputed2, weekDayEnd %in% c("Weekend", "Weekday")),
             aes(x=as.numeric(as.character(interval)), y=steps, color=weekDayEnd) ) +
        geom_line() +
        facet_grid(weekDayEnd ~ ., scales="fixed", space="fixed") +
        labs(x="interval", 
             y="number of steps/interval",
             color= "Part of Week",
             title="Average Steps/Interval",
             subtitle="Comparison of Weekends vs. Weekdays"
             ) +
        theme(
                legend.position = "none",
                ## plot.margin = unit(c(1.5,1,1.5,1), "cm"),
                strip.text.y = element_text(face="bold")
                )
print(pl)


## From the plots we observed the following:  
## 
## * on the weekend start-of-day activity is much lower and ramps more gradually  
##     + ...and the morning peak (around 800) is lower *(our guess: no commute)*  
## * on the weekend mid-day activity is significantly higher (roughly twice as high compared to weekdays)  
## * on the weekend there is a gradual rolloff at end-of-day  
##     + ...with no peak (around 1800) *(our guess: no commute)*  






