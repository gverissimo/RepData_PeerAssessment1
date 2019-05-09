---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Overview
*It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.*

*This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.*

The raw dataset for this assignment 

* Was downloaded from here: [Activity monitoring data ](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
* 57KB compressed (.zip) and 351KB uncompressed (.csv)
* There are a total of 17,568 observations contained the dataset

The variables included in this dataset are:

1. steps: Number of steps taking in a 5-minute interval (missing values are coded as "NA")
1. date: The date on which the measurement was taken in YYYY-MM-DD format
1. interval: Identifier for the 5-minute interval in which measurement was taken


## Loading and preprocessing the data
Load libraries

```r
##library(dplyr)
##library(ggplot2)
```

Load dataframe from .csv file


```r
activity <- read.csv("./data/activity.csv",
                     header=TRUE, 
                     colClasses=c(
                             "numeric",   ## steps: number of steps taken during 5min sub-interval of a day
                             "character", ## date: the specific day of sample 'steps'
                             "numeric"    ## interval: the specific 5min sub-interval of sample 'steps'
                )
        )
activity$date <- as.Date(activity$date, format="%Y-%m-%d")   ## convert string ("YYYY-MM-DD") to class 'date'
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
*For this part of the assignment, you can ignore the missing values in the dataset.*

1. *Calculate the total number of steps taken per day*
1. *Make a histogram of the total number of steps taken each day. If you do not understand the difference between a histogram and a barplot, research the difference between them.*
1. *Calculate and report the mean and median of the total number of steps taken per day*

Calculate steps per day *(ignoring NA)*


```r
stepsPerDay <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)

hist(stepsPerDay$steps, 
     xlim=c(0,25000), breaks = 25,
     col = "sky blue", border = "white",
     main="Histogram of Total Steps per Day",
     xlab="Steps", ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
meanStepsPerDay <- format(round(mean(stepsPerDay$steps), 0), 
                          scientific=FALSE, big.mark=",")
medianStepsPerDay <- format(round(median(stepsPerDay$steps), 0), 
                            scientific=FALSE, big.mark=",")
summary(stepsPerDay$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

Number of steps per day:

* mean: 10,766  
* median: 10,765  


## What is the average daily activity pattern?
1. *Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*
1. *Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*


```r
averageSteps <- aggregate(steps ~ interval, data = activity, mean, na.rm=TRUE)

plot(averageSteps$interval, averageSteps$steps, 
     type = "l", 
     lwd = 2, col = "deep sky blue 2",
     main = "Steps per 5min Interval Averaged Across All Days",
     xlab = "5-min interval", ylab = "Average steps per interval" )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
1. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
1. Create a new dataset that is equal to the original dataset but with the missing data filled in.
1. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


## Are there differences in activity patterns between weekdays and weekends?
For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
1. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



