---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
First, I read in the dataset.


```r
dat <- read.csv("activity.csv")
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
dat$date <- ymd(dat$date)
```

## What is mean total number of steps taken per day?
Next, I find the mean and median number of steps taken in a day.


```r
totalstepsbyday <- tapply(dat$steps,dat$date,sum,na.rm=TRUE)
hist(totalstepsbyday)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
meanstepsperday <- mean(totalstepsbyday)
medianstepsperday <- median(totalstepsbyday)
```

The mean of steps taken per day is 9354.2295082 
and the median of steps taken per day is 10395.

## What is the average daily activity pattern?
Let's look at the daily activity pattern.


```r
averagestepsperinterval <- tapply(dat$steps,dat$interval,mean,na.rm=TRUE)
plot(dat$interval[1:288], averagestepsperinterval, type = 'l', xlab = "Interval", ylab="Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxinterval <- dat$interval[which.max(averagestepsperinterval)]
```

The 5-minute interval with the most average steps is the one starting at 835.

## Imputing missing values
Let's work with some of our missing values.


```r
numberofmissingvalues <- sum(is.na(dat$steps))
```



There are 2304 observations with missing values.
Let's replace those with the average number of steps for any given interval.


```r
datnew <- dat
datnew$steps[is.na(datnew$steps)] <- mean(datnew$steps, na.rm = T)
colSums(is.na(datnew))
```

```
##    steps     date interval 
##        0        0        0
```

```r
totalstepsbydaynew <- tapply(datnew$steps,datnew$date,sum,na.rm=TRUE)
hist(totalstepsbydaynew)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
meanstepsperday2 <- mean(totalstepsbyday)
medianstepsperday2 <- median(totalstepsbyday)
```

The mean of steps taken per day is 9354.2295082 
and the median of steps taken per day is 10395.

The distribution became less skewed with the imputed missing values.


## Are there differences in activity patterns between weekdays and weekends?


```r
datnew$week <- weekdays(datnew$date)
datnew$week  <- ifelse(datnew$week %in% c("Saturday","Sunday"), "Weekend","Weekday")
datnew$week <- as.factor(datnew$week)
library(lattice)
averagestepsperinterval2 <- tapply(datnew$steps,list(datnew$interval,datnew$week),mean,na.rm=TRUE)
par(mfrow=c(1,2))
plot(dat$interval[1:288], averagestepsperinterval2[,1], type = 'l', xlab = "Interval", ylab="Average Steps", main ="Weekday")
plot(dat$interval[1:288], averagestepsperinterval2[,2], type = 'l', xlab = "Interval", ylab="Average Steps", main="Weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



