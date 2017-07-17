---
title: "Reproducible Research - Assignment1"
date: "16 July 2017"
output: html_document
---


##Loading and preprocessing the data

```r
if(!file.exists("activity.csv")){
  unzip("activity.zip")
}
data<- read.csv("activity.csv")
data$date<- as.Date(data$date)
```
##What is mean total number of steps taken per day?
###Ignoring the missing values

```r
stepsbyday<- tapply(data$steps, data$date, sum, na.rm=TRUE)
library(ggplot2)
qplot(stepsbyday, xlab="No. of Steps Taken Each Day", ylab="Total Frequency", binwidth=500)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
medianbyday<- median(stepsbyday)
meanbyday<- mean(stepsbyday)
```
######Median = 10395L
######Mean = 9354.22950819672

###What is the average daily activity pattern?

```r
avg<- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(names(avg), avg, xlab="5-min interval", type="l", ylab="Average no. of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)


```r
maxavg<- max(avg)
maxinterval<- as.numeric(names(avg)[which(avg==max(avg))])
```

######5 Minute Interval:835
######Max Average Value:206.169811320755

###Imputing missing values
####1.Calculate and report the total number of missing values in the dataset 

```r
totalna<- sum(is.na(data$steps))
```
######Total NAs: 2304
####2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
imputedata<- data
imputedata$steps[which(is.na(data$steps))]<- as.vector(avg[as.character(data[which(is.na(data$steps)),3])])
```
####3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

####4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepseachday<- tapply(imputedata$steps, imputedata$date, sum, na.rm=TRUE)
qplot(stepseachday, xlab="No. of Steps Taken Each Day", ylab="Total Frequency", binwidth=500)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
medianEachDayImputed<- median(stepseachday)
meanEachDayImputed<- mean(stepseachday)
```

######Mean total number of steps taken per day - 10766.1886792453
######Median total number of steps taken per day - 10766.1886792453

###Are there differences in activity patterns between weekdays and weekends?
####1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
imputedata$dayType<- ifelse(as.POSIXlt(imputedata$date)$wday %in% c(0,6), "weekends","weekdays")
```
####2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
aggregateData<- aggregate(steps ~ interval + dayType, data=imputedata, mean)
ggplot(aggregateData, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dayType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
















