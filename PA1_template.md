---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
# convert date string to date using lubridate
activity$date <- ymd(activity$date)
```



## What is mean total number of steps taken per day?

```r
activity <- group_by(activity, date)
steps_perday <- aggregate(activity[c("steps", "interval")], by = list(as.factor(activity$date)), sum)

# create histogram
ggplot(steps_perday, aes(steps)) + geom_histogram() + xlab("steps per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/mean per day-1.png)<!-- -->

```r
mean_perday <- round(mean(steps_perday$steps, na.rm = TRUE), 2)
median_perday <- median(steps_perday$steps, na.rm = TRUE)

activity <- as.data.frame(ungroup(activity))
```
The mean per day is 10766.19 and the median is 10765.

## What is the average daily activity pattern?

```r
activity <- group_by(activity, interval)
steps_perinterval <- aggregate(activity[c("steps")], by = list(interval = activity$interval), mean, na.rm = TRUE)

# create time series
timeseries = format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "5 min"), "%H%M", tz="GMT")
timeseries <- parse_time(timeseries[1:length(timeseries)-1], "%H%M")
steps_perinterval$timeseries <- timeseries

ggplot(steps_perinterval, aes(timeseries, steps)) + geom_line()
```

![](PA1_template_files/figure-html/daily activity pattern-1.png)<!-- -->

```r
# find interval with max average steps
idx <- which.max(steps_perinterval$steps)
maxinterval <- steps_perinterval$timeseries[idx]

activity <- ungroup(activity)
```
The most steps occur on average in the 5 minutes after 08:35:00.


## Imputing missing values

```r
na <- sum(is.na(activity$steps))
```
Of the 17568 step entries 2304 are missing, that is 13.11%.  

The missing values in the raw data will be filled with the mean number of steps for the corresponding time interval.

```r
activity_filled <- activity
for (i in 1:nrow(activity)) {
    if (is.na(activity$steps[i])==TRUE){
        activity_filled$steps[i] <- steps_perinterval[(steps_perinterval$interval == activity$interval[i]),]$steps
    }
}

activity_filled <- group_by(activity_filled, date)
steps_perday_filled <- aggregate(activity_filled[c("steps")], by = list(as.factor(activity_filled$date)), sum)

# create histogram
ggplot(steps_perday_filled, aes(steps)) + geom_histogram() + xlab("steps per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/fill nas-1.png)<!-- -->

```r
mean_perday_filled <- round(mean(steps_perday_filled$steps, na.rm = TRUE), 2)
median_perday_filled <- median(steps_perday_filled$steps, na.rm = TRUE)

comp <- data.frame("mean" = c(mean_perday, mean_perday_filled), "median" = c(median_perday, median_perday_filled), row.names = c("with na", "imputed na"))
```


```r
kable(comp)
```

                  mean     median
-----------  ---------  ---------
with na       10766.19   10765.00
imputed na    10766.19   10766.19
  
The results dont have an effect on the mean per day, and only a minor effect on the median.  
The total number of steps per day is affected because the imputed valus are added to the number of total steps.

## Are there differences in activity patterns between weekdays and weekends?

```r
activity <- mutate(activity, "weekday" = weekdays(activity$date, 1))

wd_we <- c()
for (i in 1:nrow(activity)){
    if (activity$weekday[i] %in% c("Sa", "So")){
        wd_we[i] <- "weekend"
    } else {
        wd_we[i] <- "weekday"
    }
}

activity <- mutate(activity, "we_wd" = as.factor(wd_we))

ggplot(activity, aes(interval, steps)) + geom_line() + facet_grid(we_wd ~ .)
```

```
## Warning: Removed 1 rows containing missing values (geom_path).
```

![](PA1_template_files/figure-html/day of the week-1.png)<!-- -->

