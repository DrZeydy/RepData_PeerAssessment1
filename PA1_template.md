---
title: "Reproducible Research - Peer Assessment 1"
author: "Zeydy Ortiz, Ph. D."
date: "Sunday, May 17, 2015"
output: html_document
---

### Loading and preprocessing the data

1. Load the data (i.e. read.csv())
```{r}
activity <- read.csv("./data/activity.csv", stringsAsFactors=FALSE)

```

2. Process/transform the data (if necessary) into a format suitable for your analysis
```{r}
library(plyr)
# summary of total number of steps per day
steps.day <- ddply(activity, .(date), summarize, total_steps = sum(steps, na.rm=TRUE))
# summary of average number of steps per 5-min interval
steps.interval <- ddply(activity, .(interval), summarize, avg_steps = mean(steps, na.rm=TRUE))
```

### What is mean total number of steps taken per day?

1. Ignoring the missing values in the dataset, calculate the total number of steps taken per day
```{r}
steps.day
```

2. Make a histogram of the total number of steps taken each day
```{r}
hist(steps.day$total_steps, main="Histogram of the total number of steps taken each day", 
    xlab="Total steps")
```

3. Calculate and report the mean and median of the total number of steps taken per day
```{r}
mean.steps.day <- mean(steps.day$total_steps)
mean.steps.day
```
```{r}
median.steps.day <- median(steps.day$total_steps)
median.steps.day
```

### What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
plot(steps.interval, type='l', main="Average number of steps taken per interval", xlab="5-minute interval", ylab="Average steps")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
steps.interval$interval[steps.interval$avg_steps==max(steps.interval$avg_steps)]
```


### Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

We can see all missing fields with missing values with the ```summary()``` command
```{r}
summary(activity)
```

Since the ```steps``` variable is the only one with missings values, I report the number below
```{r}
sum(is.na(activity$steps))
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will use the average number of steps for the corresponding 5-minute interval

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
smooth.activity <- activity
for (i in 1:length(smooth.activity$steps)) {
  if (is.na(smooth.activity$steps[i])) {     ### if steps missing
    interval <- smooth.activity$interval[i]  ### determine interval and use it's avg_steps
    smooth.activity$steps[i] <- steps.interval$avg_steps[steps.interval$interval==interval]
  }
}
# summary of total number of steps per day to use below
smooth.steps.day <- ddply(smooth.activity, .(date), summarize, 
                          total_steps = sum(steps, na.rm=TRUE))
```

4. Make a histogram of the total number of steps taken each day
```{r}
hist(smooth.steps.day$total_steps, main="Histogram of the total number of steps taken each day", 
    xlab="Total steps")
```

Calculate and report the mean and median total number of steps taken per day. 
```{r}
smooth.mean.steps.day <- mean(smooth.steps.day$total_steps)
smooth.mean.steps.day
```
```{r}
smooth.median.steps.day <- median(smooth.steps.day$total_steps)
smooth.median.steps.day
```

Do these values differ from the estimates from the first part of the assignment? 

The difference from the estimates from the first part are shown below:
```{r}
smooth.mean.steps.day - mean.steps.day
```
```{r}
smooth.median.steps.day - median.steps.day
```

What is the impact of imputing missing data on the estimates of the total daily number of steps?

The total number of steps increased as a result of filling in the missing values with the
average number of steps for the corresponding interval.  As expected, the mean and median values
also increased.

### Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
indicating whether a given date is a weekday or weekend day.
```{r}
for (i in 1:length(smooth.activity$steps)) {
  if (weekdays(as.Date(smooth.activity$date[i])) == "Saturday" | 
        weekdays(as.Date(smooth.activity$date[i])) == "Sunday") {
          smooth.activity$day[i] <- "weekend"
  } else { smooth.activity$day[i] <- "weekday" }
}
smooth.activity$day <- as.factor(smooth.activity$day)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
smooth.steps.interval <- ddply(smooth.activity, .(interval, day), summarize, 
                               avg_steps = mean(steps, na.rm=TRUE))
```
