# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
# Do not Filter out all NA data
# activity <- activity[!is.na(activity$steps),]
```

## What is mean total number of steps taken per day?

```r
aggregateStepsByDay <- aggregate(steps ~ date, activity, sum)
hist(aggregateStepsByDay$steps,
     nclass = 10, 
     main = "Histogram of Daily Steps",
     xlab = "# daily steps")
```

![](PA1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
meanAggStepsByDay <- mean(aggregateStepsByDay$steps, na.rm = TRUE)
medianAggStepsByDay <- median(aggregateStepsByDay$steps, na.rm = TRUE)

meanAggStepsByDay <- formatC(meanAggStepsByDay, digits = 1, format = "f")
medianAggStepsByDay <- formatC(medianAggStepsByDay, digits = 0, format = "f")
```

The Mean is 10766.2 and median is 10765



## What is the average daily activity pattern?

```r
meanStepsByInterval <- aggregate(steps ~ interval, activity, mean)
plot(meanStepsByInterval$interval,
     meanStepsByInterval$steps,
     type = "l", 
     main="Average steps per interval", 
     xlab = "Interval Number", 
     ylab="# Steps")
```

![](PA1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# and now the max
maxSteps <- max(meanStepsByInterval$steps)
maxInterval <- meanStepsByInterval$interval[which.max(meanStepsByInterval$steps)]
maxHours <- maxInterval %/% 60
maxMinutes <- maxInterval %% 60
maxHours <- formatC(maxHours, width = 2, flag = "0")
maxMinutes <- formatC(maxMinutes, width=2, flag = "0")
```

The Maximum of 206.1698113 occurs at interval #835 at 13:55 hours.

## Imputing missing values

```r
# Calculate number of missing values
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
missing <- which(is.na(activity$steps))
length(missing)
```

```
## [1] 2304
```

```r
imputedActivity <- activity
# These are dupes, I just want to look at them here and be sure I know what I'm looking at
for(i in missing){
  row <- activity[i,]
  act <- meanStepsByInterval[which(meanStepsByInterval$interval==row$interval),]$steps
  imputedActivity$steps[i] <- act
}
aggregateImputedStepsByDay <- aggregate(steps ~ date, imputedActivity, sum)
hist(aggregateImputedStepsByDay$steps,
     nclass = 10, 
     main = "Histogram of Imputed Daily Steps",
     xlab = "# imputed daily steps")
```

![](PA1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
meanImputedAggStepsByDay <- mean(aggregateImputedStepsByDay$steps)
medianImputedAggStepsByDay <- median(aggregateImputedStepsByDay$steps)

meanImputedAggStepsByDay <- formatC(meanImputedAggStepsByDay, digits = 1, format = "f")
medianImputedAggStepsByDay <- formatC(medianImputedAggStepsByDay, digits = 1, format = "f")
```

The Mean is 10766.2 and median is 10766.2


## Are there differences in activity patterns between weekdays and weekends?

```r
# Add dayType column
activity$dayType <- as.factor(ifelse(
  weekdays(as.Date(activity$date)) %in% c("Saturday", "Sunday"),
  "Weekend", 
  "Weekday"))
meanStepsByIntervalAndDayType <- aggregate(steps ~ interval + dayType, activity, mean)


# Now do the plot
library(lattice)
xyplot(steps~interval | dayType, data = meanStepsByIntervalAndDayType,
      type = 'l',
      xlab = 'Interval',
      ylab = 'Number of Steps',
      layout = c(1,2))
```

![](PA1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
