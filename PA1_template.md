# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
filename <- "activity.csv"
if(!file.exists(filename)) {
        unzip("activity.zip")
        }
data <- read.csv(filename)
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

```r
steps <- xtabs(steps ~ date, data)
hist(steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
smean <- mean(steps)
smedian <- median(steps)
```
The mean and median of the total number of steps taken per day are 1.0766189\times 10^{4} and 1.0765\times 10^{4}, respectively.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
