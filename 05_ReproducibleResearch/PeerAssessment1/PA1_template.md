# Reproducible Research: Peer Assessment 1
<!---

-->


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

```r
date <- unique(data$date)
day_total <- function(date){ sum(data[data$date == date, "steps"] )  }

total_steps <- sapply(date, day_total) 
df <- data.frame(date = date, total_steps = total_steps)
df <- df[!is.na(df$total_steps),]
hist(df$total_steps, main="Total number of steps per day",xlab="Number of steps", breaks=9, ylim=c(0,20))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
summary(df$total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

## What is the average daily activity pattern?

```r
interval <- unique(data$interval)
interval_mean <- function(int){ mean(data[data$interval == int, "steps"], na.rm=TRUE )  }
int_mean <- sapply(interval, interval_mean)
df_day <- data.frame(interval = interval, int_mean = int_mean)

plot(int_mean~interval, type="l", main="Average daily activity pattern", ylab="Average steps per 5 min interval", xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Inputing missing values

```r
print(sum(is.na(data$steps)))
```

```
## [1] 2304
```

```r
# fill missing entries with mean for the 5 min interval
# possible to do without for loop?
data_fill <- data
for(int in interval){
          data_fill[is.na(data_fill[, "steps"] & data_fill$interval == int ), "steps" ] <- df_day[interval==int,"int_mean"]       
}

day_total_fill <- function(date){ sum(data_fill[data_fill$date == date, "steps"] )  }

total_steps_fill <- sapply(date, day_total_fill) 
df_fill <- data.frame(date = date, total_steps = total_steps_fill)
hist(df_fill$total_steps, main="Total number of steps per day, filled data",xlab="Number of steps", breaks=9, ylim=c(0,25))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
summary(df_fill$total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

## Are there differences in activity patterns between weekdays and weekends?
