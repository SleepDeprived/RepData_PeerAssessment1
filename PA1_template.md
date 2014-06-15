# Reproducible Research: Peer Assessment 1
This data set includes information about step activitiy, binned into 5 minute intervals, for a single person over 61 days (October - November of 2012). 

## Loading and preprocessing the data


```r
    library(ggplot2)
    library(plyr)
    library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
## 
## The following object is masked from 'package:plyr':
## 
##     here
```

```r
    data <- read.csv("activity.csv")
    data$date_as_obj <- as.Date(data$date, "%Y-%m-%d")
    data$hour  <- data$interval %/% 100
    data$minute  <- data$interval %% 100
    data$date_time <- strptime(paste(data$date, " ", data$hour, ":", data$minute, sep = ""), format("%Y-%m-%d %H:%M"))
```

## What is mean total number of steps taken per day?

Histogram is the frequency of total steps in a day binned into 15 segments.


```r
    total_steps_in_a_day_na_removed  <-  tapply(data[!is.na(data$steps),]$steps, data[!is.na(data$steps),]$date_as_obj, sum)

    mean_steps <- mean(tapply(data[!is.na(data$steps),]$steps, data[!is.na(data$steps),]$date_as_obj, sum))
    median_steps <- median(tapply(data[!is.na(data$steps),]$steps, data[!is.na(data$steps),]$date_as_obj, sum))
```

The mean number of steps in a day is 1.0766 &times; 10<sup>4</sup> and the median number of steps in a day is 10765.  The data has the following distribution of steps per 5 minute interval:


```r
    png("figures/daily_step_frequency_na_removed.png", width = 480, height = 480, units="px")
    hist(total_steps_in_a_day_na_removed, breaks = 15, xlab = "Number of Steps Per Day", main = "Daily Step Frequency (Oct - Nov)")
    dev.off( )
```

```
## pdf 
##   2
```

## What is the average daily activity pattern?


```r
    data$day_time <- data$date_time - trunc(data$date_time, "days")
    data_na_removed <- data[!is.na(data$steps), ]
    average_steps_throughout_day <- ddply(.data = data_na_removed, .(day_time), summarize, ave_steps = mean(steps))    
```

The daily pattern of steps is:


```r
    png("figures/daily_activity_average_step_number.png", width = 480, height = 480, units="px")
    plot(strptime(as.character(seconds_to_period(average_steps_throughout_day$day_time)), format="%HH %MM %SS"), average_steps_throughout_day$ave_steps, type = "l", xlab = "Time of Day", ylab = "Average Number of Steps")
    dev.off( )
```

```
## pdf 
##   2
```



```r
    period_with_max_ave_steps <- average_steps_throughout_day[average_steps_throughout_day$ave_steps == max(average_steps_throughout_day$ave_steps), ]$day_time
    time_period <- strsplit(as.character(strptime(as.character(seconds_to_period(period_with_max_ave_steps)), format="%HH %MM %SS")), " ")[[1]][2]
    period_with_max_ave_steps_step_num <-  average_steps_throughout_day[average_steps_throughout_day$ave_steps == max(average_steps_throughout_day$ave_steps), ]$ave_steps
```

The interval with max average steps is the 5 minute period at 08:35:00 with an average of 206.1698 steps.

## Imputing missing values



```r
    median_steps_total_na_removed <- median(data_na_removed$steps)
    mean_steps_total_na_removed <- mean(data_na_removed$steps)
    data_na_removed$day_of_week  <- as.factor(weekdays(data_na_removed$date_as_obj))
```

The median (0), mean (37.3826), and distribution of step numbers per period (step_frequency_by_day.png) were examined for the data set as a whole and per day of the week excluding missing (NA) values.  


```r
    png("figures/step_frequency_by_day.png", width = 480, height = 480, units="px" )
    qplot(steps, data = data_na_removed, facets = day_of_week~., binwidth = 10, main = "Frequency of steps per interval by day", xlab = "Number of steps in an interval", ylab = "Frequency")
    dev.off()
```

```
## pdf 
##   2
```

Additionally, I calculated the mean and median steps for each day and identified a statistically significant difference in mean step number between some days. 


```r
    ave_day_of_week_steps <- ddply(.data = data_na_removed, .(weekdays(date_as_obj)), summarize, ave_steps = mean(steps))
    colnames(ave_day_of_week_steps)[1] <- "days"
    median_day_of_week_steps <- ddply(.data = data_na_removed, .(weekdays(date_as_obj)), summarize, median_steps = median(steps))
    pairwise.t.test(data_na_removed$steps, data_na_removed$day_of_week, pool.sd = FALSE)
```

```
## 
## 	Pairwise comparisons using t tests with non-pooled SD 
## 
## data:  data_na_removed$steps and data_na_removed$day_of_week 
## 
##           Friday  Monday  Saturday Sunday  Thursday Tuesday
## Monday    0.28914 -       -        -       -        -      
## Saturday  1.00000 0.21380 -        -       -        -      
## Sunday    1.00000 0.28064 1.00000  -       -        -      
## Thursday  0.00048 0.63372 0.00021  0.00021 -        -      
## Tuesday   0.00588 1.00000 0.00296  0.00332 1.00000  -      
## Wednesday 1.00000 0.70010 1.00000  1.00000 0.00238  0.02592
## 
## P value adjustment method: holm
```

Therefore I decided to replace missing (NA) step values in the data with the mean step value for a period for each day of the week respectively.


```r
    data[is.na(data$steps) & data$days == "Monday",]$steps <- ave_day_of_week_steps[ave_day_of_week_steps$days == "Monday",]$ave_steps
```

```
## Error: replacement has 1 row, data has 0
```

```r
    data[is.na(data$steps) & data$days == "Tuesday",]$steps <- ave_day_of_week_steps[ave_day_of_week_steps$days == "Tuesday",]$ave_steps
```

```
## Error: replacement has 1 row, data has 0
```

```r
    data[is.na(data$steps) & data$days == "Wednesday",]$steps <- ave_day_of_week_steps[ave_day_of_week_steps$days == "Wednesday",]$ave_steps
```

```
## Error: replacement has 1 row, data has 0
```

```r
    data[is.na(data$steps) & data$days == "Thursday",]$steps <- ave_day_of_week_steps[ave_day_of_week_steps$days == "Thursday",]$ave_steps
```

```
## Error: replacement has 1 row, data has 0
```

```r
    data[is.na(data$steps) & data$days == "Friday",]$steps <- ave_day_of_week_steps[ave_day_of_week_steps$days == "Friday",]$ave_steps
```

```
## Error: replacement has 1 row, data has 0
```

```r
    data[is.na(data$steps) & data$days == "Saturday",]$steps <- ave_day_of_week_steps[ave_day_of_week_steps$days == "Saturday",]$ave_steps
```

```
## Error: replacement has 1 row, data has 0
```

```r
    data[is.na(data$steps) & data$days == "Sunday",]$steps <- ave_day_of_week_steps[ave_day_of_week_steps$days == "Sunday",]$ave_steps
```

```
## Error: replacement has 1 row, data has 0
```


```r
    median_steps_total <- median(data$steps)
    mean_steps_total <- mean(data$steps)
    t.test(data$steps, data_na_removed$steps)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  data$steps and data_na_removed$steps
## t = 0, df = 30526, p-value = 1
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.513  2.513
## sample estimates:
## mean of x mean of y 
##     37.38     37.38
```

```r
    total_steps_in_a_day  <-  tapply(data$steps, data$date_as_obj, sum)
```


Imputing data for the missing values in this way did not change the median (NA) and had a non-significant impact upon the mean (NA)

The frequency for daily steps also increased .


```r
    png("figures/daily_step_frequency.png", width = 480, height = 480, units="px")
    hist(total_steps_in_a_day, breaks = 15, xlab = "Number of Steps Per Day", main = "Daily Step Frequency (Oct - Nov)")
    dev.off( )
```

```
## pdf 
##   2
```


## Are there differences in activity patterns between weekdays and weekends?

The following plot indicatest hat there is a difference in activity between weekdays and weekends:


```r
    data$weekday_weekend <- NA
    sat_sun <- data$days %in% c("Saturday", "Sunday")
    data[sat_sun,]$weekday_weekend <- "Weekend"
```

```
## Error: replacement has 1 row, data has 0
```

```r
    data[!sat_sun,]$weekday_weekend <- "Weekday"
```

```
## Error: replacement has 1 row, data has 0
```

```r
    data$weekday_weekend <- as.factor(data$weekday_weekend)

    average_steps_weekend_weekday <- ddply(.data = data, .(interval, weekday_weekend), summarize, ave_steps = mean(steps))
```


```r
    png("figures/daily_activity_average_weekday_weekend.png", width = 480, height = 480, units="px")
    qplot(interval, ave_steps, data = average_steps_weekend_weekday, facets = weekday_weekend~., xlab = "Time of Day", ylab = "Average Number of Steps", geom = "line")
```

```
## Error: 'from' must be of length 1
```

```r
    dev.off( )
```

```
## pdf 
##   2
```



