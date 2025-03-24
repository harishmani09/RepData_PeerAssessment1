---
title: "Reproducible Research Course Project 1"
author: "Harish Mani"
date: "2025-03-24"
output: 
    html_document:
        keep_md: true 
---

# loading and processing the data 


``` r
library(tidyverse)
library(data.table)
unzip('activity.zip')
data <- read.csv('activity.csv')
data_dt <- setDT(data)
```

## 1. Calculate the total number of steps taken per day


``` r
steps_per_day <- data |>
    mutate(
        date = ymd(date),
    ) |>
    group_by(date) |>
    summarise(
              steps_total = sum(steps),
              .groups='drop')
```


## 2.  Make a histogram of the total number of steps taken each day

``` r
ggplot(steps_per_day, aes(steps_total)) + geom_histogram(fill='red') +
    labs(x='number of steps',y='frequency',title='daily steps')
```

```
## Warning: Removed 8 rows containing non-finite outside the scale range
## (`stat_bin()`).
```

![](RR_Project_1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## 3. Calculate and report the mean and median of the total number of steps taken per day

``` r
summary(steps_per_day$steps_total)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

## 4. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
## average number of steps taken, averaged across all days (y-axis)


``` r
mean_interval <- data |>
    group_by(interval) |>
    na.omit() |>
    summarise(avg_steps = mean(steps) ,.groups='drop')

ggplot(mean_interval, aes(x=interval,avg_steps)) + geom_line() +
    labs(x='intervals',y='avg number of steps') +
    ggtitle('time series of steps taken during 5 mins interval')+theme_light()
```

![](RR_Project_1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


## 5. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


``` r
mean_interval |> filter(avg_steps == max(avg_steps))
```

```
## # A tibble: 1 × 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835      206.
```

## calculate missing values

``` r
data |>
    summarise_all(~sum(is.na(.)))
```

```
##   steps date interval
## 1  2304    0        0
```

## imputing missing values 

``` r
data_dt[is.na(steps),'steps'] <- data_dt[,c(lapply(.SD,median,na.rm=T)),.SDcols = c('steps')]
```

##new mean and median

``` r
steps_per_day_after_na <- data_dt |>
    mutate(date = ymd(date)) |>
    group_by(date) |>
    summarise(steps_total = sum(steps),.groups = 'drop')

summary(steps_per_day_after_na$steps_total)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

##write a new dataset 

``` r
data.table::fwrite(x=data_dt,file = 'tidy_data.csv',quote = FALSE)
```

## Make a histogram of the total number of steps taken each day and Calculate and 
## report the mean and median total number of steps taken per day


``` r
# Make a histogram of the total number of steps taken each day and Calculate and 
# report the mean and median total number of steps taken per day
hist_data <- data_dt |>
    group_by(date) |>
    summarise(
        sum_steps = sum(steps), .groups = 'drop'
    )

ggplot(hist_data, aes(sum_steps)) +
    geom_histogram(fill='gold') + theme_classic() +
    labs(x='steps', y='frequency',title='Daily Steps')
```

![](RR_Project_1_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


## Do these values differ from the estimates from the first part of the assignment? What is ## the impact of imputing missing data on the estimates of the total daily number of steps?

    |    Estimate Type  | Mean      | Median |
    | :---              | :---      | :---   |
    | with na           | 10766     | 10765  |
    | with na filled    |  9354     | 10395  |


## Are there differences in activity patterns between weekdays and weekends?

``` r
data_with_day_type <- data_dt |>
    mutate( date = ymd(date)) |>
    mutate( day = weekdays(date)) |>
    mutate(day_type=ifelse(day %in% c('Saturday','Sunday'),'Weekend','Weekday'))
    
interval_date <-  data_with_day_type |>
    group_by(date,interval,day_type) |>
    summarise(avg_steps = mean(steps),.groups = 'drop')
```

## plot on basis of day_type

``` r
ggplot(interval_date, aes(x=interval,y=avg_steps,color=day_type)) + 
    facet_grid(day_type ~.) +
    geom_line(stat = 'summary')+
    labs(title='Avg num of steps taken on day type ie weekday or weekend',x='interval',y='steps')
```

![](RR_Project_1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->




