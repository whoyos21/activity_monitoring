---
title: "Course Project 1 - Reproducible Research"
author: "William Hoyos Morales"
date: "6/1/2020"
output: 
        html_document:
                keep_md: TRUE
---



## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in the dataset are:

* **steps:** Number of steps taking in a 5-minute interval (missing values are coded as NA).

* **date:** The date on which the measurement was taken in YYYY-MM-DD format.

* **interval:** Identifier for the 5-minute interval in which measurement was taken.
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### 1. Code for reading in the dataset and/or processing the data
Load the data using the *read_csv()* function. This function is from *readr* package.

```r
library(tidyverse)
```

```
## ── Attaching packages ──────────────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
## ✓ tibble  2.1.3     ✓ dplyr   0.8.5
## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.4.0
```

```
## ── Conflicts ─────────────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
activity <- read_csv("activity.csv")
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

### 2. Histogram of the total number of steps taken each day

```r
activity_by_days <- aggregate(steps ~ date, data = activity, FUN = sum, na.action = na.omit)
```


```r
#Histogram of total steps
ggplot(data = activity_by_days, aes(steps)) +
        geom_histogram(fill = "blue", bins = 8) +
        theme_light()
```

![](../activity_monitoring/figures/plot1-1.png)<!-- -->

### 3. Mean and median number of steps taken each day

```r
#Calculate mean
mean(activity_by_days$steps)
```

```
## [1] 10766.19
```

```r
#Calculate median
median(activity_by_days$steps)
```

```
## [1] 10765
```

### 4. Time series plot of the average number of steps taken

```r
activity_by_time <- aggregate(steps ~ interval,data = activity, FUN = mean, na.rm = TRUE)
```


```r
ggplot(data = activity_by_time, aes(x = interval, 
                                    y = steps)) +
        geom_line(color = "blue") +
        theme_light()
```

![](../activity_monitoring/figures/plot2-1.png)<!-- -->

### 5. The 5-minute interval that, on average, contains the maximum number of steps

```r
activity_by_time %>% 
        filter(steps == max(steps)) %>% 
        select(interval)
```

```
##   interval
## 1      835
```

### 6. Code to describe and show a strategy for imputing missing data
Calculate the number of missing data in the dataset

```r
activity %>% 
        filter(is.na(steps)) %>% 
        count()
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1  2304
```

Imputation using the mean

```r
filled_activity <- activity %>% 
        mutate(filled_steps = ifelse(is.na(steps), 
                                     round(aggregate(formula = steps ~ interval, FUN = mean)[[2]], 0),
                                     steps)) 
filled_activity
```

```
## # A tibble: 17,568 x 4
##    steps date       interval filled_steps
##    <dbl> <date>        <dbl>        <dbl>
##  1    NA 2012-10-01        0            2
##  2    NA 2012-10-01        5            0
##  3    NA 2012-10-01       10            0
##  4    NA 2012-10-01       15            0
##  5    NA 2012-10-01       20            0
##  6    NA 2012-10-01       25            2
##  7    NA 2012-10-01       30            1
##  8    NA 2012-10-01       35            1
##  9    NA 2012-10-01       40            0
## 10    NA 2012-10-01       45            1
## # … with 17,558 more rows
```

Building a new dataset with missing values imputed

```r
activity2 <- activity %>% 
        mutate(steps = ifelse(is.na(steps), 
                                     round(aggregate(formula = steps ~ interval, FUN = mean)[[2]], 0),
                                     steps)) 
```

### 7. Histogram of the total number of steps taken each day after missing values are imputed

```r
activity_by_days_2 <- aggregate(steps ~ date, data = activity2, FUN = sum, na.action = na.omit)
```


```r
#Histogram of total steps after missing values are imputed
ggplot(data = activity_by_days_2, aes(steps)) +
        geom_histogram(fill = "blue", bins = 8) +
        theme_light()
```

![](../activity_monitoring/figures/plot3-1.png)<!-- -->


```r
#Calculate mean after missing values are imputed
mean(activity_by_days_2$steps)
```

```
## [1] 10765.64
```

```r
#Calculate median after missing values are imputed
median(activity_by_days_2$steps)
```

```
## [1] 10762
```

After imputing missing data, the values do not differ from the estimates from the first part of the assignment. The impact of imputing missing data on the estimates is minimal. There is no big difference between in two results.

### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
Create a new factor variable in the dataset with two levels (“weekday” and “weekend”)

```r
activity2 <- activity2 %>% 
        mutate(day = weekdays(as.Date(date))) %>% 
        mutate(type_of_day = ifelse(day == "Saturday" | day == "Sunday",
                                    "weekend", "weekday"))
```


```r
activity_by_time_2 <- aggregate(steps ~ interval + type_of_day, data = activity2, FUN = mean, na.rm = TRUE)
```
Create the plot

```r
ggplot(data = activity_by_time_2, aes(x = interval, 
                                    y = steps)) +
        geom_line(color = "blue") +
        theme_light() +
        facet_grid(type_of_day ~ .)
```

![](../activity_monitoring/figures/plot4-1.png)<!-- -->




