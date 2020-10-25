Loading and preprocessing the data
----------------------------------

Load library

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)
    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

Read the data and assign it’s content to “activity”

    activity <- read.table(unz("activity.zip", "activity.csv"), header=T, sep=",")
    print("See preview of loaded data")

    ## [1] "See preview of loaded data"

    head (activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

Process or transform the date field into the format "Year-Month-Day

    activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

What is mean total number of steps taken per day?
-------------------------------------------------

Compute total number of steps taken each day

    totalsteps <- activity %>% group_by(date) %>% summarise(total.steps = sum(steps, na.rm = TRUE))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    head(tibble(totalsteps))

    ## # A tibble: 6 x 2
    ##   date       total.steps
    ##   <date>           <int>
    ## 1 2012-10-01           0
    ## 2 2012-10-02         126
    ## 3 2012-10-03       11352
    ## 4 2012-10-04       12116
    ## 5 2012-10-05       13294
    ## 6 2012-10-06       15420

Histogram of the number of steps taken each day

    g <- ggplot(totalsteps, aes(x = date, y = total.steps))
    g <- g + geom_histogram(stat = "identity", colour = " black", fill = "green")

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

    g <- g + labs(title = "Histogram of the total number of steps taken each day", x = "Date", y = "Total steps")
    print(g)

![](PA1_template_files/figure-markdown_strict/histview-1.png)

Compute the mean of number of steps taken each day

    mean <- mean(totalsteps$total.steps, na.rm = TRUE)
    mean

    ## [1] 9354.23

Compute the median of number of steps taken each day

    median <- median(totalsteps$total.steps, na.rm = TRUE)
    median

    ## [1] 10395

Average daily activity pattern?
-------------------------------

Compute daily average steps

    averagesteps <- activity %>% group_by(interval) %>% summarise(avg.steps = mean(steps, na.rm = TRUE))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    head(tibble(averagesteps))

    ## # A tibble: 6 x 2
    ##   interval avg.steps
    ##      <int>     <dbl>
    ## 1        0    1.72  
    ## 2        5    0.340 
    ## 3       10    0.132 
    ## 4       15    0.151 
    ## 5       20    0.0755
    ## 6       25    2.09

Plot time series

    g <- ggplot(averagesteps, aes(x = interval, y = avg.steps))
    g <- g + geom_line(stat = "identity", col = "blue")
    g <- g + labs(title = "Time series plot of the average number of steps taken", x = "Intrval", y = "Average steps")

    print(g)

![](PA1_template_files/figure-markdown_strict/timeseries-1.png)

Compute the interval with the maximum number of steps on average

    intwthmaxsteps <- averagesteps$interval[averagesteps$avg.steps == max(averagesteps$avg.steps)]
    intwthmaxsteps

    ## [1] 835

The maximum average steps coincides with the 835th 5 minute interval

Imputing missing values
-----------------------

Total number of missing values in data set

    missingvalue <- activity[is.na(activity) == TRUE]
    length(missingvalue)

    ## [1] 2304

Method of of filling in missing values

    fills <-  function(x){ 
                     x$steps[is.na(x$steps)] <- ave(x$steps, x$interval,                                          FUN = function(z) 
                     mean(z, na.rm = TRUE))[c(which(is.na(x$steps)))]
    return(x)
    }
    activity_fld <- fills(activity)
    head(tibble(activity_fld))

    ## # A tibble: 6 x 3
    ##    steps date       interval
    ##    <dbl> <date>        <int>
    ## 1 1.72   2012-10-01        0
    ## 2 0.340  2012-10-01        5
    ## 3 0.132  2012-10-01       10
    ## 4 0.151  2012-10-01       15
    ## 5 0.0755 2012-10-01       20
    ## 6 2.09   2012-10-01       25

Histogram view of the number of steps taken each day over filled data

    totalsteps2 <- activity_fld %>% group_by(date) %>% summarise(total.steps = sum(steps))

    ## `summarise()` ungrouping output (override with `.groups` argument)

    head(tibble(totalsteps2))

    ## # A tibble: 6 x 2
    ##   date       total.steps
    ##   <date>           <dbl>
    ## 1 2012-10-01      10766.
    ## 2 2012-10-02        126 
    ## 3 2012-10-03      11352 
    ## 4 2012-10-04      12116 
    ## 5 2012-10-05      13294 
    ## 6 2012-10-06      15420

    ## Plotting total steps
    g <- ggplot(totalsteps2, aes(x = date, y = total.steps))
    g <- g + geom_histogram(stat = "identity", colour = " black", fill = "green")

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

    g <- g + labs(title = "Histogram of the total number of steps taken each day after missing values are imputed", x = "Date", y = "Total steps")
    print(g)

![](PA1_template_files/figure-markdown_strict/histview2-1.png)

Compute the new mean of number of steps per day

    mean <- mean(totalsteps2$total.steps)

    mean

    ## [1] 10766.19

Compute the new median of number of steps per day

    median <- median(totalsteps2$total.steps)
    median

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    activity_fld$wk_days <- weekdays(as.Date(activity_fld$date))

    i = 1

    for (i in 1:length(activity_fld$wk_days)){
        if(activity_fld$wk_days[i] == "Saturday"|activity_fld$wk_days[i] == "Sunday"){
            activity_fld$wk_grp[i] <- "Weekend"
        } else {
          activity_fld$wk_grp[i] <- "Weekday"
          }
    }

    averagesteps2 <- activity_fld %>% group_by(interval, wk_grp) %>% summarise(avg.steps = mean(steps, na.rm = TRUE))

    ## `summarise()` regrouping output by 'interval' (override with `.groups` argument)

    head(tibble(averagesteps2))

    ## # A tibble: 6 x 3
    ##   interval wk_grp  avg.steps
    ##      <int> <chr>       <dbl>
    ## 1        0 Weekday    2.25  
    ## 2        0 Weekend    0.215 
    ## 3        5 Weekday    0.445 
    ## 4        5 Weekend    0.0425
    ## 5       10 Weekday    0.173 
    ## 6       10 Weekend    0.0165

    library(lattice)
    g <- ggplot(averagesteps2, aes(x = interval, y = avg.steps))
    g <- g + geom_line(stat = "identity", col = "blue")
    g <- g + facet_wrap(~ wk_grp, ncol = 1)
    g <- g + labs(title = "Comparing the average number of steps taken per 5-minute interval", x = "Interval", y ="Average steps")
    print(g)

![](PA1_template_files/figure-markdown_strict/wk_days_end-1.png)

    #xyplot(avg.steps ~ interval | wk_grp, data = averagesteps2, type = "l", ylab = "Number of Steps", xlab = "Interval", layout = c(2,1))
