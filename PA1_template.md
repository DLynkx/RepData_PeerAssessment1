Loading and preprocessing the data
----------------------------------

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

Compute total number of steps

    totalsteps <- sum(activity$steps, na.rm = TRUE)

    totalsteps

    ## [1] 570608

Histogram of the number of steps taken each day

    hist(activity$steps[activity$steps > 0], main = " Steps Per Day ", xlab = "Number of Steps", col = "green", breaks = 100)

![](PA1_template_files/figure-markdown_strict/histview-1.png)

Bar plot of the number of steps taken each day

    barplot(activity$steps, main = "Steps Per Day", xlab = "Number of Steps")

![](PA1_template_files/figure-markdown_strict/barview-1.png)

Compute the mean of number of steps taken each day

    m1 <- mean(activity$steps, na.rm = TRUE)

    m1

    ## [1] 37.3826

Compute the median of number of steps taken each day

    m2 <- median(activity$steps, na.rm = TRUE)
    m2

    ## [1] 0

What is the average daily activity pattern?
-------------------------------------------

Load relevant libraries and compute daily average steps and plot time
series

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    avgstep <- tapply(activity$steps, activity$interval, mean, na.rm = T)
    plot(avgstep, type = "l" , col = "darkred")

![](PA1_template_files/figure-markdown_strict/activitypattern-1.png)

Compute the interval with the maximum number of steps on average

    interval <- data.frame(avgstep[avgstep == max(avgstep)])
    interval

    ##     avgstep.avgstep....max.avgstep..
    ## 835                         206.1698

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

Histogram view of the number of steps taken each day over filled data

    hist(activity_fld$steps[activity_fld$steps > 0], main = " Steps Per Day ", xlab = "Number of Steps", col = "green", breaks = 100)

![](PA1_template_files/figure-markdown_strict/histview2-1.png)

Compute the mean of number of steps per day

    m1 <- mean(activity_fld$steps, na.rm = TRUE)

    m1

    ## [1] 37.3826

Compute the median of number of steps per day

    m2 <- median(activity_fld$steps, na.rm = TRUE)
    m2

    ## [1] 0

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

    library(lattice)
    activity_fld <- data.frame(activity_fld)
    activity_fld <- group_by(activity_fld, wk_grp, interval)
    activity_fld <- summarise(activity_fld, average = mean(steps))

    ## `summarise()` regrouping output by 'wk_grp' (override with `.groups` argument)

    activity_fld

    ## # A tibble: 576 x 3
    ## # Groups:   wk_grp [2]
    ##    wk_grp  interval average
    ##    <chr>      <int>   <dbl>
    ##  1 Weekday        0  2.25  
    ##  2 Weekday        5  0.445 
    ##  3 Weekday       10  0.173 
    ##  4 Weekday       15  0.198 
    ##  5 Weekday       20  0.0990
    ##  6 Weekday       25  1.59  
    ##  7 Weekday       30  0.693 
    ##  8 Weekday       35  1.14  
    ##  9 Weekday       40  0     
    ## 10 Weekday       45  1.80  
    ## # ... with 566 more rows

    xyplot(average ~ interval | wk_grp, data = activity_fld, type = "l", ylab = "Number of Steps", xlab = "Interval")

![](PA1_template_files/figure-markdown_strict/wk_days_end-1.png)
