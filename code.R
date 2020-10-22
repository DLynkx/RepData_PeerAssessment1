

activity <- read.table(unz("activity.zip", "activity.csv"), header=T, sep=",")
print("See preview of loaded data")
head (activity)


##Process or transform the date field into the format "Year-Month-Day


activity$date <- as.Date(activity$date, format = "%Y-%m-%d")



## What is mean total number of steps taken per day?

##Compute total number of steps


totalsteps <- sum(activity$steps, na.rm = TRUE)

totalsteps

#Histogram of the number of steps taken each day



hist(activity$steps[activity$steps > 0], main = " Steps Per Day ", xlab = "Number of Steps", col = "green", breaks = 100)


#Bar plot of the number of steps taken each day


barplot(activity$steps, main = "Steps Per Day", xlab = "Number of Steps")


#Compute the mean of number of steps taken each day



m1 <- mean(activity$steps, na.rm = TRUE)

m1


#Compute the median of number of steps taken each day


m2 <- median(activity$steps, na.rm = TRUE)
m2

## What is the average daily activity pattern?

#Load relevant libraries and compute daily average steps and plot time series


library(dplyr)
avgstep <- tapply(activity$steps, activity$interval, mean, na.rm = T)
plot(avgstep, type = "l" , col = "darkred")

```

#Compute the interval with the maximum number of steps on average



interval <- data.frame(avgstep[avgstep == max(avgstep)])
interval


#The maximum average steps coincides with the 835th 5 minute interval

## Imputing missing values

#Total number of missing values in data set

missingvalue <- activity[is.na(activity) == TRUE]
length(missingvalue)


#Method of of filling in missing values


fills <-  function(x){ 
                 x$steps[is.na(x$steps)] <- ave(x$steps, x$interval,                                          FUN = function(z) 
                 mean(z, na.rm = TRUE))[c(which(is.na(x$steps)))]
return(x)
}
activity_fld <- fills(activity)


#Histogram view of the number of steps taken each day



hist(activity_fld$steps[activity_fld$steps > 0], main = " Steps Per Day ", xlab = "Number of Steps", col = "green", breaks = 100)


#Compute the mean of number of steps per day



m1 <- mean(activity_fld$steps, na.rm = TRUE)

m1


#Compute the median of number of steps per day



m2 <- median(activity_fld$steps, na.rm = TRUE)
m2


## Are there differences in activity patterns between weekdays and weekends?



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
activity_fld
xyplot(average ~ interval | wk_grp, data = activity_fld, type = "l", ylab = "Number of Steps", xlab = "Interval")


