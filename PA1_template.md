##Load the data

activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
str(activity)


#What is mean total number of steps taken per day?
-------------------------------------------------
    
#1.Calculate the total number of steps taken per day.
TotalSteps<-aggregate(steps~date,data=activity,sum,na.rm=TRUE)
TotalSteps


#2.Make a histogram of the total number of steps taken each day
hist(TotalSteps$steps, main = "Total steps by day", xlab = "day")
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()


#3.Calculate and report the mean and median total number of steps taken per day 
meanSteps <- mean(TotalSteps$steps)
medianSteps <-median(TotalSteps$steps)
meanSteps
medianSteps


The mean total number of steps taken per day is `r meanSteps` steps.
The median total number of steps taken per day is `r medianSteps` steps.


#What is the average daily activity pattern?
-------------------------------------------
#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r timeseriesplot}
StepsInterval<-aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
plot(steps~interval,data=StepsInterval,type="l")
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 
    
    ```{r maxnumber}
max <- StepsInterval[which.max(StepsInterval$steps),]$interval
max
```

It is the `r max`th interval.

#Imputing missing values
-----------------------
    
    1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r missingvalues}
missing <- sum(is.na(activity$steps))
missing
```

Total `r missing` rows are missing.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r fillingNA}
IntervalFilling<-function(interval){
    StepsInterval[StepsInterval$interval==interval,]$steps
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r newdata}

ActivityFilled <- activity
count=0           
for(i in 1:nrow(ActivityFilled)){
    if(is.na(ActivityFilled[i,]$steps)){
        ActivityFilled[i,]$steps<-IntervalFilling(ActivityFilled[i,]$interval)
        count=count+1
    }
}
cat("Total ",count, "NA values were filled.\n\r")  
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
    
    ```{r histogram2}
TotalSteps2<-aggregate(steps~date,data=ActivityFilled,sum)
hist(TotalSteps2$steps, main = "Total steps by day", xlab = "day")
meanSteps2 <- mean(TotalSteps2$steps)
medianSteps2 <- median(TotalSteps2$steps)
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()
```

The mean total number of steps taken per day is 
`r meanSteps2` steps.
The median total number of steps taken per day is 
`r medianSteps2` steps.

The mean value is the same as the value before inserting missing data because we put the mean value for that particular 5-min interval. The median value shows a slight difference.

#Are there differences in activity patterns between weekdays and weekends?
---------------------------------------------------------------------------
    
    1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r daytype}

ActivityFilled$day=ifelse(as.POSIXlt(as.Date(ActivityFilled$date))$wday%%6==0,
                          "weekend","weekday")
ActivityFilled$day=factor(ActivityFilled$day,levels=c("weekday","weekend"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r panelplot}
StepsInterval2=aggregate(steps~interval+day,ActivityFilled,mean)
library(lattice)
xyplot(steps~interval|factor(day),data=StepsInterval2,aspect=1/2,type="l", xlab = "Interval",ylab = "Number of steps")
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()
```



