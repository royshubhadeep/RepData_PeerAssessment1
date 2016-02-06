
# Unzip the data file
if (!file.exists("activity.csv")) {
    unzip("activity.zip")
}

# Load the data
activity1 <- read.csv("activity.csv", header=TRUE, sep=",", na.strings="NA")

# Format the date field
activity1$date <- as.Date(as.character(activity1$date), "%Y-%m-%d")

# Calculate the total number of steps taken per day
totByDay1 <- aggregate(steps~date, data=activity1, FUN=sum)

# Make a histogram of the total number of steps taken each day
hist(totByDay1$steps, xlab="Total number of steps", 
     main="Total number of steps taken each day"
)

# Calculate and report the mean and median of the total number of steps taken 
# per day
abline(v=mean(totByDay1$steps), lwd=3, lty=1, col="red")
abline(v=median(totByDay1$steps), lwd=3, lty=2, col="blue")

print(mean(totByDay1$steps))
print(median(totByDay1$steps))

# Calculate the daily average activity by interval
avgByInterval1 <- aggregate(steps~interval, data=activity1, FUN=mean)

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
with(avgByInterval1, 
     plot(x=interval, y=steps, xlab="5 minute Intervals", 
          ylab="Average no. of steps", col="blue"
          main="Avg. steps for 5 minute intervals across all days", type="l", 
     )
)

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
print(avgByInterval1[which.max(avgByInterval1$steps),])

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
print(sum(is.na(activity1$steps)))


# Fill the missing values (NA) with the mean of that 5 minute interval
# Create a new dataset that is equal to the original dataset but with the 
# missing data filled in

library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))


activity2 <- ddply(activity, ~ interval, transform, steps = impute.mean(steps))
# Format the date field
activity2$date <- as.Date(as.character(activity2$date), "%Y-%m-%d")
activity2  <- arrange(activity2, date)

# Calculate the total number of steps taken per day
totByDay2 <- aggregate(steps~date, data=activity2, FUN=sum)

# Make a histogram of the total number of steps taken each day
hist(totByDay2$steps, xlab="Total number of steps", 
     main="Total number of steps taken each day"
)

# Calculate and report the mean and median of the total number of steps 
# taken per day
abline(v=mean(totByDay2$steps), lwd=3, lty=1, col="red")
abline(v=median(totByDay2$steps), lwd=3, lty=2, col="blue")

print(mean(totByDay2$steps))
print(median(totByDay2$steps))

# Create a new factor variable in the dataset with two levels - "weekday" and 
# "weekend" indicating whether a given date is a weekday or weekend day
weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
activity2$day <- factor((weekdays(activity2$date) %in% weekday), 
                        levels=c(FALSE, TRUE), labels=c("weekend", "weekday"))

# Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis).
library(lattice)
xyplot(steps~interval | as.factor(day), data=activity2, layout=c(1,2), 
       type="l", xlab="Interval", ylab="No. of steps", 
       main="Avg. # of steps taken in 5 minute intervals on Weekdays/Weekends",
)
