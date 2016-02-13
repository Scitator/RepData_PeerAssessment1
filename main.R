## requirements
# data analysis:
require("dplyr")
# plots:
require("ggplot2")
require("lattice")

activity <- read.csv("activity.csv")

## What is mean total number of steps taken per day?

# Make a histogram of the total number of steps taken each day
activityByDays <- activity %>% group_by(date) %>% summarise(stepsSum = sum(steps))

hist(activityByDays$stepsSum, 
     breaks = 30, 
     xlab="Mean total number of steps", 
     main = "Histogram of Total Steps per Day",
     col="blue")

# Calculate and report the mean and median of the total number of steps taken per day
print(mean(activityByDays$stepsSum, na.rm = TRUE))
print("Mean of the total number of steps taken per day:")
# 10766.19
print("Median of the total number of steps taken per day:")
print(median(activityByDays$stepsSum, na.rm = TRUE))
# 10765

rm(activityByDays)


## What is the average daily activity pattern?

activityByinterval <- activity %>% group_by(interval) %>% 
  summarise(stepsMean = mean(steps, na.rm =TRUE))

plot(activityByinterval$interval, 
     activityByinterval$stepsMean, 
     type="l", 
     lwd=2, 
     xlab = "5-minute interval", 
     ylab = "mean steps", 
     main = "Daily Activity Pattern", 
     col="blue")
axis(1, at = seq(100, 2300, by = 100))

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
maxStepsInterval <- which.max(activityByinterval$stepsMean)
print("5-minute interval, that contains the maximum number of steps:")
print(activityByinterval[maxStepsInterval,])

rm(activityByinterval, maxStepsInterval)


## Imputing missing values

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
print("Total number of missing values in the dataset:")
print(sum(is.na(activity$steps)))

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

activity$weekday <- weekdays(as.Date(activity$date))
activity$weekday <- factor(activity$weekday, 
                           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
                                      "Saturday", "Sunday"))

activityByDay <- activity %>% group_by(weekday, interval) %>%
  summarise(stepsMean = mean(steps, na.rm =TRUE))

# Create a new dataset that is equal to the original dataset but with the missing data filled in.

activityExtra <- merge(activity, activityByDay, by=c("weekday","interval"))
activityExtra$stepsExtra <- ifelse(is.na(activityExtra$steps),
                                   activityExtra$stepsMean, 
                                   activityExtra$steps)

# Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. 
# Do these values differ from th### Clean environmente estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?

activityExtraMean <- activityExtra %>% 
  group_by(date) %>%
  summarise(stepsSum = sum(stepsExtra))

hist(activityExtraMean$stepsSum, 
     breaks = 30, 
     xlab="Mean total number of steps", 
     main = "Histogram of Total Steps per Day using Imputed Data",
     col="red")

activityByDays <- activity %>% 
  group_by(date) %>% 
  summarise(stepsSum = sum(steps))

hist(activityByDays$stepsSum, 
     breaks = 30, 
     xlab="Mean total number of steps", 
     main = "Histogram of Total Steps per Day",
     col="blue",
     add=T)

legend("topright", c("Imputed", "Non-imputed"), col=c("red", "blue"), lwd=10)

print(mean(activityExtraMean$stepsSum))

print(median(activityExtraMean$stepsSum))

activityByDayMean <- activity %>% group_by (date, weekday)  %>%  summarise(stepsSum = sum(steps)) %>% 
  group_by (weekday) %>% summarise(stepsMean = round(mean(stepsSum, na.rm = TRUE),0))
print(activityByDayMean)

rm(activityByDay, activityByDayMean, plot3)


## Are there differences in activity patterns between weekdays and weekends?

activityExtra <- activityExtra %>% 
  mutate(weekend = ifelse(weekday == "Saturday" | weekday == "Sunday", "weekend", "weekday"))

activityExtraMean <- activityExtra %>% group_by(weekend, interval) %>% 
  summarise(stepsMean = mean(stepsExtra))
plot4 <- xyplot(stepsMean ~ interval | weekend, data = activityExtraMean, 
                type = "l", layout = c(1,2), xlab = "Interval", ylab = "Number of Steps", 
                main = "Average Steps by 5-minute Interval for Weekends and Weekdays")
print(plot4)

rm(activityExtra, activityExtraMean, plot4)

rm(activity)