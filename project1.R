library('ggplot2')
library('reshape2')
library('plyr')
library('timeDate')
library('dplyr')

unzip('activity.zip')


data <- read.csv(file = 'activity.csv', header = TRUE, colClasses = c('numeric', 'Date', 'numeric'))

#Sys.setlocale('LC_TIME', 'English')

data$weekday <- weekdays(data$date)

data.WithNA <- data

data.NoNA <- data[complete.cases(data),]

sum.steps.day <- ddply(data.NoNA, .(date), summarise, steps = sum(steps))

plot(sum.steps.day$date, sum.steps.day$steps, type = "h", main = "Histogram of daily steps", xlab = "Date", ylab = "Steps per day", col = "blue", lwd = 8)
abline(h = mean(sum.steps.day$steps), col = "red", lwd = 2)

paste("Mean steps per Day =", round(mean(sum.steps.day$steps), 0))

paste("Median steps per Day =", round(median(sum.steps.day$steps), 0))



mean.steps.interval <- ddply(data.WithNA, .(interval), summarise, steps = mean(steps, na.rm = TRUE))


plot(mean.steps.interval$interval, mean.steps.interval$steps, type = "l", main = "Average daily activity by interval", xlab = "Interval",
     ylab = "Steps per interval", col = "blue", lwd = 2)
abline(h = mean(mean.steps.interval$steps, na.rm = TRUE), col = "red", lwd = 2)

paste("Maximum number of steps in interval =", mean.steps.interval$interval[which.max(mean.steps.interval$steps)])
paste("Maximum number of steps =", round(max(mean.steps.interval$steps), 0))


sum(is.na(data.WithNA$steps))


mean.weekday <- ddply(data.WithNA, .(interval, weekday), summarise, steps = round(mean(steps, na.rm = TRUE), 2))

naIndex = which(is.na(data.WithNA$steps))

merged.NA = merge(data.WithNA, mean.steps.interval, by = "interval", suffixes = c(".actual", ".stepsInt"))

data.Complete <- data.WithNA

data.Complete[naIndex, "steps"] <- merged.NA[naIndex, 'steps.stepsInt']

paste("Missing values in new dataset = ", sum(is.na(data.Complete)))

steps.day <- ddply(data.Complete, .(date), summarise, steps = round(sum(steps, na.rm = TRUE), 0))

plot(steps.day$date, steps.day$steps, type = "h", main = "Histogram of daily steps (added NA Values)", xlab = "Date", ylab = "Steps per day", col = "blue", lwd = 8)
abline(h = mean(steps.day$steps, na.rm = TRUE), col = "red", lwd = 2)


sum.steps.day <- ddply(data.Complete, .(date), summarise, steps = sum(steps))

paste("Mean steps per Day =", round(mean(sum.steps.day$steps), 0))

paste("Median steps per Day =", round(median(sum.steps.day$steps), 0))


data.Complete$daytype <- lapply(data.Complete$date, function(x) ifelse(isWeekday(x, wday = 1:5), 'weekday', 'weekend'))

data.Complete$daytype <- unlist(data.Complete$daytype, use.names = TRUE)

data.Complete$daytype <- as.factor(data.Complete$daytype)


day.interval.Steps <- ddply(data.Complete, .(interval, daytype), summarise, steps = mean(steps, na.rm = TRUE))

ggplot(day.interval.Steps, aes(x = interval, y = steps)) +
  geom_line(col='blue') +
  ylab('Number of steps') + xlab("Interval") +
  ggtitle("Number of Steps per Interval (weekend/weekend") +
  facet_grid(daytype ~ .)
