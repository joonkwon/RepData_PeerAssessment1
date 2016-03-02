library(dplyr)

data <- read.csv(unz("activity.zip", "activity.csv"))
data$interval <- as.factor(as.character(data$interval))
data <- tbl_df(data)
data.by.date <- group_by(data,date)

daily.total <- summarise(data.by.date, sum(steps, na.rm = TRUE))
names(daily.total)[2] <- "daily.total.steps"

hist(daily.total$daily.total.steps)

summary(daily.total$daily.total.steps)

data.by.interval <- group_by(data,interval)
mean.by.interval <- summarise(data.by.interval, mean(steps, na.rm=TRUE))
names(mean.by.interval)[2] <- "mean.steps"

mean.by.interval$interval <- as.numeric(as.character(mean.by.interval$interval))
mean.by.interval.sorted <- mean.by.interval[order(mean.by.interval$interval),]
with(mean.by.interval.sorted, plot(interval, mean.steps, type="l"))

data.missing <- data[is.na(data$steps),]
dim(data.missing)

data.filled <- merge(data, mean.by.interval.sorted, by.x = "interval", by.y="interval")
data.filled <- transform(data.filled, steps = ifelse(is.na(steps), mean.steps, steps))
data.filled <- select(data.filled, 1:3)
#data.filled$interval <- as.numeric(as.character(data.filled$interval))
#data.filled <- data.filled[order(data.filled$interval),]
data.filled.by.date <- group_by(data.filled, date)
sum.by.date <- summarise(data.filled.by.date, daily.sum=sum(steps))
hist(sum.by.date$daily.sum)

data.filled.sorted <- data.filled[order(as.numeric(as.character(data.filled$interval))),]
data.filled.sorted$date <- as.Date(data.filled$date)
data.filled.sorted$day <- weekdays(data.filled.sorted$date)
data.filled.sorted$weekday <- ifelse(data.filled.sorted$day == "Sunday", FALSE, 
                                     ifelse(data.filled.sorted$day == "Saturday", FALSE, TRUE))

data.filled.by.interval.weekday <- group_by(data.filled.sorted, interval,weekday)


