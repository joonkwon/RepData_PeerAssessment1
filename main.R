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





