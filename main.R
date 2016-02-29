library(dplyr)

data <- read.csv(unz("activity.zip", "activity.csv"))
data$interval <- as.factor(as.character(data$interval))
data <- tbl_df(data)

daily.total <- summarise(data_by_date, sum(steps, na.rm = TRUE))
names(daily.total)[2] <- "daily.total.steps"

hist(daily.total$daily.total.steps)

summary(daily.total$daily.total.steps)

data_by_date <- group_by(data, date)

