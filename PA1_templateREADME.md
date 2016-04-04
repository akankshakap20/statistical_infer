# statistical_infer
coursera
library(ggplot2)

data <- read.csv("activity.csv")

total.steps <- tapply(data$steps, data$date, FUN = sum,na.rm=TRUE)
qplot(total.steps, binwidth = 1000,xlab="Total Steps per Day")

mean(total.steps, na.rm = TRUE)

median(total.steps, na.rm = TRUE)

a[which.max(a$steps), ]

missing <- is.na(data$steps)
table(missing) 

fill.value <- function(steps, interval)
{ filled <- NA
if (!is.na(steps))
  filled <- c(steps)
else filled <- (a[a$interval == interval, "steps"])
return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

total.steps <- tapply(filled.data$steps, filled.data$date, FUN = sum)
qplot(total.steps, binwidth = 1000, xlab = "total number of steps taken each day")

mean(total.steps)

median(total.steps)

weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
    return("weekday") else if (day %in% c("Saturday", "Sunday")) 
      return("weekend") else stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN = weekday.or.weekend)

a <- aggregate(steps ~ interval + day, data = filled.data, mean)
ggplot(a, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
  xlab("5-minute interval") + ylab("Number of steps")

