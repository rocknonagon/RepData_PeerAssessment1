library(dplyr)
library(ggplot2)

## Loading and preprocessing the data

unzip("activity.zip")
activity.data <- read.csv(
    "activity.csv", 
    na.strings="NA",
    col.names = c("steps", "date", "interval"),
    colClasses = c("numeric", "Date", "character")
)

activity.data <- mutate(
    activity.data, 
    interval= sprintf("%04s",interval)
)

## What is mean total number of steps taken per day?
steps_per_day <- activity.data %>% 
    group_by(date) %>%
    summarize(total_steps = sum(steps, na.rm=TRUE))
days_with_steps <- aggregate(steps ~ date, data = activity.data, sum)
mean(days_with_steps$steps)

## What is the average daily activity pattern?
steps_per_interval <- activity.data %>% 
    group_by(interval) %>%
    summarize(steps = mean(steps, na.rm=TRUE))
    
##OR:
intervals_with_steps <- aggregate(steps ~ interval, 
                                  data = activity.data, 
                                  mean)
## Need to fix labels


filter(steps_per_interval, steps == max(steps_per_interval$steps))

## Imputing missing values
table(is.na(activity.data))
table(is.na(activity.data))[["TRUE"]]
activity.data <- mutate(activity.data, isna= is.na(steps))
nas <- activity.data %>% group_by(date) %>% summarize(allna = all(isna), anyna = any(isna))
nas %>% filter(anyna==TRUE)
## The NA's all occur on particular days. 8*288 = 2304.
## So it makes the most sense to use the average of that interval.
## I'll use the mean
steps_for_interval = function(t){
    steps_per_interval[steps_per_interval$"interval"==t,"steps"]
}
steps2 <- steps_for_interval(activity.data$interval)
activity.data <- data.frame(activity.data, steps2 = steps2$steps)
#activity.data$steps2 <- sapply(activity.data$interval, steps_for_interval)

imputed.data <- activity.data %>% 
                    mutate(steps = ifelse(isna==FALSE, 
                                        steps, 
                                        steps2
                                        )) %>% 
                    select(steps, date, interval, isna)

days_with_steps <- aggregate(steps ~ date, data = imputed.data, sum)
mean(days_with_steps$steps)

#     
## Are there differences in activity patterns between weekdays and weekends?
weekends <- activity.data %>%
    filter(weekdays(date) %in% c("Saturday", "Sunday"))

    
weekdays <- activity.data %>%
    filter(weekdays(date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

weekend_pattern <- aggregate(steps ~ interval, 
                             data = weekends, 
                             mean)

weekday_pattern <- aggregate(steps ~ interval, 
                             data = weekdays, 
                             mean)

pattern.data <- activity.data %>% 
    mutate(daytype = ifelse(
                            weekdays(date) %in% c("Saturday", "Sunday"), 
                            "weekend",
                            "weekday")
           ) %>% 
    group_by(daytype, interval) %>%
    summarize(steps = mean(steps, na.rm=TRUE))

g <- ggplot(pattern.data, aes(x = interval, y = steps, colour = daytype))
g + geom_line(aes(group=daytype)) + facet_grid(. ~ daytype)

