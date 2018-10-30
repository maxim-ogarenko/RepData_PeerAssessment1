#1 Code for reading in the dataset and/or processing the data
setwd("C:/Users/Admin/Documents/DS from Coursera/5 Reproducible Research/RepData_PeerAssessment1")
unzip("activity.zip")
activity.data <- read.csv("activity.csv")

#2 Histogram of the total number of steps taken each day
library(dplyr)
steps.daily.total <-  summarise(group_by(activity.data, date), steps=sum(steps))
hist(steps.daily.total$steps/1000, breaks = seq(0,25, by=2.5), ylim = c(0,20), col = "blue",  
     main="Total steps per day distribution", xlab = "Thousand steps per day")

#3 Mean and median number of steps taken each day
mean(steps.daily.total$steps, na.rm = TRUE)
median(steps.daily.total$steps, na.rm = TRUE)

#4 Time series plot of the average number of steps taken
steps.interval.average <- summarise(group_by(activity.data, interval), 
                                    steps=mean(steps, na.rm = TRUE))

plot(steps.interval.average$interval, steps.interval.average$steps, 
     type = "l", xlab="Interval", 
     ylab="Average number of steps", 
     main="Average number of steps per interval")

#5 The 5-minute interval that, on average, contains the maximum number of steps
steps.interval.average[which.max(steps.interval.average$steps), 1]

#6 Code to describe and show a strategy for imputing missing data
paste(round(sum(is.na(activity.data$steps))/nrow(activity.data)*100, digits = 1), "percent")

steps.no.nas <- steps.interval.average$steps[match(activity.data$interval, 
                                                    steps.interval.average$interval)]

activity.data.no.nas <- transform(activity.data, 
                                  steps = ifelse(is.na(activity.data$steps), 
                                        yes = steps.no.nas, 
                                        no = activity.data$steps))


#7 Histogram of the total number of steps taken each day after missing values are imputed
steps.daily.total.no.nas <-  summarise(group_by(activity.data.no.nas, date), steps=sum(steps))

hist(steps.daily.total.no.nas$steps/1000, breaks = seq(0,25, by=2.5), ylim = c(0,20), 
     col = "blue", main="Total steps per day distr'n after NAs imputation", 
     xlab = "Thousand steps per day")

#8 Panel plot comparing the average number of steps taken per 5-minute interval 
#across weekdays and weekends
activity.data.no.nas$date <- suppressWarnings(weekdays(as.Date(strptime(activity.data.no.nas$date, 
                                                       format = "%Y-%m-%d"))))

activity.data.no.nas$date <- replace(activity.data.no.nas$date, 
                                     activity.data.no.nas$date=="суббота" | 
                                             activity.data.no.nas$date=="воскресенье", 
                                     "weekend")

activity.data.no.nas$date <- replace(activity.data.no.nas$date, 
                                     !activity.data.no.nas$date=="weekend", "weekday")

activity.date.interval.total <- summarise(group_by(activity.data.no.nas, interval, date), 
                                          steps=mean(steps, na.rm = TRUE))

library(ggplot2)
ggplot(activity.date.interval.total, aes(x = interval, y = steps, color = date)) +
        geom_line() +
        labs(title = "Average daily steps day type", x = "Interval", 
             y = "Average number of steps") +
        facet_wrap(~date, ncol = 1, nrow=2)


#9 All of the R code needed to reproduce the results (numbers, plots, etc.) in the report