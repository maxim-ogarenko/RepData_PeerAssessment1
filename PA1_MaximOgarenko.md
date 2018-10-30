---
title: "Reproducible Research PA 1"
author: "Maxim Ogarenko"
date: '30 окт€бр€ 2018 г '
output: html_document
---

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices. 

This assignment makes use of data from a such a device, which collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The original data is downloadable
[here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).


#1 Reading in the dataset and the preliminary data exploration
Firstly, I unzip and read the original data into a CSV dataset:
```{r read, include=TRUE}
setwd("C:/Users/Admin/Documents/DS from Coursera/5 Reproducible Research/RepData_PeerAssessment1")
unzip("activity.zip")
activity.data <- read.csv("activity.csv")
```

Secondly, I load the necessary R libraries (dplyr and ggplot2):
```{r libraries, include=TRUE}
library(dplyr)
library(ggplot2)
```

Thirdly, I do the preliminary data exploration:
```{r explore, echo=FALSE}
paste("The dataset is of the", class(activity.data), "class.")

"The dataset has the following dimensions:"
dim(activity.data) 

"The columns have the following names:"
names(activity.data)

"The beginning of the dataset looks like this:"
head(activity.data)

paste("There are observations for", length(unique(activity.data$date)), "consequent days,")
paste("Each day is divided into", length(unique(activity.data$interval)), "5-minutes intervals.")
```

#2 Histogram of the total number of steps taken each day
```{r hist1, include=TRUE}
steps.daily.total <-  summarise(group_by(activity.data, date), steps=sum(steps))
hist(steps.daily.total$steps/1000, breaks = seq(0,25, by=2.5), ylim = c(0,20), col = "blue",  
     main="Total steps per day distribution", xlab = "Thousand steps per day")
```

#3 Mean and median number of steps taken each day
```{r mean mean, echo=FALSE}
paste("The mean number of steps per day is ", round(mean(steps.daily.total$steps/1000, na.rm = TRUE), 1), "thousand")

paste("The median number of steps per day is ", round(median(steps.daily.total$steps/1000, na.rm = TRUE), 1), "thousand")
```

#4 Time series plot of the average number of steps taken
```{r plot 4, echo=TRUE}
steps.interval.average <- summarise(group_by(activity.data, interval), 
                                    steps=mean(steps, na.rm = TRUE))

plot(steps.interval.average$interval, steps.interval.average$steps, 
     type = "l", xlab="Interval", 
     ylab="Average number of steps", 
     main="Average number of steps per interval")
```

#5 The 5-minute interval that, on average, contains the maximum number of steps
```{r max interval, echo=FALSE}
paste("Interval #", steps.interval.average[which.max(steps.interval.average$steps), 1], "on average, contains the maximum number of steps")
```

#6 Code to describe and show a strategy for imputing missing data
```{r NAs, echo=FALSE}
paste("In the original dataset", round(sum(is.na(activity.data$steps))/nrow(activity.data)*100, digits = 1), "percent of observations contain missing data for steps.")
```

I chose to replace those missing data with daily averages for the respective time intervals:
```{r Imputation, include=TRUE}
steps.no.nas <- steps.interval.average$steps[match(activity.data$interval, 
                                                    steps.interval.average$interval)]

activity.data.no.nas <- transform(activity.data, 
                                  steps = ifelse(is.na(activity.data$steps), 
                                        yes = steps.no.nas, 
                                        no = activity.data$steps))
```

#7 Histogram of the total number of steps taken each day after missing values are imputed
```{r Hist2, echo=TRUE}
steps.daily.total.no.nas <-  summarise(group_by(activity.data.no.nas, date), steps=sum(steps))

hist(steps.daily.total.no.nas$steps/1000, breaks = seq(0,25, by=2.5), ylim = c(0,20), 
     col = "blue", main="Total steps per day distribution after NAs imputation", 
     xlab = "Thousand steps per day")
```

#8 Are there differences in activity patterns between weekdays and weekends?
```{r weekdays, include=TRUE}
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
```

```{r ggplot, echo=TRUE}
ggplot(activity.date.interval.total, aes(x = interval, y = steps, color = date)) +
        geom_line() +
        labs(title = "Average number of steps per interval, weekday VS wekend", x = "Interval", 
             y = "Average number of steps") +
        facet_wrap(~date, ncol = 1, nrow=2)
```





