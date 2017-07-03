#Reproducible Research: Peer-graded Assignment: Course project 1
================================================================

##Explicitely setting the defaults and loading R packages



```r
library(ggplot2)
library (dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
```

##1.Loading the data

It is assumed that the Working Directory is set correctly, i.e. is set to where the file "repdata_data_activity.zip" is saved. 


```r
unzip(zipfile="repdata_data_activity.zip")

data <- read.csv("activity.csv")
```

##2. & 3. Histogram of the total number of steps taken each day, and mean and median


```r
dayly.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)

qplot(dayly.steps, binwidth=1000, xlab="Total Number of Steps taken each Day")
```

![plot of chunk Question2-3](figure/Question2-3-1.png)

```r
mean(dayly.steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(dayly.steps, na.rm=TRUE)
```

```
## [1] 10395
```

##4. Time series plot of the average number of steps taken


```r
average.steps <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)
ggplot(data=average.steps, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-Minute Interval") +
    ylab("Average Number of Steps taken")
```

![plot of chunk Question4](figure/Question4-1.png)

##5. The 5-minute interval that, on average, contains the maximum number of steps


```r
average.steps[which.max(average.steps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

##6. Code to describe and show a strategy for imputing missing data


This is the percentage of intervals that have no values for the number of steps in that interval:


```r
mean(is.na(data$steps))*100
```

```
## [1] 13.11475
```

This is the total number of intervals that have no values for the number of steps in that interval:


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

We will replace each missing value with the mean.


```r
cleaned.data<-data

for (row.num in 1:nrow(cleaned.data)) {

    if (is.na(cleaned.data[row.num, "steps"])) {

        interval.sgn<- cleaned.data[row.num, "interval"]
        interval.sgn.ind <- which(average.steps[, "interval"] == interval.sgn)
        interval.steps.mean <- average.steps[interval.sgn.ind, "steps"]
        cleaned.data[row.num, "steps"] <- interval.steps.mean
         }
}
```

##7. Histogram of the total number of steps taken each day after missing values are imputed


```r
steps.cleaned <- tapply(cleaned.data$steps, cleaned.data$date, FUN=sum)

qplot(steps.cleaned, binwidth=1000, xlab="Total Number of steps per Day")
```

![plot of chunk Question7](figure/Question7-1.png)

```r
mean(steps.cleaned)
```

```
## [1] 10766.19
```

```r
median(steps.cleaned)
```

```
## [1] 10766.19
```
##8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
Sys.setenv(LANGUAGE = "en")
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
days.of.week <- weekdays(as.Date(cleaned.data$date))

cleaned.data$day.type <- sapply(days.of.week, function(which.day) {
    if (which.day %in% c("Saturday", "Sunday")) {
        return("weekend")
    } else {
        return("weekday")
           }
    })

steps.per.daytype <- cleaned.data %>% group_by(interval, day.type) %>% summarise(avg.steps = mean(steps))

xyplot(avg.steps ~ interval | day.type, data = steps.per.daytype, type = "b", layout = c(1, 2))
```

![plot of chunk Question8](figure/Question8-1.png)
