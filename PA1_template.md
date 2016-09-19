# Reproducible Research: Peer Assessment 1
Javier Iglesia Aparicio  
19 de septiembre de 2016  


## Loading and preprocessing the data
First we read the activity.csv file


```r
#unzip("activity.zip")
data <- read.csv("activity.csv", header=TRUE, sep=",")
# Convert date in a Date format
data$date <- as.Date(data$date,"%Y-%m-%d")
# Some previous look into the data
names(data)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
## What is mean total number of steps taken per day?
For this part of the assignment, we can ignore the missing values in the dataset.

1.Calculate the total number of steps taken per day.


```r
stepsByDay<-aggregate(steps~date,data=data,sum)
```

2.Make a histogram of the total number of steps taken each day

```r
hist(stepsByDay$steps, breaks=30, main="Freq. of total number of steps taken each day", xlab="# of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3.Calculate and report the mean and median of the total number of steps taken per day

```r
meanWithNA<-mean(stepsByDay$steps)
medianWithNA<-median(stepsByDay$steps)
```
* The **mean** is **1.0766189\times 10^{4}**
* The **median** is **10765**

## What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsByInterval <- aggregate(steps~interval,data=data,mean)
plot(steps~interval,data=stepsByInterval,type="l", main="Average number steps taken 5-minute interval", xlab="# of interval", ylab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsByInterval[which.max(stepsByInterval$steps),]$interval
```
The **interval which contains the maximun number of steps** is the **835**


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data))
```

The **total number of missing data in the dataset** is **2304**

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We apply the mean for a particular 5-minute interval to fill NA values. We have these values in previously computed variable stepsByInterval. So now we identify any NA value a put the median value for each interval.



3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
dataWithoutNA <- data
datafilled <- 0

for (i in 1:nrow(dataWithoutNA)){

if(is.na(dataWithoutNA[i,]$steps)){
    
    dataWithoutNA[i,]$steps <- stepsByInterval[stepsByInterval$interval==dataWithoutNA[i,]$interval,]$steps
    datafilled<-datafilled + 1
}
}
```

2304 NA values have been filled wiht the 5-minutes mean steps data


4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
stepsByDayWithoutNA<-aggregate(steps~date,data=dataWithoutNA,sum)
hist(stepsByDayWithoutNA$steps, breaks=30, main="Freq. of total number of steps taken each day (NA filled)", xlab="# of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
meanWithoutNA <- mean(stepsByDayWithoutNA$steps)
medianWithoutNA <- median(stepsByDayWithoutNA$steps)
```
* The **mean** is **1.0766189\times 10^{4}**
* The **median** is **1.0766189\times 10^{4}**

The difference is minimal:

Value  | Including NA values | With NA filled     | Difference (nonfilled - filled data)
-----  | ------------------- | ------------------ | ----------
Mean   | 1.0766189\times 10^{4}      | 1.0766189\times 10^{4}  | 0
Median | 10765    | 1.0766189\times 10^{4} | -1.1886792


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
dataWithoutNA$typeOfDay <- ifelse(as.POSIXlt(as.Date(dataWithoutNA$date))$wday%%6==0,                       "weekend","weekday")
# Sunday and Saturday are weekend days; Rest : weekday 
dataWithoutNA$typeOfDay <- factor(dataWithoutNA$typeOfDay,levels=c("weekday","weekend"))

str(dataWithoutNA)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps    : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date     : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ typeOfDay: Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```



2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
stepsByIntervalWithoutNA <- aggregate(steps~interval+typeOfDay,dataWithoutNA,mean)

library("lattice")

xyplot(steps ~ interval | typeOfDay, stepsByIntervalWithoutNA, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

