Coursera Reproducible Research Project 1 - Activity Monitoring Devices Study
============================================================================

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Loading and preprocessing the data
----------------------------------

Setup the WD, download and unzip the file

``` r
setwd('C:/Users/mohao/Desktop/Coursera_Reproducible Research_PA 1')
if(!file.exists('data')) dir.create('data')
fileUrl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(fileUrl,destfile='./data/repdata_data_activity.zip')
unzip('./data/repdata_data_activity.zip', exdir = './data')
```

Read the data and convert the date type

``` r
activityData <- read.csv('./data/activity.csv', na='NA')
activityData$date<-as.Date(activityData$date, format='%Y-%m-%d')
```

What is mean total number of steps taken per day?
-------------------------------------------------

Calculate the total number of steps taken per day; calculate and report the mean and median of the total number of steps taken per day, ignore the missing values in the data set.

``` r
bydateActivity<-aggregate(steps~date,activityData,function(x) sum(x,na.rm=T))
meanSteps <- mean(bydateActivity$steps)
medianSteps <- median(bydateActivity$steps)
cat('the mean of the total number of steps taken per day is', meanSteps,'\nthe median of the total number of steps taken per day is',medianSteps)
```

    ## the mean of the total number of steps taken per day is 10766.19 
    ## the median of the total number of steps taken per day is 10765

Make a histogram of the total number of steps taken per day

``` r
hist(bydateActivity$steps,col='green',xlab='Total number of steps taken per day',main='Histogram of total steps taken per day',breaks=5)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

What is the average daily activity pattern?
-------------------------------------------

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
byIntervalActivity<-aggregate(steps~interval,activityData,mean)
plot(byIntervalActivity,type='l',main='Average daily step pattern',xlab='Step count recording interval', ylab='Average step count')
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)

Which 5-minute interval, on average across all the days in the datastep, contains the maximum number of steps?

``` r
maxInterval <- byIntervalActivity$interval[which(byIntervalActivity$steps==max(byIntervalActivity$steps))]
cat('The ',maxInterval,'th 5-minute interval, contains the maximum number of steps.', sep="")
```

    ## The 835th 5-minute interval, contains the maximum number of steps.

Imputing missing values
-----------------------

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
sumNA <- sum(!complete.cases(activityData))
cat('The total number of missing values in the dataset is', sumNA)
```

    ## The total number of missing values in the dataset is 2304

Fill the missing values with the interval mean steps

``` r
naInterval <- data.frame(interval=activityData[is.na(activityData$steps), 'interval'])
naSteps <- merge(naInterval, byIntervalActivity, all.x=T)[, 'steps']
activityData2=activityData
activityData2[is.na(activityData2$steps), 'steps'] <- naSteps
```

Make a histogram of the total number of steps taken each day

``` r
bydateActivity2 <- aggregate(steps~date, activityData2, sum)
hist(bydateActivity2$steps, col='green', xlab='Daily total steps', main='Histogram of daily total steps', breaks=5)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-9-1.png)

Calcualte the mean and median total number of steps taken per day.

``` r
cat('the mean of the total number of steps taken per day is', mean(bydateActivity2$steps),'\nthe median of the total number of steps taken per day is',median(bydateActivity2$steps))
```

    ## the mean of the total number of steps taken per day is 10766.19 
    ## the median of the total number of steps taken per day is 11015

Do these values differ from the estimates from the first part of the assignment?

``` r
par(mfrow=c(1,2))
hist(bydateActivity$steps,col='red',xlab='Daily total steps',main='Daily total steps w/o imputation',breaks=5)
hist(bydateActivity2$steps,col='blue',xlab='Daily total steps',main='Daily total steps with imputation',breaks=5)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
par(mfrow=c(1,1))
cat('Before the data imputation:',
    '\nthe mean total number of steps is',mean(bydateActivity$step),
    '\nthe median totla number of steps is',median(bydateActivity$steps),
'\n\nAfter data imputation:',
    '\nthe mean total number of steps is',mean(bydateActivity2$steps),
    '\nthe median totla number of steps is',median(bydateActivity2$steps))
```

    ## Before the data imputation: 
    ## the mean total number of steps is 10766.19 
    ## the median totla number of steps is 10765 
    ## 
    ## After data imputation: 
    ## the mean total number of steps is 10766.19 
    ## the median totla number of steps is 11015

What is the impact of imputing missing data on the estimates of the total daily number of steps? After data imputation, more small number os steps are generated. Mean value is the same because the NAs are filled with the average of the interval. Median value is higher

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
weekday <- weekdays(activityData2$date)
activityData2$weekday <- NA
activityData2$weekday[weekday %in% c('Saturday','Sunday')] <-'Weekend'
activityData2$weekday[weekday %in% c('Monday','Tuesday','Wednesday','Thursday','Friday')] <-'Weekday'
byIntervalActivity2 <- aggregate(steps~interval+weekday,activityData2,mean)
library(ggplot2)
ggplot(byIntervalActivity2,aes(interval,steps))+geom_line()+facet_grid(~weekday)+theme_bw()+labs(title='Average daily pattern')
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-13-1.png) During weekdays, there are more steps taken in the morning (6am-9am), while during weekends, the number of steps taken are more than those of weekdays and more evenly distributed between 9am to 8pm.
