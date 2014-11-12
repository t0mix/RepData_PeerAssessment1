# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. Load the data (i.e. read.csv())


```r
# create a temporary directory
td <- tempdir()
# create the placeholder file
tf <- tempfile(tmpdir=td, fileext=".zip")
# download into the placeholder file
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", tf, mode="wb")
# get the name of the first file in the zip archive
fname <- unzip(tf, list=TRUE)$Name[1]
# unzip the file to the temporary directory
unzip(tf, files=fname, exdir=td, overwrite=TRUE)
# fpath is the full path to the extracted file
fpath = file.path(td, fname)
# stringsAsFactors=TRUE will screw up conversion to numeric!
data <- read.csv(fpath, header=TRUE, row.names=NULL, 
             stringsAsFactors=FALSE, na.strings = "NA")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
# convert second column as Date
data[,2] <- as.Date(data[,2])
# remove rows where steps are NA and store in nondata data.frame
nonadata <- data[!is.na(data[,1]),]
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
# load data.table library
hist(data$steps)
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
# load data.table library
library(data.table)
# create data.table from nonadata
nonadatatable <- data.table(nonadata)
# show mean and median by date from nonadatatable
nonadatatable[,list(Mean=mean(steps),Median=median(steps))]
```

```
##       Mean Median
## 1: 37.3826      0
```

```r
# show mean and median by date from nonadatatable, excluding interval with no 
# steps (e.g, 0 steps)
nonadatatable[nonadatatable$steps>0,list(Mean=mean(steps),Median=median(steps))]
```

```
##        Mean Median
## 1: 134.2607     56
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# 5-minute interval average number of steps taken across all days
plotdata <- nonadatatable[,list(AverageSteps=mean(steps)),by=interval]
# creating time serie plot
plot(plotdata[,interval],plotdata[,AverageSteps], xlab = "interval", ylab = "average number of steps taken", col = "black", type="l")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# show 5-minute interval containing the maximum number of steps
plotdata[which.max(AverageSteps)]
```

```
##    interval AverageSteps
## 1:      835     206.1698
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
# count missing values
sum(is.na(data[,1]))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# 5-minute interval average number of steps taken across all days
missingdata <- nonadatatable[,list(AverageSteps=mean(steps)),by=interval]
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# copy data in new data.frame
adjusteddata <- data
# use mean for respective 5-minute interval for each NA values
adjusteddata[is.na(adjusteddata[,1]),1] <- missingdata[interval==adjusteddata[is.na(adjusteddata[,1]),3]][,AverageSteps]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# load data.table library
hist(adjusteddata$steps)
```

![](./PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
# create data.table from adjusteddata
adjusteddatatable <- data.table(adjusteddata)
# show mean and median by date from adjusteddatatable
adjusteddatatable[,list(Mean=mean(steps),Median=median(steps))]
```

```
##       Mean Median
## 1: 37.3826      0
```

```r
# show mean and median by date from adjusteddatatable, excluding interval with no 
# steps (e.g, 0 steps)
adjusteddatatable[adjusteddatatable$steps>0,list(Mean=mean(steps),Median=median(steps))]
```

```
##        Mean   Median
## 1: 102.5832 45.33962
```

```r
# compare to mean and median by date from nonadatatable, excluding interval with
# no steps (e.g, 0 steps)
nonadatatable[nonadatatable$steps>0,list(Mean=mean(steps),Median=median(steps))]
```

```
##        Mean Median
## 1: 134.2607     56
```

```r
# Showing the delta between mean and median by date from adjusteddatatable, and
# mean and median by date from nonadatatable
adjusteddatatable[adjusteddatatable$steps>0,list(Mean=mean(steps),Median=median(steps))] - nonadatatable[nonadatatable$steps>0,list(Mean=mean(steps),Median=median(steps))]
```

```
##         Mean    Median
## 1: -31.67753 -10.66038
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
# copy data in new data.frame
adjusteddata <- data
# use mean for respective 5-minute interval for each NA values
adjusteddata[is.na(adjusteddata[,1]),1] <- missingdata[interval==adjusteddata[is.na(adjusteddata[,1]),3]][,AverageSteps]
# adding a column named wd to adjusteddata which indicates wheter it's a weekday or weekend
adjusteddata[,4] <- "weekday"
adjusteddata[weekdays(adjusteddata[,2]) == "Saturday" | weekdays(adjusteddata[,2]) == "Sunday",4] <- "weekend"
names(adjusteddata)[4] <- "wd"
# setting wd as factor
adjusteddata[,4] <- as.factor(adjusteddata[,4])
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
# create data.table from adjusteddata
adjusteddatatable <- data.table(adjusteddata)
# 5-minute interval average number of steps taken across all days
plotdata <- adjusteddatatable[,list(AverageSteps=mean(steps)),by=list(interval,wd)]
# creating 2 time serie plots, one for weekends and one for weekdays
library(lattice)
xyplot(AverageSteps ~ interval | wd, data=plotdata, type='l')
```

![](./PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
