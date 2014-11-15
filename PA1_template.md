# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
  setwd("~/datasciencecoursera/RepData_PeerAssessment1/")

  file    = unz("activity.zip","activity.csv")
  classes = c("numeric","Date","numeric")
  full    = read.csv(file, colClasses = classes)
  df      = full[complete.cases(full),]
```

## What is mean total number of steps taken per day?

Information on daily steps is given by

```r
  dailysteps  = tapply(df$steps,df$date,sum)
  hist(dailysteps)
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
  (meansteps   = mean(dailysteps))
```

```
## [1] 10766
```

```r
  (medsteps    = median(dailysteps))
```

```
## [1] 10765
```

The median number of steps is 10766 and the median is 10765.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
