---
title: "RepData_ActivityMonitoring"
output:
  html_document:
    keep_md: yes
---



```r
# load the libraries
  library(data.table)
  library(dplyr)
  library(ggplot2)
```


### Loading and pre-processing the activity monitoring date.


```r
  unzip("activity.zip")
  
  activityData <- read.csv("activity.csv")
  # converting date from factors to date
  activityData$date <- as.Date(activityData$date, "%Y-%m-%d")
```

### plot for total number of steps per day

```r
  # ignoring the data with NAs
  completeData <- activityData[!is.na(activityData$steps), ]
  
  aggByDate <- aggregate(completeData$steps, by = list(completeData$date), FUN = sum)
  colnames(aggByDate) <- c("date", "totalSteps")
  
  # plot a bar plot not sure why questions refers to a histogram
  barplot(aggByDate$totalSteps, names.arg = aggByDate$date, ylab = "total steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
  # mean
  meanSteps <- mean(aggByDate$totalSteps)
  
  # order the data for finding the mean.
  orderedData <- aggByDate[order(aggByDate$totalSteps),]
  medianSteps <- median(orderedData$totalSteps)
```
The mean number of steps taken per day are 1.0766189 &times; 10<sup>4</sup>.
The median number of steps are 10765.

### Q2 : What is the average daily activity pattern?

```r
  aggByInterval <- aggregate(completeData$steps, by = list(completeData$interval), FUN = mean)
  colnames(aggByInterval) <- c("interval", "averageSteps")
  
  plot(aggByInterval$interval, aggByInterval$averageSteps, type = "l", xlab = "interval", ylab = "average_steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
  # maximum average  number of steps
  maxSteps <- max(aggByInterval$averageSteps)
  maxStepInterval <- aggByInterval[ aggByInterval$averageSteps == maxSteps, "interval"]
```

The interval 835 has the highest average steps across all days.
