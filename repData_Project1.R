library(data.table)
library(dplyr)
library(ggplot2)


# loading and pre-processing the data
  unzip("activity.zip")
  
  activityData <- read.csv("activity.csv")
# converting date from factors to date
  activityData$date <- as.Date(activityData$date, "%Y-%m-%d")
  
  head(activityData)

# What is mean total number of steps taken per day?
  completeData <- activityData[!is.na(activityData$steps), ]
  head(completeData)
  
  nrow(completeData)

# aggByDate <- tapply(completeData$steps, completeData$date, sum)
  aggByDate <- aggregate(completeData$steps, by = list(completeData$date), FUN = sum)
  colnames(aggByDate) <- c("date", "totalSteps")

# plot a bar plot not sure why they call it as histogram
  barplot(aggByDate$totalSteps, names.arg = aggByDate$date)

# mean
  meanSteps <- mean(aggByDate$totalSteps)

# order the data for finding the mean.
  orderedData <- aggByDate[order(aggByDate$totalSteps),]
  medianSteps <- median(orderedData$totalSteps)




# Q2 : What is the average daily activity pattern?
  aggByInterval <- aggregate(completeData$steps, by = list(completeData$interval), FUN = mean)
  colnames(aggByInterval) <- c("interval", "averageSteps")
  
  plot(aggByInterval$interval, aggByInterval$averageSteps, type = "l", xlab = "interval", ylab = "average_steps")
  
# maximum average  number of steps
  maxSteps <- max(aggByInterval$averageSteps)
  maxStepInterval <- aggByInterval[ aggByInterval$averageSteps == maxSteps, "interval"]




# imputing missing values
# total number of missing values in the dataset.
  missingValues = activityData[is.na(activityData$steps),]
  missingValuesCount = nrow(missingValues)

# filling missing value in any interval by getting the average steps for that interval across all days.
# creating a data.table for faster access.
  avgStepsByInterval <- data.table(aggByInterval)
  setkey(avgStepsByInterval, interval)
  
  # copy the data into a new data frame
  filledActData <- activityData
  
  interpolatingFunction <- function(actData) {
    dataSteps <- as.numeric(actData["steps"])
    dataInterval <- as.numeric(actData["interval"])
    if (is.na(dataSteps)) {
      avgStepsByInterval[interval == dataInterval]$averageSteps
    }
    else {
      dataSteps
    }
  }

  imputedSteps <- apply(filledActData, 1, interpolatingFunction)

  filledActData$steps <- imputedSteps

  # plot for imputedDataSet
  filledAggByDate <- aggregate(filledActData$steps, by = list(filledActData$date), FUN = sum)
  colnames(filledAggByDate) <- c("date", "totalSteps")
  
  barplot(filledAggByDate$totalSteps, names.arg = filledAggByDate$date)
  
  # finding mean
  filledMeanSteps <- mean(filledAggByDate$totalSteps)
  
  # finding median (ordering tthe data for finding the median)
  filledMedianSteps <- median(filledAggByDate[order(filledAggByDate$totalSteps), "totalSteps"])

  # the mean is same as before, but the median has changed.



## Are there differences in activity patterns between weekdays and weekends?
  # get the weekday and weekend as a column in the dataset.
  isWeekend <- grepl("^(Saturday|Sunday)$", weekdays(filledActData$date))
  isWeekend <- as.factor(isWeekend)
  levels(isWeekend) <- c("WeekDay", "WeekEnd")

  filledActData <- mutate(filledActData, isWeekend)

  aggByIntervalAndWeekend <- aggregate(steps ~ interval + isWeekend, data = filledActData, mean)


g <- ggplot(aggByIntervalAndWeekend, aes(x = interval, y = steps))
g <- g + geom_line()
g <- g + facet_grid(isWeekend ~ .)
print(g)