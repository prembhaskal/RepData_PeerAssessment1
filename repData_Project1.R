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
aggByDate <- aggregate(completeData$steps, by = list("date" = completeData$date), FUN = sum)
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

