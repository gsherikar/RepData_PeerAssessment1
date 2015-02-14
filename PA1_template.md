# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# extract the supplied file from compressed format into the current folder
#unzip("activity.zip")

# read the csv file from current directory
fileName<- "activity.csv"
activityData <- read.csv(fileName,header=TRUE,na.strings="NA")
```

## Mean total number of steps taken per day

```r
# Compute and summarize by date the total number of steps
stepCountPerDay<-aggregate(steps~date, data=activityData, sum)

# Compute the median of total steps for each day
medianOfStepsEachDay<-round(median(stepCountPerDay$steps))

# Compute the average number of steps for each day 
averageStepsEachDay<-round(mean(stepCountPerDay$steps))

# Prepare histogram
histgm <- hist(stepCountPerDay$steps, plot=FALSE)

# Determine total number of breaks in the histogram
breakCount <- abs(histgm$breaks)

# Determine the max number of counts (the frequency) - to be used for extending the y-axis
maxCount <- max(histgm$counts)

# determine the 25th and 75th percentiles just to color the histogram accodingly
quantiles<-quantile(histgm$breaks, c(.25,.75))

# draw the plot and color the bars as per the percentiles computed earlier
plot(histgm, ylim=c(0,maxCount*1.10), labels=TRUE,main="Average steps per day",xlab="No of Steps",
     col=ifelse(histgm$breaks < quantiles[1], "red", 
                ifelse(histgm$breaks > quantiles[2], "green","yellow")))

# Draw a reference line to show the average number of steps per day     
abline(v = averageStepsEachDay, col = "blue", lwd = 3)
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 
With an average count of steps daily being __10766__, one can see that number of observations with step count higher than average is greater than those below average. This is a good sign! Interestingly the median value of total steps over all the days is __10765__ is a very close to the average number of steps each day!


## What is the average daily activity pattern?

```r
######################

# Load required libraries
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.1.2
```

```r
library(stringr)

# read the csv file from current directory
fileName<- "activity.csv"
activityData <- read.csv(fileName,header=TRUE,na.strings="NA")


# Summarize by interval the average number of steps during that interval over all the days
AvgStepsPerInterval<-aggregate(steps~interval, data=activityData, mean)

# Convert the interval into a POSIX Date Time format
AvgStepsPerInterval$interval <- strptime(sprintf("%04d",AvgStepsPerInterval$interval),"%H%M")

# Prepare x and y axis values
xData<-AvgStepsPerInterval$interval
yData<-AvgStepsPerInterval$steps

# Plot the data without x axis lables
plot(xData, yData, type = "l", ylab="Average Number of steps", xlab = "Time of day", xaxt = "n")

# Prepare labels for x-axis to display the ticks at 30 min intervals
# Credits: Stack Overflow for ideas on how to prepare the labels
#          http://stackoverflow.com/questions/6592627/r-plot-specify-number-of-time-tickmarks-time-date-equivalent-to-pretty
#
tseq <- seq(from = round(xData[1], "hours"),
            to = xData[1] + ceiling(difftime(tail(xData, 1), head(xData, 1), 
                                         units = "hours")),
            by = "30 min")

# draw the x axis labels in HH:MM format
axis.POSIXct(side = 1, x = xData, at = tseq,
             labels = format(tseq, format = "%H:%M"), las = 2,cex.axis=.60,font=2)

maxAverageSteps<-round(max(AvgStepsPerInterval$steps))

timeIntervalForMaxSteps <- strftime(AvgStepsPerInterval[AvgStepsPerInterval$steps == max(AvgStepsPerInterval$steps),1],"%r")

abline(v = AvgStepsPerInterval[AvgStepsPerInterval$steps == max(AvgStepsPerInterval$steps),1], col = "blue", lwd = 1)
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The __08:35:00 AM__ interval shows as having the highest average number of steps, __206__ across all days

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
