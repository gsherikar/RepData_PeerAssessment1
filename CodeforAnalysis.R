# extract the supplied file from compressed format into the current folder
unzip("activity.zip")

# read the csv file from current directory
fileName<- "activity.csv"
activityData <- read.csv(fileName,header=TRUE,na.strings="NA")

# Summarize count of steps for each day
stepCountPerDay<-aggregate(steps~date, data=activityData, sum)

# Compute the median number of steps for each day where number of steps for an interval is non-zero
medianOfStepsEachDay<-round(median(stepCountPerDay$steps))

# Compute the average number of steps for each day where number of steps for an interval is non-zero
averageStepsEachDay<-round(mean(stepCountPerDay$steps))

# Compute average steps per day over all the days
DailyAvgStepCount <- abs(mean(stepCountPerDay$steps))

# Prepare histogram
histgm <- hist(stepCountPerDay$steps, plot=FALSE)

# Determine total number of breaks in the histogram
breakCount <- abs(histgm$breaks)

# Determine the max number of counts to be used to extend the y-axis
maxCount <- max(histgm$counts)

# determine the 25th and 75th percentiles just to color
quantiles<-quantile(histgm$breaks, c(.25,.75))

# draw the plot and color the bars as per the percentiles computed earlier
plot(histgm, ylim=c(0,maxCount*1.10), labels=TRUE,main="Average steps per day",xlab="No of Steps",
     col=ifelse(histgm$breaks < quantiles[1], "red", 
                ifelse(histgm$breaks > quantiles[2], "green","yellow")))

# Draw a reference line for average number of steps per day     
abline(v = DailyAvgStepCount, col = "blue", lwd = 1)

library(lubridate)
library(stringr)
AvgStepsPerInterval<-aggregate(steps~interval, data=activityData, mean)

AvgStepsPerInterval$interval <- strptime(sprintf("%04d",AvgStepsPerInterval$interval),"%H%M")


x<-AvgStepsPerInterval$interval
y<-AvgStepsPerInterval$steps


plot(x, y, type = "l", ylab="Average Number of steps", xlab = "Time", xaxt = "n")
tseq <- seq(from = round(x[1], "hours"),
            to = x[1] + ceiling(difftime(tail(x, 1), head(x, 1), 
                                         units = "hours")),
            by = "30 min")

axis.POSIXct(side = 1, x = x, at = tseq,
             labels = format(tseq, format = "%H:%M"), las = 2,cex.axis=.60,font=2)


plot(AvgStepsPerInterval, type="l")
strftime(AvgStepsPerInterval[AvgStepsPerInterval$steps == max(AvgStepsPerInterval$steps),1],"%H:%M")

