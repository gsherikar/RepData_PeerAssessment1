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

######################

# Load required libraries
library(lubridate)
library(stringr)

# Summarize by interval the average number of steps during that interval over all the days
AvgStepsPerInterval<-aggregate(steps~interval, data=activityData, mean)

# Convert the interval into a POSIX Date Time format
AvgStepsPerInterval$interval <- strptime(sprintf("%04d",AvgStepsPerInterval$interval),"%H%M")

# Prepare x and y axis values
x<-AvgStepsPerInterval$interval
y<-AvgStepsPerInterval$steps

# Plot the data without x axis lables
plot(x, y, type = "l", ylab="Average Number of steps", xlab = "Time", xaxt = "n")

# Prepare labels for x-axis to display the ticks at 30 min intervals
tseq <- seq(from = round(x[1], "hours"),
            to = x[1] + ceiling(difftime(tail(x, 1), head(x, 1), 
                                         units = "hours")),
            by = "30 min")

# draw the x axis labels in HH:MM format
axis.POSIXct(side = 1, x = x, at = tseq,
             labels = format(tseq, format = "%H:%M"), las = 2,cex.axis=.60,font=2)

maxAverageSteps<-round(max(AvgStepsPerInterval$steps))

timeIntervalForMaxSteps <- strftime(AvgStepsPerInterval[AvgStepsPerInterval$steps == max(AvgStepsPerInterval$steps),1],"%r")

abline(v = AvgStepsPerInterval[AvgStepsPerInterval$steps == max(AvgStepsPerInterval$steps),1], col = "blue", lwd = 1)

## Question 3

missingData<-activityData[is.na(activityData$steps) | is.na(activityData$date) | is.na(activityData$interval) ,]
a<-nrow(activityData[is.na(activityData$steps) | is.na(activityData$date) | is.na(activityData$interval) ,])


library(plyr)
library(dplyr)
# introduce a column which will identify the weekday
activityData<-mutate(activityData,weekday=strftime(date,"%A"))

# Compute average number of steps for a given weekday and time interval over all the dates
# So essentially we are computing average number of steps at a given time interval on all Mondays, Tuesdays etc..

avgStepsWeekdayInterval<-aggregate(steps~strftime(date,"%A")+interval,data=activityData, mean)

# Let's name the column so it is consistent with the one in activityData dataframe
colnames(avgStepsWeekdayInterval)[1]<-"weekday"

# Round the average value to eliminate decimal positions
avgStepsWeekdayInterval$steps<-round(avgStepsWeekdayInterval$steps)

# This function performs following
# for a given weekday and interval it returns the average no of steps for that weekday and interval from 
# the previously prepared data frame called avgStepsWeekdayInterval.
getAvgStepsForWeekdayInterval <- function(weekday, interval, ... ){
        avgSteps<-avgStepsWeekdayInterval[avgStepsWeekdayInterval$weekday==weekday & avgStepsWeekdayInterval$interval==interval,3]
        return(avgSteps)
}

# Prepare 2nd data frame from original so we can use that to fill in missing values
completeData<-activityData

# In this data set now go through every row and where value of steps is missing, fill it using the
# function prepared earlier

for (i in 1:nrow(completeData)){
        #fill no of steps only if it is missing, i.e. NA
        if (is.na(completeData[i,1])) {
                completeData[i,1]<-getAvgStepsForWeekdayInterval(completeData[i,4],completeData[i,3])
        }
        
}

# At this point we should have zero missing values
nrow(completeData[is.na(completeData$steps) | is.na(completeData$date) | is.na(completeData$interval) ,])


# Summarize count of steps for each day
stepCountPerDay<-aggregate(steps~date, data=completeData, sum)

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
plot(histgm, ylim=c(0,maxCount*1.25), labels=TRUE,main="Average steps per day",xlab="No of Steps",
     col=ifelse(histgm$breaks < quantiles[1], "red", 
                ifelse(histgm$breaks > quantiles[2], "green","yellow")))

# Draw a reference line for average number of steps per day     
abline(v = DailyAvgStepCount, col = "blue", lwd = 1)


completeData<-mutate(completeData,daytype=ifelse(weekday=="Saturday"|weekday=="Sunday","Weekend","Weekday"))

# kernel density plots by factor level
library(lattice)
attach(completeData)
densityplot(steps~interval|daytype,
            main="Density Plot by Number of Cylinders",type="l",
            xlab="Number of Steps")



