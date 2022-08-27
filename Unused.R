#Used this to generate a new column with the timestamp for each interval,
#But in the end it wasn't necessary.
#I wanted to keep it either way, for future reference.

activityData$interval <- str_pad(activityData$interval, 4, pad = "0")
activityData$timestamp <- 
ymd_hm(paste(activityData$date, activityData$interval))

#And a lattice plot
xyplot(avgSteps ~ interval | dayType, 
       data = avgByIntervalAndDayType, 
       type = "l", 
       lwd = 2,
       layout = c(1, 2), 
       xlab = "Interval", 
       ylab = "Steps",
       main = "Avg. Steps (weekday x weekend)")