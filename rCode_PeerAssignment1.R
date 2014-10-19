activityData <- read.table(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")

# convert date to date activityData type
activityData$date <- as.Date(activityData$date) 
activityDataNoNA <- na.omit(activityData) 

# sum steps by date
dailySteps <- rowsum(activityDataNoNA$steps, format(activityDataNoNA$date, '%Y-%m-%d')) 
dailySteps <- activityData.frame(dailySteps) 
names(dailySteps) <- ("steps")
hist(dailySteps$steps, 
     main=" ",
     breaks=10,
     xLable="Daily Steps")
mean(dailySteps$steps)
median(dailySteps$steps) 


library(plyr)
# Calculate average steps for each of 5-minute interval during a 24-hour period
intervalMeanSteps <- ddply(activityDataNoNA,~interval, summarise, mean=mean(steps))

library(ggplot2)
qplot(x=interval, y=mean, activityData = intervalMeanSteps,  geom = "line",
      xLable="5min interval",
      yLable="Step Count",
      main="Average count of steps"
)

intervalMeanSteps[which.max(intervalMeanSteps$mean), ]

library(sqldf)

tableNAVal <- sqldf(' 
                    SELECT d.*            
                    FROM "activityData" as d
                    WHERE d.steps IS NULL 
                    ORDER BY d.date, d.interval ') 

NROW(tableNAVal) 

tableFinal <- sqldf('  
                    SELECT d.*, i.mean
                    FROM "intervalMeanSteps" as i
                    JOIN "activityData" as d
                    ON d.interval = i.interval 
                    ORDER BY d.date, d.interval ') 

tableFinal$steps[is.na(tableFinal$steps)] <- tableFinal$mean[is.na(tableFinal$steps)]

tTotalSteps <- as.integer( sqldf(' 
                                 SELECT sum(steps)  
                                 FROM tableFinal') );

tTotalStepsByDate <- sqldf(' 
                           SELECT date, sum(steps) as "tTotalStepsByDate" 
                           FROM tableFinal GROUP BY date 
                           ORDER BY date') 

dailySteps <- sqldf('   
                    SELECT date, t1_total_steps_by_date as "steps"
                    FROM "tTotalStepsByDate"
                    ORDER BY date') 

hist(dailySteps$steps, 
     main=" ",
     breaks=10,
     xLable="After imputing NA steps taken")

tMeanSteps <- as.integer(tTotalSteps / NROW(tTotalStepsByDate) )
tMeanSteps

tMedianSteps <- median(tTotalStepsByDate$tTotalStepsByDate)
tMedianSteps

tableFinal$weektime <- as.factor(ifelse(weekdays(tableFinal$date) %in% 
                                          c("Saturday","Sunday"),"weekend", "weekday"))

t5 <- sqldf('   
            SELECT interval, avg(steps) as "meanSteps", weektime
            FROM tableFinal
            GROUP BY weektime, interval
            ORDER BY interval ')

library("lattice")
p <- xyplot(meanSteps ~ interval | factor(weektime), activityData=t5, 
            type = 'l',
            main="Steps taken average on weekdays or weekends",
            xLable="5minute interval",
            yLable="Average steps")
print (p)    
