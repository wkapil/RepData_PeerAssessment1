library("data.table")

### Read Data and formulate

activityData  <- read.csv("activity.csv", header=TRUE)
dataTable        <- data.table(activityData)
dataTable$steps  <- as.integer(dataTable$steps)
dataTable$day    <- as.Date(activityData$date,"%Y-%m-%d")

### Step 1

workingData <- dataTable[ dataTable$steps != 0
                          ,list(
                            sum    = sum(steps,na.rm=TRUE)
                            , mean   = mean(steps,na.rm=TRUE) 
                            , median = as.double( median(steps,na.rm=TRUE) )
                          )
                          ,by=day
                          ]

def.par <- par(no.readonly = TRUE)

layout( matrix( c(1, 1, 2, 3), 2, 2, byrow = TRUE) )

plot( x = workingData$day , type = "h", xlab = "Day", 
      y = workingData$sum , ylab = "Sum Steps", 
      main = "Number of Steps By Day")

plot( x = workingData$day, type = "h", xlab = "Day", 
      y = workingData$mean , ylab = "Mean Steps" , ylim=c(0, 300), 
      main = "Mean Steps By Day")

plot( x = workingData$day, type = "h", xlab = "Day", 
      y = workingData$median , ylab = "Median Steps" , ylim=c(0, 300), 
      main = "Median Steps By Day")

par(def.par)

### Step 2
# Calculate Mean for 5 min interval

workingData2 <- dataTable[ dataTable$steps != 0
                           ,list( 
                             AvgSteps = mean(steps,na.rm=TRUE) )
                           ,by=interval
                           ]

plot( x = workingData2$interval,  type = "h", xlab = "5-Minute Interval", 
      y = workingData2$AvgSteps,  ylab = "Mean Steps",
      main = "Mean Steps by Interval")

print("Maximum Average Number of steps by Interval:" )
print(workingData2[workingData2$AvgSteps == max(workingData2$AvgSteps)])
print(workingData2)

### Step 3

# Find missing values with NA

dataTableNoNas <- data.table(dataTable)

print(c("Missing Values count:", dim( dataTableNoNas[ is.na(dataTableNoNas$steps) ] )[1], "NAs" ))

# Data imputate for NA. 


dataTableNoNas$steps <- ifelse( is.na(dataTableNoNas$steps), 
                                workingData2$AvgSteps[ dataTableNoNas$interval ], 
                                dataTableNoNas$steps 
)



# Histogram for steps take per day with impact of imputing NA values. 

workingData3 <- dataTableNoNas[ dataTableNoNas$steps != 0
                                ,list(
                                  sum    = sum(steps,na.rm=TRUE)
                                  , mean   = mean(steps,na.rm=TRUE) 
                                  , median = as.double( median(steps,na.rm=TRUE) )
                                )
                                ,by=day
                                ]

def.par <- par(no.readonly = TRUE)

layout( matrix( c(1, 1, 2, 3), 2, 2, byrow = TRUE) )

plot( x = workingData3$day , type = "h", xlab = "Day", 
      y = workingData3$sum , ylab = "Sum Steps", 
      main = "Number of Steps by day (No NAs)")

plot( x = workingData3$day, type = "h", xlab = "Day", 
      y = workingData3$mean , ylab = "Mean Steps" , ylim=c(0, 300), 
      main = "Mean Steps by day (No NAs)")

plot( x = workingData3$day, type = "h", xlab = "Day", 
      y = workingData3$median , ylab = "Median Steps" , ylim=c(0, 300), 
      main = "Median Steps by day (No NAs)")

par(def.par)

## Mean values for the new data set

workingData4 <- dataTableNoNas[ dataTableNoNas$steps != 0
                                ,list( 
                                  AvgSteps = mean(steps,na.rm=TRUE) )
                                ,by=interval
                                ]

plot( x = workingData4$interval,  type = "h", xlab = "5-minute Interval", 
      y = workingData4$AvgSteps,  ylab = "Mean Steps", 
      main = "Mean of Steps by Interval for new dataset")

print("Max Avg steps:" )
print(workingData4[workingData4$AvgSteps == max(workingData4$AvgSteps)])
print(workingData4)		

weekDayVector <- weekdays(dataTableNoNas$day)
saturdayVector <- weekDayVector=="Saturday"
sundayVector <- weekDayVector=="Sunday"
weekEndVector <- saturdayVector | sundayVector	
dataTableNoNas <- cbind(dataTableNoNas, weekEndVector)

weekEnddataTable <- dataTableNoNas[dataTableNoNas$weekEndVector == TRUE,]
weekDaydataTable <- dataTableNoNas[dataTableNoNas$weekEndVector == FALSE,]

workingData4 <- weekEnddataTable[ weekEnddataTable$steps != 0
                                  ,list(
                                    sum    = sum(steps,na.rm=TRUE)
                                    , mean   = mean(steps,na.rm=TRUE) 
                                    , median = as.double( median(steps,na.rm=TRUE) )
                                  )
                                  ,by=day
                                  ]


workingData5 <- weekDaydataTable[ weekDaydataTable$steps != 0
                                  ,list(
                                    sum    = sum(steps,na.rm=TRUE)
                                    , mean   = mean(steps,na.rm=TRUE) 
                                    , median = as.double( median(steps,na.rm=TRUE) )
                                  )
                                  ,by=day
                                  ]			

def.par <- par(no.readonly = TRUE)

par(mfrow=c(2,1)) 

plot( x = workingData4$day , type = "l", xlab = "Day", 
      y = workingData4$sum , ylab = "Sum Steps", 
      main = "Steps count on weekends without NA")

plot( x = workingData5$day, type = "l", xlab = "Day", 
      y = workingData5$mean , ylab = "Mean Steps" , ylim=c(0, 300), 
      main = "Mean Steps on weekdays without NA")


par(def.par)