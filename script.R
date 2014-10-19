
# Unzip de activity.zip file that contains the Dataset to be used
unzip("activity.zip")

# Loads the data in a dataframe called data01
data01 = read.csv("activity.csv")

# Calculate total steps per day
daily_steps=tapply(data01$steps, data01$date, sum) 

# histogram of the total number of steps taken each day
hist(daily_steps)

# Calculate the mean of the total number of steps taken per day
mean(daily_steps, na.rm=TRUE)

# Calculate the median of the total number of steps taken per day
median(daily_steps, na.rm=TRUE)

# Verify the values of Mean and Median provided by the summary function
summary(daily_steps)

# Calculate average steps per interval
interval_steps=tapply(data01$steps, data01$interval, mean, na.rm=TRUE) 

# Plot time series of the 5 minute intervaland 
plot(interval_steps, type="l")

# Identify the interval with the maximum number of steps
interval0=as.numeric(names(which.max(interval_steps)))

# Identify the hour and minutes of the beginning of the interval
hour0<-round(interval0/100)
minutes0<-interval0-(hour0*100)

# Identify the hour and minutes of the end of the interval
interval1=interval0+5
hour1<-round(interval1/100)
minutes1<-interval1-(hour1*100)

print("The interval with more steps as an average is the interval between ")
print(c(hour0, ":", minutes0))


# Count amount of missing values
completeRows=complete.cases(data01)
sum(!completeRows)

# Convert interval_steps to data frame
interval_data=data.frame(interval_steps)
interval_data=cbind(Interval_Names= as.integer(rownames(interval_data)), interval_data)
interval_data$interval_steps<-as.numeric(interval_data$interval_steps)


# Create a new data set as a merge between the original data set and the data set with the average values for each 5 minute interval
data02=merge(data01, interval_data, by.x = "interval", by.y="Interval_Names")

# Replace the NAs with the average 
data02$steps[is.na(data02$steps)]<-data02$interval_steps[is.na(data02$steps)]
data02$interval_steps<-NULL

# Calculate total steps per day
daily_steps2=tapply(data02$steps, data02$date, sum) 

# histogram of the total number of steps taken each day
hist(daily_steps2)

# New Values of Mean and Median
# Calculate the mean of the total number of steps taken per day
mean(daily_steps2, na.rm=TRUE)

# Calculate the median of the total number of steps taken per day
median(daily_steps2, na.rm=TRUE)

# Verify the values of Mean and Median provided by the summary function
summary(daily_steps2)

# Previous values of Mean and Median 
# Calculate the mean of the total number of steps taken per day
mean(daily_steps, na.rm=TRUE)

# Calculate the median of the total number of steps taken per day
median(daily_steps, na.rm=TRUE)

# Verify the values of Mean and Median provided by the summary function
summary(daily_steps)


# Copy the dataset of the previous step and create a factor variable to signal weekdays and weekends
data03<-data02
dim(data03)[1]
for (i in 1:dim(data03)[1]) {
  if ((weekdays(as.POSIXct(data03$date[i]))=="sábado") || (weekdays(as.POSIXct(data03$date[i]))=="domingo"))
      data03$datetype[i]<-"weekend"
    else
      data03$datetype[i]<-"weekday"
  }


# Plot a time series plot of the 5 minute interval for weekends and weekdays and the average steps taken on each interval

# Plotting the combined graph 
library(lattice)
xyplot(data03$steps ~ data03$interval  | data03$datetype, 
      layout=c(1,2), type="l", main="Steps per interval during Weekdays and WeeKends",
      ylab="Steps", xlab="5 minute intervals")

#Plotting a separate plot for weekends
subwe=subset(data03, datetype == "weekend")
subwe_steps=tapply(subwe$steps, subwe$interval, mean, na.rm=TRUE) 
plot(subwe_steps, type="l", main="Average steps per interval during Weekends", ylab="Average steps", xlab="5 minute intervals")

#Plotting a separate plot for weekdays
subWD=subset(data03, datetype == "weekday")
subWD_steps=tapply(subWD$steps, subWD$interval, mean, na.rm=TRUE) 
plot(subWD_steps, type="l", main="Average steps per interval during Weekdays", ylab="Average steps", xlab="5 minute intervals")


#The end
