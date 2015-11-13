# Setting the proper directory to the folder "RepData_PeerAssessment1"
setwd("~/RepData_PeerAssessment1")

# Reading the data and converting variable "date" into "Date" format
activity <- read.csv(file="activity.csv", head=TRUE, sep=",")
activity$date <- as.Date(as.character(activity$date, format="%y-%m-%d"))

# Calculating the daily total of the number of steps taken
activity_sum <- aggregate(activity$steps, by=list(Day=activity$date), FUN=sum)
barplot(activity_sum$x, col="darkblue", main="Total Number of Steps by Day", 
        ylab="Number of Steps", 
        xlab="Day between Oct. 1 to Nov. 30, 2012")

# Creating a histogram to show the frequncy of observations by the daily total number of steps
hist(activity_sum$x, col="darkblue", 
     main="Frequency Distribution by Daily Total Number of Steps", 
     xlab="Total Number of Steps", ylab="Frequency")

# Calculating the mean and median of the daily total number of steps
mean <- mean(activity_sum$x, na.rm=TRUE)
median <- median(activity_sum$x, na.rm=TRUE)

# Ploting the Mean Number of Steps in Each Time Interval (aggredated across all days)
activity_interval <- aggregate(activity$steps, by=list(Interval=activity$interval), FUN=mean, na.rm=TRUE)
plot(activity_interval$x, type="l", col="magenta", 
     main="Average Daily Activity Pattern: Mean Steps by Time Interval", 
     xlab="Time Interval (All Days from Oct. 1 to Nov. 30, 2012)", ylab="Mean Number of Steps")

# Finding out what the maximum of the mean number of steps is
max_interval <- max(activity_interval$x, na.rm=TRUE)

# Calculating the number of cases with missing values
incomplete_case <- sum(!complete.cases(activity))
incomplete_case

# Substituting the mean value of steps by intervial for NA
interval_means <- aggregate(activity$steps, by=list(Interval=activity$interval), FUN="mean", na.rm=TRUE)
colnames(interval_means)[colnames(interval_means)=="x"] <- "Interval_Mean"
for(interval in unique(activity$interval)){
  activity$steps[activity$interval==interval & is.na(activity$steps)] <- mean(activity$steps[activity$interval==interval], na.rm=TRUE)
}

# Substituting the mean value of steps by day for NA
daily_means <- aggregate(activity$steps, by=list(Day=activity$date), FUN="mean", na.action=NULL)
colnames(daily_means)[colnames(daily_means)=="x"] <- "Daily_Mean"

ifelse(is.na(activity$steps), daily_means2[activity$date], activity$steps)
incomplete_case2 <- sum(!complete.cases(activity))

# Substitting the mean value of steps for NA
for(day in unique(activity$date)){
  activity$steps[activity$date==day & is.na(activity$steps)] <-mean(activity$steps[activity$date==day], na.rm=TRUE)
}

activity_weekday <- aggregate(activity$steps, by=list(Interval=activity$interval), FUN=mean, na.rm=TRUE)
