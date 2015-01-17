
setwd("~/classes/ReproData/RepData_PeerAssessment1")

#1. Load the data (i.e. read.csv() )
data_orig <- read.csv("activity.csv")

#2. Process/transform the data (if necessary) into a format suitable for your analysis

#For this part of the assignment, you can ignore the missing values in the dataset.
data_nna <- subset(data_orig, !is.na(data_orig$steps))

#Make a histogram of the total number of steps taken each day
data_nna_by_date <- aggregate(data_nna$steps, by=list(data_nna$date), FUN=sum)
names(data_nna_by_date) <- c("interv", "sum")

png(file="data_nna_by_date.png",width=480,height=480)
barplot(data_nna_by_date$sum, ylab = "Steps", xlab="Date")
dev.off()

#Calculate and report the mean and median total number of steps taken per day
#What is the average daily activity pattern?
mean(data_nna_by_date$sum) #10766.19
median(data_nna_by_date$sum) # 10765 


#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
data_nna_by_interv <- aggregate(data_nna$steps, by=list(data_nna$interval), FUN=sum)
num_days <- length(unique(data_nna$date))
data_nna_by_interv <- cbind(data_nna_by_interv, data_nna_by_interv[,2]/num_days)
names(data_nna_by_interv) <- c("interv", "sum", "day_av")

num_interv <- dim(data_nna_by_interv)[1]
interv_av <- sum(data_nna_by_interv$sum)/num_interv # 1981.278
sum(data_nna_by_interv$day_av)/num_interv * num_days # 1981.278
# sum(data_nna_by_interv[,2]) # 570608

png(file="interv_time_ser.png",width=480,height=480)
plot.ts(data_nna_by_interv[,1], data_nna_by_interv[,2], ylab = "Steps", xlab="Interval", type = "l") 
lines(data_nna_by_interv$interv, cbind(data_nna_by_interv, interv_av)[,4])
dev.off()


#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
data_nna_by_interv[which(data_nna_by_interv[,2]== max(data_nna_by_interv[,2])),1] #835


#Imputing missing values

#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
dim(data_orig)[1] - dim(data_nna)[1] # 2304    0

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
data_imp <- data_orig
for (i in 1:dim(data)[1]) {
    if (is.na(data[i,1])) {
        step_ndx <- which(data_nna_by_interv$interv == data$interv[i])
        data_imp$steps[i] <- data_nna_by_interv$day_av[step_ndx]
        #print(c(step_ndx, data_nna_by_interv$day_av[step_ndx]))
    }
}


#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

data_imp_by_date <- aggregate(data_imp$steps, by=list(data_imp$date), FUN=sum)
sum(data_imp_by_date[,2]) # 656737.5 (vs sum(data_nna_by_date[,2]) = 570608) (good, the sum should be > than the no na data set)

data_imp_by_interv <- aggregate(data_imp$steps, by=list(data_imp$interval), FUN=sum)
sum(data_imp_by_interv[,2]) # 656737.5


png(file="data_imp_by_date.png",width=480,height=480)
barplot(data_imp_by_date[,2], ylab = "Steps", xlab="Date")
dev.off()

mean(data_imp_by_date[,2]) # 10766.19
median(data_imp_by_date[,2]) # 10766.19


#Are there differences in activity patterns between weekdays and weekends?

#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
data_imp <- cbind(data_imp, as.factor((weekdays(strptime(data_imp[,2], format = "%Y-%m-%d")) == "Saturday" | weekdays(strptime(data_imp[,2], format = "%Y-%m-%d")) == "Sunday" )))
colnames(data_imp) <- c("steps", "date", "interval", "weekend")

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
data_tmp <- subset(data_imp, data_imp$weekend == FALSE)
data_imp_by_interv <- cbind(data_imp_by_interv, aggregate(data_tmp$steps, by=list(data_tmp$interval), FUN=sum)[2])

data_tmp <- subset(data_imp, data_imp$weekend == TRUE)
data_imp_by_interv <- cbind(data_imp_by_interv, aggregate(data_tmp$steps, by=list(data_tmp$interval), FUN=sum)[2])

names(data_imp_by_interv) <- c("interval", "steps", "wkday", "wkend")
sum(data_imp_by_interv$wkday)+sum(data_imp_by_interv$wkend) # 656737.5

library(lattice)
png(file="plot.png",width=480,height=480)
par(mfrow=c(2,1))  
plot(data_imp_by_interv$interval, data_imp_by_interv$wkday, type="l", col="darkgrey",
xlab="Interval", ylab="Steps")
plot(data_imp_by_interv$interval, data_imp_by_interv$wkend, type="l", col = "brown", xlab="Interval", ylab="Steps" ) 
dev.off()




