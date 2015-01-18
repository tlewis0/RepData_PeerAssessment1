
setwd("~/classes/ReproData/proj1")

#1. Load the data (i.e. read.csv() )
data <- read.csv("activity.csv")

#2. Process/transform the data (if necessary) into a format suitable for your analysis

#For this part of the assignment, you can ignore the missing values in the dataset.
data1 <- subset(data, !is.na(data$steps))
unique(data1$interval) # 0,5,10, ... 2355
length(unique(data1$steps)) # 617
length(unique(data1$date)) # 53

#Make a histogram of the total number of steps taken each day
library(ggplot2)
png(file="steps_by_date.png",width=480,height=480)
qplot(date, steps, data = data1, geom="histogram",stat="identity")
dev.off()


#check above: 
#date1 <- subset(data1, levels(data1$date) == "2012-10-01")
date_str <- unique(data1$date)
date_sums <- NULL
for (date_var in date_str) {
    print(date_var)
    date_sub <- subset(data1,data1[,2]==date_var)
    print(sum(date_sub[,1]))
    date_sums <- c(date_sums, sum(date_sub[,1]))
}
date_tally <- as.character(date_str)
date_tally <- data.frame(date_tally)
date_tally <- cbind(date_tally,as.numeric(date_sums))
names(date_tally) <- c("date","steps")

png(file="steps_check.png",width=480,height=480)
barplot(date_tally$steps, ylab = "Steps", xlab="Date")
dev.off()

#Calculate and report the mean and median total number of steps taken per day
#What is the average daily activity pattern?
mn <- mean(date_tally$steps)  # 10766.19
med <- median(date_tally$steps)  # 10765

step_agg <- aggregate(data1$steps, by=list(data1$date), FUN=sum)
mean(step_agg[,2]) #10766.19
median(step_agg[,2])  # 10765

png(file="steps_check.png",width=480,height=480)
barplot(step_agg[,2], ylab = "Steps", xlab="Date")
dev.off()

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
png(file="steps_by_int.png",width=480,height=480)
qplot(interval, steps, data = data1, geom="histogram",stat="identity")
dev.off()


#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


I am also using aggregate, and didn't observe any differences in the mean/median when toggling na.rm on/off. 


#Imputing missing values

#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


#Create a new dataset that is equal to the original dataset but with the missing data filled in.


#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


#Are there differences in activity patterns between weekdays and weekends?


#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
weekdays(strptime(data1[,2], format = "%Y-%m-%d"))

#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



Fitted = c(data_imp_by_interv$wkday,data_imp_by_interv$wkend)

library(ggplot2)
ggplot(data_imp_by_interv, aes(x = interval, y = Fitted)) +
     geom_line(mapping = aes(y = data_imp_by_interv$wkday), lty = "solid") +
     geom_line(mapping = aes(y = data_imp_by_interv$wkend), lty = "dashed") +
     facet_wrap( ~ Fitted)
     
     geom_line(mapping = aes(y = Signif), lwd = 1.3, colour = "red") +
     facet_wrap( ~ Site)

ggplot(data_imp_by_interv, aes(x = interval, y = Fitted, group = Site)) +

library(lattice)
plot(Yr_frac,RSS,  type = "S", col = "darkgrey",

png(file="plot.png",width=480,height=480)
par(mfrow=c(2,1))  
plot(data_imp_by_interv$interval, data_imp_by_interv_wkday$steps,  type="l", col="darkgrey")
legend("topright", lty=c(2,1), col = c("chocolate", "16"), legend = c("wday", "weekend"), seg.len=3)
points(data_imp_by_interv_wkday$interval, data_imp_by_interv_wkday$steps, col = 16)

dev.off()

with(data_imp_by_interv_wkday, lines(steps ~ interval, type="l", col="chocolate"))  

with(data_imp_by_interv_wkend, lines(steps ~ interval, type="l", col="16" ))  

legend("topright", lty=c(1,1), col = c("chocolate", "16"), legend = c("weekday", "weekend"), seg.len=3)
dev.off()

png(file="plot.png",width=480,height=480)
xyplot(data_imp_by_interv_wkend$interval , groups=fac2, data=data_imp_by_interv_wkend, type='b', auto.key=list(space="top", title='fac2', cex.title=1.1))
dev.off()








