library(plyr)
dev.off()
# 1.) download data,save & format
setwd("C:/Users/T470/Desktop/Coursera/Data Science/Reproducible Research/RepData_PeerAssessment1/data")
#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","dl.zip")

#if(!file.exists("data")) {
 # dir.create("data")
#}
#unzip("dl.zip", exdir = "data")

#setwd("data")

am <- read.csv("activity.csv")
#sum steps by day
am1 <- with(am,tapply(steps,date,sum,na.rm=T))
am2 <- data.frame(date=row.names(am1),steps=as.integer(am1))

# 2.) Plot histogram of steps/day
hist(am2$steps, main = "Histogram of Steps/Day",xlab="Steps/Day",ylab="Frequency", col="hotpink2")

# 3.) Calc mean and med of steps per day
am2_mean <- mean(am2$steps)
print(am2_mean)
am2_med <- median(am2$steps)
print(am2_med)

# 4.) Time series plot of avg steps taken per time interval
am3 <- with(am,tapply(steps,interval,mean,na.rm=T))
am4 <- data.frame(interval=row.names(am3),steps=as.integer(am3))
am4$interval <- as.character(am4$interval)
plot(am4$interval,am4$steps,type="l",col="slateblue3",
     xlab="Interval",ylab="Mean Steps Taken", 
     lwd=3, main="Mean Steps by 5 Min Interval")

# 5.) Find interval with max steps on avg
am4_max <- am4[am4$steps==max(am4$steps),1]
print(am4_max)

# 6.) #Impute missing data

#count missing vals
am_na <- sum(is.na(am))
print(am_na)

#Impute missing step values with intervals median steps
#add avg steps column
am5 <- merge(am,am4,by="interval")

#substitue each NA with the mean steps value for that interval
for (i in 1:nrow(am5)){
  if (is.na(am5[i,2])){
    am5[i,2]<-am5[i,4]
  }
}
#recreate original dataset without NAs
am_xna <- am5[,1:3]
colnames(am_xna) <- c("interval","steps","date")

#re-average steps per day
#sum steps by day
am1_xna <- with(am_xna,tapply(steps,date,sum,na.rm=T))
am2_xna <- data.frame(date=row.names(am1_xna),steps=as.integer(am1_xna))

# 7.) Plot histogram of steps/day
par(mfrow=c(1,2))
hist(am2$steps, main = "Histogram of Steps/Day",xlab="Steps/Day",ylab="Frequency", col="hotpink2",ylim=c(0,35))
hist(am2_xna$steps, main = "Histogram of Steps/Day [No NAs]",xlab="Steps/Day",ylab="Frequency", col="darkslategray3")

#Calc mean and med of steps per day
am2_xna_mean <- mean(am2_xna$steps)
print(am2_xna_mean)
am2_xna_med <- median(am2_xna$steps)
print(am2_xna_med)

# 8.) Plot avg steps per interval by weekday vs weekend
#create factor 
am_xna$day <- as.factor(ifelse(weekdays(as.Date(as.character(am_xna$date))) %in% 
                                 c("Saturday","Sunday"),"weekend","weekday"))

#weekdays
am_w <- subset(am_xna,day=="weekday",select=c("interval","steps"))
am_w1 <- with(am_w,tapply(steps,interval,mean,na.rm=T))
am_w1 <- data.frame(interval=row.names(am_w1),steps=as.integer(am_w1))
am_w1$interval <- as.character(am_w1$interval)

#weekends
am_we <- subset(am_xna,day!="weekday",select=c("interval","steps"))
am_we1 <- with(am_we,tapply(steps,interval,mean,na.rm=T))
am_we1 <- data.frame(interval=row.names(am_we1),steps=as.integer(am_we1))
am_we1$interval <- as.character(am_we1$interval)

plot(am_w1$interval,am_w1$steps,type="l",col="lightblue",
     xlab="Interval",ylab="Mean Steps Taken", 
     lwd=3, main="Weekday v. Weekend: Mean Steps by 5 Min Interval")
lines(am_we1$interval,am_we1$steps,type="l",col="darkorchid1",
     lwd=3)
legend("topleft",c("weekday","weekend"),col=c("lightblue","darkorchid1"),pch = c(15,15))





