# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

* Load the data  

download the data into working directory and read the file
```{r}
setwd("C:\\Users\\Xinhui\\Desktop\\coursera")
activitydata<-read.csv("activity.csv")
```
  

## Mean of total number of steps taken per day

* Make a histogram of the total number of steps taken each day

```{r plot1, fig.height=5,fig.width=5}
activitydata$date<-as.character(activitydata$date)
data<-activitydata[!is.na(activitydata$steps),]
totalsteps<-tapply(data$steps,data$date,sum)
totalsteps1<-data.frame(date=names(totalsteps),steps=totalsteps)
hist(totalsteps1$steps,xlab="Total Steps per day",main="")
```

* Calculate and report the mean and median total number of steps taken per day

```{r}
mean(totalsteps1$steps)
median(totalsteps1$steps)
```


## The average daily activity pattern

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r plot2, fig.height=5,fig.width=5}
averagesteps<-tapply(data$steps,data$interval,mean)
averagesteps1<-data.frame(interval=names(averagesteps),steps=averagesteps)
averagesteps1$interval<-as.numeric(as.character(averagesteps1$interval))
plot(averagesteps1$interval,averagesteps1$steps,type="l",ylab="average steps", xlab="intervals")
```

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The time interval 835 contains the maximum number of steps
```{r}
averagesteps1[averagesteps1$steps==max(averagesteps1$steps),]
```

## Imputing missing values

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r} 
length(activitydata[is.na(activitydata$steps),1])
```

* Devise a strategy for filling in all of the missing values in the dataset. 

Plan to use the mean steps of the five minute interval to substitue NA value

```{r}
meansteps<-tapply(data$steps,data$date,mean)
meansteps1<-data.frame(date=names(meansteps),steps=meansteps)
meansteps1$date<-as.character(meansteps1$date)
newdata<-activitydata
for (i in 1:17568){
  if (is.na(newdata[i,1]))
  {newdata[i,1]<-averagesteps1[averagesteps1$interval==as.numeric(newdata[i,3]),][,2]}
  }
```

* Create a new dataset that is equal to the original dataset but with the missing data filled in
  
Create a new dataset called newdata with the missing data filled in and its structure is as 
follows

```{r}
str(newdata)
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r plot3, fig.height=5,fig.width=5}
newtotalsteps<-tapply(newdata$steps,newdata$date,sum)
newtotalsteps1<-data.frame(date=names(newtotalsteps),steps=newtotalsteps)
hist(newtotalsteps1$steps)
```
```{r}
mean(newtotalsteps1$steps)
median(newtotalsteps1$steps)
```
  
mean is the same with the first part of the assignment but the median is different.

## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day
```{r}
newdata$weekday<-weekdays(as.Date(newdata$date))
for (i in 1:17568){
     if(newdata$weekday[i] %in% c("Saturday","Sunday"))
      {newdata$ifweekday[i]<-"weekend"}
     else
      {newdata$ifweekday[i]<-"weekday"}
     }
newdata$ifweekday<-as.factor(newdata$ifweekday)
str(newdata$ifweekday)
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
```{r}
library(reshape2)
datamelt<-melt(newdata,id=c("ifweekday","interval"),measure.vars=c("steps"))
dcastdata<-dcast(datamelt,ifweekday+interval~variable,mean)
library(ggplot2)
ggplot(dcastdata,aes(interval,steps))+geom_line(stat="identity")+facet_grid(ifweekday~.)
```




