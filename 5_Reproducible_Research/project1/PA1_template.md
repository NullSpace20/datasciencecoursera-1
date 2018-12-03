# Reproducible Research: First Programming Assinment

```{r, results='hide', warning=FALSE, message=FALSE}
library(ggplot2)
library(scales)
library(Hmisc)
```

## Loading and preprocessing the data

```{r, results='markup', warning=TRUE, message=TRUE}
if(!file.exists('./data/activity.csv')){
    unzip('./data/activity.zip')
}
activityData <- read.csv('./data/activity.csv')
View(activityData)
```


## What is mean total number of steps taken per day?

```{r}
stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
stepsByDay
```

###  Make a histogram of the total number of steps taken each day

```{r}
aggData <- aggregate(steps ~ date, data = activityData, FUN = sum)
hist(aggData$step, xlab = "Total of Steps", main = "Total of steps per day")
```

### Calculate and report the mean and median total number of steps taken per day

```{r}
stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)
```
* Mean: `r stepsByDayMean`
* Median:  `r stepsByDayMedian`

###  Make a time series plot

```{r}
aggData2 <- aggregate(steps ~ interval, data = activityData, FUN = mean, na.rm = TRUE)
```


```{r}
plot(aggData2$steps ~ aggData2$interval,type = "l", xlab ="Time (5-minute intervals)", ylab="Average of steps taken across all days", main="Average of steps taken for each interval across all days")
```



```{r}
aggData2$interval[which.max(aggData2$steps)]

```


```{r}
sum(is.na(activityData))
```

 ##My strategy is to fill up all the missing values with the mean for that 5-minute          interval.

```{r}
fillingNA <- function(data, naSub)
{
        size <- nrow(data)
        
        for(i in 1:size)
        {
                if(is.na(data$steps[i]))
                {
                        # Replacing the NA for the 5-minute interval mean
                        data$steps[i] = naSub$steps[naSub$interval == data$interval[i]]
                }
        }
        data
}
```



```{r}
newDataSet <- fillingNA(activityData,aggData2)
```

* Number of missing values on the new data set.

```{r}
sum(is.na(newDataSet))
```


* It was created a new subset of the data without missing values. This subset is the result of aggregating the total of steps by day.

````{r}
newDataSetaggData <- aggregate(steps ~ date, data = newDataSet, FUN = sum)
```

* Drawing the histogram of the total number of steps:
```{r}
hist(newDataSetaggData$step, xlab = "Total of Steps", main = "Total of steps per day")
```

* Calculating the mean total number of steps taken per day of the data without missing values
```{r}
mean(newDataSetaggData$step)
```

* Calculating the median total number of steps taken per day of the data without missing values
```{r}
median(newDataSetaggData$step)
```


 The days with only missing values on the original data:
```{r}
testData <- activityData  # Making a copy of the original data
testData[is.na(testData)] <- 0 # Replace NA by zeros
x <- aggregate(steps ~ date, data = testData, FUN = sum) # Agg by data
x[x$step == 0,]    # Days with zero number of steps
```


```{r}
transfWeekDays <- function(oldData)
{
        oldData$weekDays <- weekdays(as.Date(oldData$date))
        size <- nrow(oldData)
        
        for(i in 1:size)
        {
                if(oldData$weekDays[i] == "Saturday" | oldData$weekDays[i] == "Sunday")
                {
                        oldData$weekDays[i] <- "weekend"
                }
                else
                {
                        oldData$weekDays[i] <- "weekday"
                }
        }
        oldData              
}
newDataSet <- transfWeekDays(newDataSet)
```

* It was created a new subset of the data without missing values. This subset is the result of aggregating the total of steps by interval and weekday.
```{r}
aggData3 <- aggregate(steps ~ interval + weekDays, data = newDataSet, FUN = mean, na.rm = TRUE)
```

```{r}
library(lattice)
xyplot(steps ~ interval | weekDays, data = aggData3, layout=c(1,2), type ="l", main ="Average number of steps taken across all days")
```
