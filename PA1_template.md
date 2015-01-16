---
output: html_document
---
Assignment 1
==================================
##Fernando Colugnati


This is the first peer reviewed assignement for the "Reproducible Research" at Coursera, January, 2015.

##Loading and preprocessing the data
Just unziped and loaded data using simple read.cvs function. Also, packages that should be installed are listed. In some tests I could not point the mirror correctely, resulting error.


```r
#Read the file
unzip("activity.zip")
a<-read.csv("activity.csv")

#Install packages
#install.packages("plyr")
#install.packages("Hmisc")
#install.packages("doBy")
```


##What is mean total number of steps taken per day?

Fisrts table presents simple statistics for the whole dataset, and also descritptivestatistics for the total number of steps in each day.


```r
#summarizes dataset and shows statistics for daily steps
summary(a)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
desc <- summary(tapply(a$steps, a$date, sum))
ldesc <- summary(tapply(log(a$steps+1), a$date, sum))
desc
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

```r
ldesc
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   5.724 295.900 354.600 332.800 391.000 505.200       8
```

The mean and median for the total daily steps are 1.077 &times; 10<sup>4</sup> and 1.076 &times; 10<sup>4</sup>, respectively. Also, the log of the same variable plus one is described.

Histograms bellow presents the distribution for the total number of daily steps.


```r
par(mfrow = c(1, 2))
hist(tapply(a$steps, a$date, sum), main="Daily steps distribution", xlab="Total daily steps")
hist(tapply(log(a$steps+1), a$date, sum), main=" ", xlab="Log(total steps + 1)")
```

![plot of chunk ploting steps by interval](figure/ploting steps by interval-1.png) 


## What is the average daily activity pattern?



```r
c <- aggregate(steps ~ interval, data=a, FUN=mean)
attach(c)
```

```
## The following objects are masked from c (position 12):
## 
##     interval, steps
```

```r
par(mfrow = c(1, 1))
plot(c, type="l")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

```r
c <- c[order(-steps),]
```

Across all days, considering the same 5 minutes interval, it is possible to note a activity in the first intervals, maybe during the morning, reaching the maximun average number of steps at  interval 835, reaching arround 206 steps.


##Imputing missing values

I am using functions from plyr and Hmisc packages, that allows to input values for NA's easily, as follows. I used two strategies, considering the interval means and medians for the number of steps.


```r
require(plyr)
require(Hmisc)

a <- ddply(a, "interval", mutate, steps.mean = impute(steps, mean))
a <- ddply(a, "interval", mutate, steps.median = impute(steps, median))


summary(a)
```

```
## 
##  8 values imputed to 1.716981 
## 
## 
##  8 values imputed to 0
```

```
##      steps                date          interval        steps.mean    
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0   Min.   :  0.00  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8   1st Qu.:  0.00  
##  Median :  0.00   2012-10-03:  288   Median :1177.5   Median :  0.00  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5   Mean   : 37.38  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2   3rd Qu.: 27.00  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0   Max.   :806.00  
##  NA's   :2304     (Other)   :15840                                    
##   steps.median
##  Min.   :  0  
##  1st Qu.:  0  
##  Median :  0  
##  Mean   : 33  
##  3rd Qu.:  8  
##  Max.   :806  
## 
```

```r
par(mfrow = c(1, 2))
hist(tapply(a$steps.mean, a$date, sum), main="Daily steps - mean inputed", xlab="Total daily steps")
hist(tapply(a$steps.median, a$date, sum), main="Daily steps - median inputed", xlab="Total daily steps")
```

![plot of chunk input missings](figure/input missings-1.png) 


There are -1 missing values in the original dataset.  Apparently, there are no big impacts regarding mean and median, but the 3rd quartile varies. As can be noted in histograms, the medians increased the number of 0 (zeroes). 

##Are there differences in activity patterns between weekdays and week- ends?

Follows how I created the factor. I have to use a setup so I can have weekdays in English format.


```r
Sys.setlocale("LC_TIME", "C") #I am in Brazil, must do this!
```

```
## [1] "C"
```

```r
a$weekday <- weekdays(as.Date(a$date), abbreviate=TRUE)

a$weekday_f[a$weekday=="Sat" | a$weekday=="Sun"]<-0
a$weekday_f[a$weekday=="Mon" | a$weekday=="Tue" | a$weekday=="Wed" | 
            a$weekday=="Fri" | a$weekday=="Thu"]<-1
a$weekday_f <- factor(a$weekday_f, levels=c(0,1), labels=c("Weekends", "Weekdays"))

table(a$weekday)
```

```
## 
##  Fri  Mon  Sat  Sun  Thu  Tue  Wed 
## 2592 2592 2304 2304 2592 2592 2592
```

```r
table(a$weekday_f)
```

```
## 
## Weekends Weekdays 
##     4608    12960
```

Regarding differences between weekdays and weekends, plots bellow, as well as simple statistical tests, shows that this guys walks a little bit more during weekends. In avarage or in median, he walks 7 steps more during weekends.



```r
a_weekend <- subset(a, weekday_f=="Weekends")
a_weekday <- subset(a, weekday_f=="Weekdays")

par(mfrow = c(2, 1))
plot(aggregate(steps.mean ~ interval, data=a_weekend, FUN=mean), type="l", sub="Weekend")
plot(aggregate(steps.mean ~ interval, data=a_weekday, FUN=mean), type="l", sub="weekday")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
par(mfrow = c(1, 2))
boxplot(a$steps.mean ~ a$weekday,data=a, main="Steps Distribution" , ylab="Number of steps")
boxplot(log(steps.mean+1) ~ weekday,data=a , ylab="Log(Number of steps +1)")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-2.png) 

```r
par(mfrow = c(1, 2))
boxplot(a$steps~a$weekday_f,data=a, main="Steps Distribution" , ylab="Number of steps")
boxplot(log(steps+1)~weekday_f,data=a, ylab="Log(Number of steps +1)")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-3.png) 


```r
library(doBy)
summaryBy(steps.mean + steps.median ~ weekday_f, data = a, 
          FUN = function(x) { c(m = mean(x), s = sd(x), md = median(x)) } )
```

```
##   weekday_f steps.mean.m steps.mean.s steps.mean.md steps.median.m
## 1  Weekends     42.36640     108.2310             0       38.18880
## 2  Weekdays     35.61058     104.2188             0       31.15448
##   steps.median.s steps.median.md
## 1       108.1857               0
## 2       103.8732               0
```

```r
wilcox.test(a_weekend$steps.mean,a_weekday$steps.mean)
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  a_weekend$steps.mean and a_weekday$steps.mean
## W = 31360967, p-value = 3.895e-09
## alternative hypothesis: true location shift is not equal to 0
```

```r
wilcox.test(log(a_weekend$steps.mean+1),log(a_weekday$steps.mean+1))
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  log(a_weekend$steps.mean + 1) and log(a_weekday$steps.mean + 1)
## W = 31360967, p-value = 3.895e-09
## alternative hypothesis: true location shift is not equal to 0
```

```r
t.test(a_weekend$steps.mean,a_weekday$steps.mean)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  a_weekend$steps.mean and a_weekday$steps.mean
## t = 3.6746, df = 7842.405, p-value = 0.0002398
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##   3.151823 10.359817
## sample estimates:
## mean of x mean of y 
##  42.36640  35.61058
```

```r
t.test(log(a_weekend$steps.mean+1),log(a_weekday$steps.mean+1))
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  log(a_weekend$steps.mean + 1) and log(a_weekday$steps.mean + 1)
## t = 5.9072, df = 7752.957, p-value = 3.627e-09
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.1392138 0.2774970
## sample estimates:
## mean of x mean of y 
##  1.527863  1.319508
```

Very funny and usefull assignment! Thank you, I hope I could achieve the expected performance.


