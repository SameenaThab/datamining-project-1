---
title: "P1_template"
author: "Sameena Thabassum"
date: "January 18, 2017"
output:
  html_document:
    css: min.css
    highlight: textmate
    theme: null
  pdf_document: default
---

Instructions: This is the template you may use to type up your responses to the exercises and the on your own questions. Within RStudeo, to produce a document that you can print out and turn in just click on Knit HTML above. All you need to do to complete the lab is to type up your BRIEF answers and the R code (when necessary) in the spaces provided below. 

To use the min.css formatting, leave the css file mentioned in the YAML section of the document. Otherwise, I suggest a simple Bootstrap theme. 


# Question 1
Classification is to prdict what class the variable belows to, whereas Regression predicts a continuous value.
For example given a persons family history of diabetes, health condition,food habits we can predict if that person may have diabates  in future(categorical) , this is classfication.
Given a computers configuration and brand , one can predict its price(price), this is regression.

Regression and classification are similar as both of them predict a value depending on the other variable values

# Q2

The census data can be downloaded from the [UCI ML site](http://archive.ics.uci.edu/ml/datasets/Census+Income) and loaded using the `read.csv` command.  Alternatively, it can be read directly from the UCI site (follow the example used in the Exploratory Analysis examples).


```r
adult <- read.csv("adult.data.txt",header=F, na.strings="?")
```

Inline r code can be inserted as 39.  
This can also be used in a table:

|       |  Var 1              |  Var 3              | 
|-------|---------------------|---------------------|
| Mean  | 38.5816468 | 1.8977837 &times; 10<sup>5</sup> | 
| Max   | 90  | 1484705  | 

## Q2a

Information on each variable:

* __Age__: Age is the age of an individual
as reported by that person for the 1990 census; the value is continuous and reported in integer units of
years.
* __Workclass__:Workclass is the workclass of an individual
as reported by that person for the 1990 census;the value is reported in categoricaly {" ?", " Federal-gov"," Local-gov"," Never-worked"," Private"," Self-emp-inc"," Self-emp-not-inc"," State-gov"," Without-pay"}

```r
wc<-levels(adult$V2)
wc
```

```
## [1] " ?"                " Federal-gov"      " Local-gov"       
## [4] " Never-worked"     " Private"          " Self-emp-inc"    
## [7] " Self-emp-not-inc" " State-gov"        " Without-pay"
```
*__finalsamplingweight__:Shows the final sampling weight of an individual in the 1990 census.THe value is continuous and reported in integer.
*__education__:Shows the education of individual as reported by that person in the 1990 census. This is Categoricaly represented.
*__Educational Number__: Shows Educational number for the individual as reported by that person in for the 1990 census.Represented in integers.
*__Martial-status__:Tells about the martial status of the individual as reported by that person in 1990 census.

```r
ms<-levels(adult$V6)
ms
```

```
## [1] " Divorced"              " Married-AF-spouse"    
## [3] " Married-civ-spouse"    " Married-spouse-absent"
## [5] " Never-married"         " Separated"            
## [7] " Widowed"
```
*__Occupation__:Occupation of the individual as reported by that person in the census 1990.Represented categoricaly.'?' represents missing value.

```r
occ<-levels(adult$V7)
occ
```

```
##  [1] " ?"                 " Adm-clerical"      " Armed-Forces"     
##  [4] " Craft-repair"      " Exec-managerial"   " Farming-fishing"  
##  [7] " Handlers-cleaners" " Machine-op-inspct" " Other-service"    
## [10] " Priv-house-serv"   " Prof-specialty"    " Protective-serv"  
## [13] " Sales"             " Tech-support"      " Transport-moving"
```
*__Relationship__: Relationship of the person as reported by the person in 1990 census.Represented categoricaly

```r
rel<-levels(adult$V8)
rel
```

```
## [1] " Husband"        " Not-in-family"  " Other-relative" " Own-child"     
## [5] " Unmarried"      " Wife"
```
*__Race__: Race of the person.Represented categoricaly

```r
race<-levels(adult$V9)
race
```

```
## [1] " Amer-Indian-Eskimo" " Asian-Pac-Islander" " Black"             
## [4] " Other"              " White"
```
*__sex__: Sex of the person.Represented categoricaly in {male,female}

```r
sex<-levels(adult$V10)
sex
```

```
## [1] " Female" " Male"
```
*__Capitalgain__: Shows Capital gain of the person.Represented in integers.
*__Capitalloss__: Shows Capital loss of the person.Represented in integers.
*__hours-per-week__:Shows how many hours the individual spends working per week.represented in integers
*__native-country__:Tells about that native country of the individual.It is represented categoricaly.'?' represents missing value.

```r
nc<-levels(adult$V14)
nc
```

```
##  [1] " ?"                          " Cambodia"                  
##  [3] " Canada"                     " China"                     
##  [5] " Columbia"                   " Cuba"                      
##  [7] " Dominican-Republic"         " Ecuador"                   
##  [9] " El-Salvador"                " England"                   
## [11] " France"                     " Germany"                   
## [13] " Greece"                     " Guatemala"                 
## [15] " Haiti"                      " Holand-Netherlands"        
## [17] " Honduras"                   " Hong"                      
## [19] " Hungary"                    " India"                     
## [21] " Iran"                       " Ireland"                   
## [23] " Italy"                      " Jamaica"                   
## [25] " Japan"                      " Laos"                      
## [27] " Mexico"                     " Nicaragua"                 
## [29] " Outlying-US(Guam-USVI-etc)" " Peru"                      
## [31] " Philippines"                " Poland"                    
## [33] " Portugal"                   " Puerto-Rico"               
## [35] " Scotland"                   " South"                     
## [37] " Taiwan"                     " Thailand"                  
## [39] " Trinadad&Tobago"            " United-States"             
## [41] " Vietnam"                    " Yugoslavia"
```
##Q2b

```r
missed<-function(x)
{
    total<-length(x)
    t<-table(x)
    td<-as.data.frame(t)
    a<-td[which(td[1,]==" ?"),2]
    if(length(a)==0)
    {
        print("NO '?' values")
    }
    else
    {
   per<-a/total
   per<-per*100
   print(per)
    }

}
```

Variable 1 Age

```r
missed(adult$V1)
```

```
## [1] "NO '?' values"
```
Variable 2 Workclass

```r
missed(adult$V2)
```

```
## [1] 5.638647
```
Variable 3 finalsamplingweight

```r
missed(adult$V3)
```

```
## [1] "NO '?' values"
```
Variable 4 education

```r
missed(adult$V4)
```

```
## [1] "NO '?' values"
```
Variable 5 Educational Number

```r
missed(adult$V5)
```

```
## [1] "NO '?' values"
```
Variable 6 Martial-status

```r
missed(adult$V6)
```

```
## [1] "NO '?' values"
```
Variable 7 Occupation

```r
missed(adult$V7)
```

```
## [1] 5.660146
```
Variable 8 Relationship

```r
missed(adult$V8)
```

```
## [1] "NO '?' values"
```
Variable 9 Race

```r
missed(adult$V9)
```

```
## [1] "NO '?' values"
```
Variable 10 sex

```r
missed(adult$V10)
```

```
## [1] "NO '?' values"
```
Variable 11 Capitalgain

```r
missed(adult$V11)
```

```
## [1] "NO '?' values"
```
Variable 12 Capitalloss

```r
missed(adult$V12)
```

```
## [1] "NO '?' values"
```
Variable 13 hours per week

```r
missed(adult$V13)
```

```
## [1] "NO '?' values"
```
Variable 14 native country

```r
missed(adult$V14)
```

```
## [1] 1.790486
```
Variable 15 classification label

```r
missed(adult$V15)
```

```
## [1] "NO '?' values"
```
##Q2c
V1 age: numerical.
V2 workclass: Categorical
V3 fnlwgt: numerical
V4 education: Categorical
V5 education-num: categorical
V6 marital-status: Categorical.
V7 occupation: Categorical
v8 relationship: Categorical
V9 race:Categorical
V10 sex:Categorical
V11 capital-gain: numerical
V11 capital-loss: numerical
V13 hours-per-week: numerical
V14 native-country: categorical
V15 Label:categorical
##Q2d(i)
For V1 age variable
The number of unique values for V1(age) will be

```r
unqv1<-as.data.frame(table(adult$V1))
nrow(unqv1) #no of rows gives the no. of unique values 
```

```
## [1] 73
```
For V13 hours-per-week variable

```r
unqv13<-as.data.frame(table(adult$V13))
nrow(unqv13) #no of rows gives the no. of unique values 
```

```
## [1] 94
```

```r
hist(adult$V1,main="Histogram of age",xlab="age")
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)

```r
hist(adult$V3,main="Histogram of Capital-Gain",breaks=50,xlab="Capital-Gain")
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-2.png)
##Q2d(ii)

```r
age1<-adult[which(adult$V15==" <=50K"),1]
age2<-adult[which(adult$V15==" >50K"),1]
par(mfrow=c(1,2))
hist(age1,main="age(label <=50k)",xlab="age")
hist(age2,main="age(label >50k)",xlab="age")
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-1.png)


```r
hoursperweek1<-adult[which(adult$V15==" <=50K"),13]
hoursperweek2<-adult[which(adult$V15==" >50K"),13]
par(mfrow=c(1,2))
hist(hoursperweek1,main="hours per week(label <=50k)",xlab="Hours-per-week")
hist(hoursperweek2,main="hours per week(label >50k)",xlab="Hours-per-week")
```

![plot of chunk unnamed-chunk-29](figure/unnamed-chunk-29-1.png)
##Q2d(iii)

```r
par(mfrow=c(1,2))
boxplot(adult$V1~adult$V15,ylab="age")
boxplot(adult$V13~adult$V15,ylab="hours per week")
```

![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30-1.png)
##Q2d(iV)
Observations from the graph
Histograms
Number of persons with label <=50k is decreasing consistently from age 20 to 80.The number of people with label <=50K is highest for the age group 20-25 and lowest for age group 85 to 90.
but for label >50k , There is a consistent increase in of people iwth > 50 k label from the age 20 to 45 and from there, we can see a consistent decrease.The number of people with label >50K is highest for the age group 40to 45 and lowest for age group 85 to 90.

The number of people with label <=50 k has increased inconsistently with working hours per week . Maximum number of people with label <=50k are working for 35 to 40 hours and minimum number of people are working for 90 to 95 hours per week

The number of people with label >50K has increased inconsistently with working hours per week . Maximum number of people with label >50k are working for 35 to 40 hours and minimum number of people are working for 90 to 95 hours per week 


BoxPlots for Age
The median is people with lable <=50K with age around  age 37.
First quartile is from age 15 to around 24 
second quartile is from age around 24 to 35.
third quartile is from age 35 to 45
fourth quartile is from age 45 to 80

The median is people with lable >50K with age around age 42.
First quartile is from age 20 to around 35 
second quartile is from age around 35 to 42.
third quartile is from age 42 to 55
fourth quartile is from age 55 to 75

BoxPlots for hours per week
The median is people with lable <=50K are working for 40 hours per week.
First quartile is from hours 28 to around 38
second quartile is from hours around 38 to 40.
third quartile is around people working for 40 hours
fourth quartile is from working hours 40 to 48

The median is people with lable >50K are working for 40 hours per week.
First quartile is from hours 23 to 4o hours
second quartile are people working around 40 hours
third quartile is around people working for 40 to 48 hours
fourth quartile is from working hours 48 to 65
##Q2e(i)

```r
par(mfrow=c(2,1))
barplot(table(adult$V2),main="workclass distribution",xlab="workclass",las=2)
barplot(table(adult$V10),main="gender distribution",xlab="gender",las=2)
```

![plot of chunk unnamed-chunk-31](figure/unnamed-chunk-31-1.png)
##Q2e(ii)

```r
par(mfrow=c(2,1))
barplot(table(adult[which(adult$V15==" <=50K"),2]),main="workclass distribution with label <=50k",las=2)
barplot(table(adult[which(adult$V15==" >50K"),2]),main="workclass distribution with label >50k",las=2)
```

![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)

```r
par(mfrow=c(2,1))
barplot(table(adult[which(adult$V15==" <=50K"),10]),main="education distribution with label <=50k",las=2)
barplot(table(adult[which(adult$V15==" >50K"),10]),main="Gender with label >50k",las=2)
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-33-1.png)
##Q2e(iii)
From the histograms
Workclass with private job are more in number
MAles are more than females
Private job employees have the most  labe<= 50k and label >50k
femals with <50K are more than >50k label
Males are almost same number of label <=50k and >50k
##Q2f(i)

```r
library(ggplot2)
ggplot(adult,aes(V1,V2))+geom_point()
```

![plot of chunk unnamed-chunk-34](figure/unnamed-chunk-34-1.png)
Seeing the plot we can say that age and workclass are independent of eachother.As every workclass value is plotted to almost every age group 

```r
library(ggplot2)
ggplot(adult,aes(V1,V6))+geom_point()
```

![plot of chunk unnamed-chunk-35](figure/unnamed-chunk-35-1.png)
Seeing the plot we can say that age and martial status are independetn of eachother.As every martial status value is plotted to almost every age group 
##Q2f(ii)

```r
plot(adult$V1,adult$V13)
```

![plot of chunk unnamed-chunk-36](figure/unnamed-chunk-36-1.png)
hours per week increased from age group 20-30 and was consistent upto 60 and from 60 it decreased

##Q3-a
mpg(mpg-miles per gallon) is quantitative.
cylinders is quantitative
displacement is quantitative
horsepower is quantitative
weight is quantitative
acceleration is quantitative
year is quantitative
origin is quantitative
name is qualitative
##Q3-b
mode function

```r
auto<-read.csv("Auto.csv")
mode<-function(x)
{
  tab<-table(x)
  a<-names(tab)[tab==max(tab)]
  print(a)
}
```
mpg
Mean:` r mean(auto$mpg)`
Mode:` r mode(auto$mpg)`
Range:9, 46.6
Median:23

cylinders

Mean:` r mean(auto$cylinders)`
Mode:` r mode(auto$cylinders)`
Range:3, 8
Median:4

displacement

Mean:` r mean(auto$displacement)`
Mode:` r mode(auto$displacement)`
Range:68, 455
Median:146

horsepower

HP<-as.character(auto$horsepower) #converting horsepower from factor to character
HP[HP=="?"]<-0 #replacing "?"
HP<-as.numeric(HP)
Mean:` r mean(HP)`
Mode:` r mode(HP)`
Range:0, 230
Median:92

weight

Mean:` r mean(auto$weight)`
Mode:` r mode(auto$weight)`
Range:1613, 5140
Median:2800

acceleration

Mean:` r mean(auto$acceleration)`
Mode:` r mode(auto$acceleration)`
Range:8, 24.8
Median:15.5

year

Mean:` r mean(auto$year)`
Mode:` r mode(auto$year)`
Range:70, 82
Median:76

origin
Mean:` r mean(auto$origin)`
Mode:` r mode(auto$origin)`
Range:1, 3
Median:1

##Q3-c
displacement is numeric numeric

```r
DS<-auto$displacement
```
unique values of horsepower :130, 165, 150, 140, 198, 220, 215, 225, 190, 170, 160, 95, 97, 85, 88, 46, 87, 90, 113, 200, 210, 193, ?, 100, 105, 175, 153, 180, 110, 72, 86, 70, 76, 65, 69, 60, 80, 54, 208, 155, 112, 92, 145, 137, 158, 167, 94, 107, 230, 49, 75, 91, 122, 67, 83, 78, 52, 61, 93, 148, 129, 96, 71, 98, 115, 53, 81, 79, 120, 152, 102, 108, 68, 58, 149, 89, 63, 48, 66, 139, 103, 125, 133, 138, 135, 142, 77, 62, 132, 84, 64, 74, 116, 82
replacing "?" with 0

```r
HP<-as.character(auto$horsepower) #converting horsepower from factor to character
HP[HP=="?"]<-0 #replacing "?"
HP<-as.numeric(HP)
```
First quartile of displacement
104
37th percentile of displacement
120.52
89th percentile of displacement
350
First quartile of horsepower
75
37th percentile of horsepower
86
89th percentile of horsepower
152.44

##Q3-d
Five-number summary of displacement
68, 104, 146, 193.5, 262, 455
Five-number summary of horsepower
0, 75, 92, 103.2, 125, 230

##Q3-e
` r boxplot(DS,main="boxplot of displacement")`
` r boxplot(HP,main="boxplot of Horsepower")`

##Q3-f

As mpg is increasing , the displacement is decreasing
mpg and displacement are inversely proportional


As horsepower is increasing the displacement is increasing
horsepower and displacement are directly proportional


As weight is increasing the displacement is increasing.
weight and  displacement are directly proportional

##Q3-g


As mpg is increasing the cylinders number 
is decreasing but not consistently . Like 
for mpg aound 15 to 20 we have 3,4,5,6 
cylinder cars.But at mpg=40 we have 4 
cylinders but not 3.So cylinders willnot be 
useful for mpg prediction.


There is a consistent decrease in mpg with
increase in displacement with very few 
outliers. So displacement can be useful for 
prediction  of mpg.



ignoring 0 or missing values. mpg has a 
consistent decrease with increase in 
horsepower. And we cand say mpg is 
inversely proportional to horsepower.So horsepower can be useful for prediction of mpg


mpg has a consistent decrease with increase in weight. And we can say mpg is 
inversely proportional to weight.So weight can be useful for prediction of mpg


mpg and acceleration plot is very scattered.Low acceleration(around 10) have low mpg .but as acceleration increases the mpg is more scattered.acceleration cannot be a good factor for predication


We cant use year as factor for prediction of mpg. The graph is way to scattered. every year has almost all mpg.


origin cannot be a factor of prediction of mpg. clearly from the graoh every origin has alomost all the mpg values

So in conclusion amongst all the variable displacement,horsepower and weight can be best suited for predicting mpg.


##Q4-a(i)
For males

```r
mmatches<-read.csv("charting-m-matches.csv")
mstats<-read.csv("charting-m-stats-overview.csv")
m<-subset(mmatches,Tournament=="Australian Open"|Tournament=="French Open"|Tournament=="Wimbledon"|Tournament=="US Open")
mmerge<-merge(m,mstats,by="match_id")
mmerge$Date<-as.Date(mmerge$Date,"%Y%m%d")
mmerged<-subset(mmerge,Date>"2011-01-01")
mp1temp<-subset(mmerged,player==1)
mp2temp<-subset(mmerged,player==2)
mp1<-mp1temp[,c(1,2,7,8,17,18,20)]
mp2<-mp2temp[,c(1,3,7,8,17,18,20)]
mp1total<-subset(mp1,set=="Total")
mp2total<-subset(mp2,set=="Total")
colnames(mp1total)[colnames(mp1total)=="Player.1"]<-"player_name"
colnames(mp2total)[colnames(mp2total)=="Player.2"]<-"player_name"
mtotal<-rbind(mp1total,mp2total)
library("dtplyr")
groupm <- group_by(mtotal, player_name)
groupm2<-summarize(groupm, max.aces = max(aces))
#unique values
head(groupm2[order(groupm2$max.aces,decreasing = TRUE),])
```

```
## [1] 37
```

```r
#without unique values
mtotal2<-mtotal[,c(2,7)]
head(mtotal2[order(mtotal2$aces,decreasing = TRUE),])
```

```
##        player_name aces
## 1347  Nick Kyrgios   37
## 1199  Nick Kyrgios   33
## 1364 Roger Federer   29
## 1466  Ivo Karlovic   29
## 1308 Tomas Berdych   25
## 1555 Bernard Tomic   25
```
For Female

```r
wmatches<-read.csv("charting-w-matches.csv")
wstats<-read.csv("charting-w-stats-overview.csv")
w<-subset(wmatches,Tournament=="Australian Open"|Tournament=="French Open"|Tournament=="Wimbledon"|Tournament=="US Open")
wmerge<-merge(w,wstats,by="match_id")

#wmerge$Date<-as.Date(wmerge$Date,"%Y%m%d")

wmerged<-subset(wmerge,Date>"2011-01-01")
wp1temp<-subset(wmerged,player==1)
wp2temp<-subset(wmerged,player==2)
wp1<-wp1temp[,c(1,2,7,8,17,18,20)]
wp2<-wp2temp[,c(1,3,7,8,17,18,20)]
wp1total<-subset(wp1,set=="Total")
wp2total<-subset(wp2,set=="Total")
colnames(wp1total)[colnames(wp1total)=="Player.1"]<-"player_name"
colnames(wp2total)[colnames(wp2total)=="Player.2"]<-"player_name"

wtotal<-rbind(wp1total,wp2total)
library("dtplyr")
groupw <- group_by(wtotal, player_name)
groupw2<-summarize(groupw, max.aces = max(aces))
head(groupw2[order(groupw2$max.aces,decreasing = TRUE),])
```

```
## [1] 31
```

```r
#without unique values
wtotal2<-wtotal[,c(2,7)]
head(wtotal2[order(wtotal2$aces,decreasing = TRUE),])
```

```
##            player_name aces
## 1755 Kristyna Pliskova   31
## 1793   Maria Sharapova   21
## 677      Petra Kvitova   18
## 1386   Serena Williams   18
## 643    Serena Williams   17
## 1507   Serena Williams   17
```
##Q4-a(ii)
For Males

```r
mmatches<-read.csv("charting-m-matches.csv")
mstats<-read.csv("charting-m-stats-overview.csv")
m2merge<-merge(mmatches,mstats,by="match_id")
m2merge$Date<-as.Date(m2merge$Date,"%Y%m%d")
m2merged<-subset(m2merge,Date>"2015-01-01"&Date<="2015-12-31")
m2p1temp<-subset(m2merged,player==1)
m2p2temp<-subset(m2merged,player==2)
m2p1<-m2p1temp[,c(1,2,7,8,17,18,26,27)]
m2p2<-m2p1temp[,c(1,3,7,8,17,18,26,27)]

m2p1total<-subset(m2p1,set=="Total")
m2p2total<-subset(m2p2,set=="Total")
colnames(m2p1total)[colnames(m2p1total)=="Player.1"]<-"player_name"
colnames(m2p2total)[colnames(m2p2total)=="Player.2"]<-"player_name"

# library("dtplyr")
m2total<-rbind(m2p1total,m2p2total)
# group2m <- group_by(m2total, player_name)
# group2m2<-summarize(group2m, count.ply = count(player_name))
# group2m3<-subset(group2m2,count.ply.freq>="5")
# library(plyr)
m2total$per<-with(m2total,((bp_saved/bk_pts)*100))
#m2total100<-subset(m2total,per==100)
mcount<-as.data.frame(table(m2total$player_name))
mcount2<-subset(mcount,Freq>=5)
colnames(mcount2)[colnames(mcount2)=="Var1"]<-"player_name"
# m2totalcount<-count(m2total,c("player_name"))
# group2m3<-subset(group2m2,freq>=5)
#m2total$per<-with(m2total,((bp_saved/bk_pts)*100))
m2total2<-merge(mcount2,m2total,by="player_name")
m2total3<-m2total2[,c(1,10)]
#m2total4<-m2total3[order(m2total3$per,decreasing = TRUE),]
head(m2total3[order(m2total3$per,decreasing = TRUE),])
```

```
##      player_name per
## 17   Andy Murray 100
## 25   Andy Murray 100
## 38   Andy Murray 100
## 46 Bernard Tomic 100
## 60   Borna Coric 100
## 82  David Goffin 100
```
For females

```r
wmatches<-read.csv("charting-w-matches.csv")
wstats<-read.csv("charting-w-stats-overview.csv")
w2merge<-merge(wmatches,wstats,by="match_id")
#w2merge$Date<-as.Date(w2merge$Date,"%Y%m%d")
w2merged<-subset(w2merge,Date>"20150101"&Date<="20151231")
w2p1temp<-subset(w2merged,player==1)
w2p2temp<-subset(w2merged,player==2)
w2p1<-w2p1temp[,c(1,2,7,8,17,18,26,27)]
w2p2<-w2p1temp[,c(1,3,7,8,17,18,26,27)]


w2p1total<-subset(w2p1,set=="Total")
w2p2total<-subset(w2p2,set=="Total")
colnames(w2p1total)[colnames(w2p1total)=="Player.1"]<-"player_name"
colnames(w2p2total)[colnames(w2p2total)=="Player.2"]<-"player_name"

# library("dtplyr")
w2total<-rbind(w2p1total,w2p2total)
# group2m <- group_by(m2total, player_name)
# group2m2<-summarize(group2m, count.ply = count(player_name))
# group2m3<-subset(group2m2,count.ply.freq>="5")
# library(plyr)
w2total$per<-with(w2total,((bp_saved/bk_pts)*100))
#m2total100<-subset(m2total,per==100)
wcount<-as.data.frame(table(w2total$player_name))
wcount2<-subset(wcount,Freq>=5)
colnames(wcount2)[colnames(wcount2)=="Var1"]<-"player_name"
# m2totalcount<-count(m2total,c("player_name"))
# group2m3<-subset(group2m2,freq>=5)
#m2total$per<-with(m2total,((bp_saved/bk_pts)*100))
w2total2<-merge(wcount2,w2total,by="player_name")
w2total3<-w2total2[,c(1,10)]
#m2total4<-m2total3[order(m2total3$per,decreasing = TRUE),]
head(w2total3[order(w2total3$per,decreasing = TRUE),])
```

```
##            player_name per
## 9  Agnieszka Radwanska 100
## 27 Agnieszka Radwanska 100
## 28 Agnieszka Radwanska 100
## 32 Alison Van Uytvanck 100
## 36 Alison Van Uytvanck 100
## 48        Ana Ivanovic 100
```

