##Diamonds
summary(diamonds)
?diamonds
## a) 53940 observations
## b) 10 variables
## c) 3 ordered factors
## d) D represents best diamond color
## Price Histogram
hist(diamonds$price)
## Price Histogram Summary
mean(diamonds$price)
median(diamonds$price)
##Diamond price is skewed to the right with the mean to the right of the median
## Diamond Counts
## 1729 cost less than $500
## 0 cost less than $250
## 1655 cost more than $1500
## Cheaper Diamonds
hist(diamonds$price,breaks=1000,xlim= c(0,2000))
## Price by Cut Histograms
qplot(x=price,data=diamonds,xlim=c(0,20000),xlab=seq(0,20000,100),binwidth=25)+facet_wrap(~cut)
##Price by Cut
by(diamonds$price,diamonds$cut,summary)
by(diamonds$price,diamonds$cut,max)
by(diamonds$price,diamonds$cut,min)
by(diamonds$price,diamonds$cut,median)
##a)Premium highest priced
##b)premium,Ideal lowest price
##c)Ideal lowest median price
## Scales and Multiple Histograms
qplot(x = price, data = diamonds) + facet_wrap(~cut,scales = "free")
## Price per Carat by Cut
qplot(x = price, data = diamonds,bins=15) + facet_wrap(~cut,scales = "free")+scale_x_log10()
## Price Box Plots
qplot(x=clarity,y=price,data= subset(diamonds),geom = 'boxplot')
## Interquartile Range-IQR
by(diamonds$price,diamonds$color,summary)
by(diamonds$price,diamonds$color,IQR)
##a) For color D first quartile, third quartile (911,4214)
##b) For color J first quartile, third quartile (1860,7695)
##c) IQR for diamonds with best color 3302.5
##d) IQR with diamonds with worst color 5834.5
## Price Per Carat Box Plots by Color
qplot(x=color,y=price,data= subset(diamonds),geom = 'boxplot')
## Carat Frequency Polygon
qplot(x=carat,data= subset(diamonds),geom = 'freqpoly',bins=200)
table (diamonds$carat)
## 0.3, 1.01 sizes have counts > 2000

## Gapminder HIV prevalence data
qplot(x=prev$X1990,data=na.omit(prev))
qplot(x=prev$X2010,data=na.omit(prev))
























