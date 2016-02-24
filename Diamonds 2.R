library(ggplot2)
data("diamonds")
## Scatterplot price vx x
qplot(x,y= price,data = diamonds)
## Correlation(price&x,price&y,price&z)
cor.test(diamonds$price,diamonds$x,methods='pearson')
cor.test(diamonds$price,diamonds$y,methods='pearson')
cor.test(diamonds$price,diamonds$z,methods='pearson')
## Scatterplot price vx x
qplot(depth,y= price,data = diamonds)
## Scatterplot price vx x (ADJ transparency 1/100th, x-axis marked every 2 units)
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha=1/100)+
  scale_x_continuous(breaks=seq(50,70,by=2))
## most diamonds are between 60 to 64
## Correlation of depth vs. price (-0.01 Would not use, very week correlation)
cor.test(diamonds$price,diamonds$depth,methods='pearson')
##Scatter price vs. carat (top 1% omitted)
qplot(x= price,y= carat,data = diamonds)+
  xlim(0,quantile(diamonds$price,0.99))+
  ylim(0,quantile(diamonds$carat,0.99))
##Scatter price vs. volume(x*y*z)
diamonds$volume<-diamonds$x*diamonds$y*diamonds$z
qplot(x= price,y= volume,data = diamonds)
##correlation price&volume(exclude 0 or >or= to 800)
diamondsReduced <- subset(diamonds, volume > 0 & volume <= 800)
with(diamondsReduced, cor.test(price, volume, method = "pearson"))
##Scatter price vs. volume (adj transparency and add linear model)
ggplot(data = diamondsReduced, aes(x = price, y = volume))+ 
  geom_point(alpha=1/100)+
  geom_smooth(method = "auto")
##I think the model is only somewhat useful at volumes below 150
## Data frame Diamonds by Clarity
library(dplyr)
##diamondsByClarity<-subset(diamonds)
mean_price<- mean(diamonds$price)
median_price<- median((diamonds$price))
min_price<- min((diamonds$price)) 
max_price<- max((diamonds$price))
n<-table(diamonds$clarity)
n<-as.data.frame(n)
diamondsByClarity<-data.frame(mean_price,median_price,min_price,max_price,n)
# create two bar plots on one output image 
# using the grid.arrange() function from the package
# gridExtra.
data(diamonds)
library(dplyr)
library(gridExtra)
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

g1 <- ggplot(data = diamonds_mp_by_clarity, aes(x = clarity, y = mean_price)) + 
  +     geom_bar(stat="identity")
g2 <- ggplot(data = diamonds_mp_by_color, aes(x = color, y = mean_price)) + 
  +      geom_bar(stat="identity")
grid.arrange(g1,g2)
##continue investigation at the end of problem set 3 or start fresh and shoose a different data set from Gapminder. 
# In your investigation, examine pairs of variable and create 2-5 plots that make
# use of the techniques from Lesson 4.
##Adjusted Close Percent vs. Log Volume
SPY$Adjusted.ClosePercent<-((SPY$Close-SPY$Open)/SPY$Open)*100
ggplot(data = SPY, aes(x = Volume, y = Adjusted.ClosePercent) )+
  geom_point(stat = 'identity')+
  scale_x_log10()

ggplot(aes(x = Volume, y = Adjusted.ClosePercent),data = SPY )+
  +     geom_point(alpha = 1/5, color = 'blue')+
  +     scale_x_log10()
  
## Absolute value of Adjusted Close Percent vs. Log Volume
ggplot(aes(x = Volume, y = abs(Adjusted.ClosePercent)),data = SPY )+
  geom_jitter(alpha = 1/10, color = 'blue')+
  scale_x_log10()+ geom_smooth()
