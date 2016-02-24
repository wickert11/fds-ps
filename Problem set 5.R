## Problem set 5
## Histogram of diamond prices (facet by color and colored bars)
ggplot(diamonds,aes(x= price, fill = cut)) + 
  geom_histogram() +
  facet_wrap(~color) + 
  scale_fill_brewer(type = 'qual')+
  scale_x_log10() 
## Scatter plot of diamond price vs. Tabel colored by cut
ggplot(aes(x = table, y = price), data = diamonds) +
  geom_point(aes(color = cut)) +
  scale_color_brewer(type='qual') +
  coord_cartesian(xlim = c(50,80)) +
  scale_x_discrete(breaks = seq(50,80,2))
## Scatter plot of diamond price vs. volume and clarity
diamonds$volume<-diamonds$x*diamonds$y*diamonds$z
ggplot(aes(x = volume, y = price), data = diamonds) +
   geom_point(aes(color = clarity))+
   scale_color_brewer(type='div')+
   scale_y_log10()+
   coord_cartesian(xlim=c(0,quantile(diamonds$volume,0.99)))
## Proportion of friendships initiated 
pseudo_facebook <- read.delim('/datasets/ud651/pseudo_facebook.tsv')
pseudo_facebook$prop_initiated <- pseudo_facebook$friendships_initiated / pseudo_facebook$friend_count
summary(pseudo_facebook$prop_initiated)
## prop_initiated vs. tenure 
pseudo_facebook$year_joined = floor(2014 - pseudo_facebook$tenure / 365)
pseudo_facebook$year_joined.bucket = cut(pseudo_facebook$year_joined,c(2004,2009,2011,2012,2014))
ggplot(aes(x = tenure, y = prop_initiated ), data = pseudo_facebook) +
  geom_line(aes(color=year_joined.bucket),stat = 'summary', fun.y=median)+
  scale_y_continuous(breaks = seq(0.00,0.75,0.25))
##Smoothing prop_initiated vs. tenure 
ggplot(aes(x = tenure, y = prop_initiated ), data = pseudo_facebook) +
  geom_line(aes(color=year_joined.bucket),stat = 'summary', fun.y=median)+
  scale_y_continuous(breaks = seq(0.00,0.75,0.25))+
  geom_smooth(method = "auto")
## people who joined after 2012 iniated the greates proportion of their friends
##Largest Group Mean prop_initiated 
with(subset(pseudo_facebook,year_joined>2012 & year_joined <=2014), summary(prop_initiated))
##mean proportion of frendships initiated 0.6654
##Price/Carat Binned, Faceted, & Colored 
diamonds$price_per_carat<-diamonds$price/diamonds$carat
ggplot(aes(x = cut, y = price_per_carat), data = diamonds)+
  geom_jitter(aes(color=color))+
  scale_color_brewer(type = 'div') +
  facet_wrap(~clarity)
# In your investigation, examine 3 or more variables and create 2-5 plots that make
# use of the techniques from Lesson 5.


## Absolute value of Adjusted Close Percent vs. Log Volume
ggplot(aes(x = Volume, y = abs(Adjusted.ClosePercent)),data = SPY )+
  geom_jitter(alpha = 1/7, color = 'red')+
  scale_x_log10()+ geom_smooth()

##Adjusted Close Percent vs. Volume
ggplot(aes(x = Volume, y = Adjusted.ClosePercent ), data = SPY) +
  geom_line(aes(color=Volume))+
  geom_smooth(method = "auto")
##Adjusted Close Percent vs. Log Volume
ggplot(aes(x = Volume, y = (Adjusted.ClosePercent) ), data = SPY) +
   geom_point(aes(color=Volume*Adjusted.ClosePercent,alpha = 1/7))+
   scale_x_log10()