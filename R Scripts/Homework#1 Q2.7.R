#######################################################################################################################################
#Forecasting Methods HW#1 Question 2.7
#Jeremy Bolden
#Due Date: 9/11/20
#
#Question 2.7)Use the following graphics functions: autoplot(), gg_season(), gg_subseries(), gg_lag(), ACF() 
#and explore features from the following time series: Total Private Employed from us_employment, Bricks 
#from aus_production, Hare from pelt, H02 cost from PBS, and us_gasoline.
#
#  Can you spot any seasonality, cyclicity and trend?
#  What do you learn about the series?
#  What can you say about the seasonal patterns?
#  Can you identify any unusual years?
#######################################################################################################################################
#Definitions from textbook-
#Seasonal:
#     A seasonal pattern occurs when a time series is affected by seasonal factors such as the time of the year or 
#     the day of the week. Seasonality is always of a fixed and known period. 
#
#Cyclic:
#     A cycle occurs when the data exhibit rises and falls that are not of a fixed frequency. These fluctuations are usually
#     due to economic conditions, and are often related to the "business cycle". 
#     The duration of these fluctuations is usually at least 2 years.
#
#Trend
#     A trend exists when there is a long-term increase or decrease in the data. It does not have to be linear. 
#      Sometimes we will refer to a trend as "changing direction", when it might go from an increasing trend to a decreasing trend.
#########################################################################################################################################

#install packages to be called if not already done
library(fpp3)
library(tsibble)
library(ggplot2)
#starting with us_employement
us_employment

#autoplot
TotalPE <- us_employment %>%
  filter(Title == "Total Private" )
TotalPE %>%
  autoplot(Employed) +
  labs(title = "Time Series Analysis of US Employement", subtitle = "Count of Total Private Employed vs Time") +
  xlab("Year")

PEspecific <- us_employment %>%
  filter(Title == "Total Private", year(Month)>=2009) #Looking specifically at large decrease in employees
PEspecific %>%
  autoplot(Employed) +
  labs(title = "Time Series Analysis of US Employement", subtitle = "Count of Total Private Employed vs Time") +
  xlab("Year")
  
#gg_season()
TotalPE %>% gg_season(Employed, labels = "both") +
  ylab("Count") +
  ggtitle("Seasonal Plot of Employed Count")

TotalPE %>% gg_season(Employed, period="year") #shortcut... Not as clear to read though

#gg_subseries()
TotalPE %>% gg_subseries(Employed) + 
  ggtitle("Seasonal Plot of Employee Count") + 
  ylab("Count") + 
  xlab("Month")

#gg_lag()
TotalPE %>% gg_lag(Employed, geom = "point")
#We can see a strong postive positive correlation between all 9 lags. 

#ACF()
TotalPE %>% 
  ACF(Employed, lag_max = 12) %>%
  autoplot() + labs(title ="ACF vs Lag") + xlab("Lag")
#All lags are highly significant and positive

#Answer: From the different plot outputs above, there appears to be some slight seasonality where more people are employed as 
#the year progresses and starts lower again the next year. This would imply that there are a number of layoffs towards the 
#end of each year, but overall there are more people at the end of each progressing year. This coincides with the first 
#graph from autoplot(). There is clearly an upward trend in the number of employed going up year by year, however, the 
#correlation between each year does seem to decrease slightly. Looking at the first time series plot, we can see a large decrease
#in the number of employed in 2009, but after the start of 2010 is turns into a very cyclic hiring cycle with a continuing upward trend.
#########################################################################################################################################
#Now for aus_production
aus_production

#autoplot
TotalBricks <- aus_production %>%
  select(Bricks)
TotalBricks %>%
  autoplot(Bricks) +
  labs(title = "Brick Production vs Time") +
  xlab("Year")

BrickSpec <- aus_production %>%
  filter(year(Quarter)>=1970) 
BrickSpec %>%
  autoplot(Bricks) +
  labs(title = "Time Series Analysis of Brick Production") +
  xlab("Year")

#gg_season()
TotalBricks %>% gg_season(Bricks, labels = "both") +
  ylab("Count") +
  ggtitle("Seasonal Plot of Brick Production")

TotalBricks %>% gg_season(Bricks, period="year") #Shortcut

#gg_subseries()
TotalBricks %>% gg_subseries(Bricks) + 
  ggtitle("Seasonal Plot of Brick Production") + 
  ylab("Count") + 
  xlab("Year/Quarter")

#gg_lag()
TotalBricks %>% gg_lag(Bricks, geom = "point")


#ACF()
TotalBricks %>% 
  ACF(Bricks, lag_max = 12) %>%
  autoplot() + labs(title ="ACF vs Lag") + xlab("Lag")

#Answer/Observations: From this analysis of aus_production of bricks, we can see an upward trend in the data initially, however, 
#it becomes more volatile and level as the years progress. There does appear to be some seasonality with major highs and lows in production, 
#but it's not consistent each year. There was a major shift change in production in 1975 where the production changed from trending upwards,
#starting with a significant decline in 1975 and again around 1983. That being said, the lag plots show that each quarter does have a 
#positive correlation, with decreasing relationship with each lag. 
#########################################################################################################################################
#Pelt data
pelt

#autoplot
TotalHare <- pelt%>%
  select(Hare)
TotalHare %>%
  autoplot(Hare) +
  labs(title = "Hare Pelts Produced over time") +
  xlab("Year")

#HareSpec <- aus_production %>%   ################ use if need to find specific years
 # filter(year(Quarter)>=1970) 
#HareSpec %>%
 # autoplot(Hare) +
  #labs(title = "Time Series Analysis of Hare Pelt Production", subtitle = "Count of Hare Pelts Produced vs Time") +
 # xlab("Year")

#gg_season()
TotalHarefilt <- pelt%>%
  filter(Hare != 0) 
TotalHarefilt %>% gg_season(Hare, labels = "both") +
  ylab("Count") +
  ggtitle("Seasonal Plot of Hare Pelt Production") #Doesn't plot due to 0s in data


#gg_subseries()
TotalHare %>% gg_subseries(Hare) + 
  ggtitle("Seasonal Plot of Hare Pelt Production") + 
  ylab("Count") + 
  xlab("Time")  #No break down other than years, so basically the same plot as autoplot, but with an average line

#gg_lag()
TotalHare %>% gg_lag(Hare, geom = "point")
 

#ACF()
TotalHare %>% 
  ACF(Hare, lag_max = 12) %>%
  autoplot() + labs(title ="ACF vs Lag") + xlab("Lag")

#Answer/Observations: There appears to be a slight mix cyclical behavior but it's not glaringly obvious. There is no observable trend, up or down,
#present from the data provided. Looking at the raw data, we only have years so we can't plot monthly seasonality, but over the course
#of a few years, we begin to see a sharp increase in the number of Hare pelts made followed by a reciprocal decline. This can be attributed 
#to a large increase in overpopulation followed by overhunting of the Hare population. The mid 1860s showed the highest increase and decrease
#in total hare pelts produced. 
#########################################################################################################################################
#PBS Data
PBS

#Filter for H02
H02 <- PBS%>%
  filter(ATC2=="H02") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalCost = sum(Cost))
H02
#autoplot
H02 %>%
  autoplot(TotalCost) +
  labs(title = "Total Cost of H02 vs Time") +
  xlab("Year")

#gg_season()
H02 %>% gg_season(TotalCost, labels = "both") +
  ylab("Cost")+
  ggtitle("Seaonality of Cost for H02") 

H02 %>% gg_season(TotalCost, period="year") #shortcut


#gg_subseries()
H02%>% gg_subseries(TotalCost) + 
  ggtitle("Seasonal Plot of Cost of H02") + 
  ylab("Cost") + 
  xlab("Time")  #No break down other than years, so basically the same plot as autoplot, but with an average line

#gg_lag()
H02 %>% gg_lag(TotalCost, geom = "point")


#ACF()
H02 %>% 
  ACF(TotalCost, lag_max = 12) %>%
  autoplot() + labs(title ="ACF vs Lag") + xlab("Lag")

#Answer/Observations: There is a slight upward, cyclical trend in the data, discerable from the autoplot() function. WHen it comes to 
#seasonality, it's not really apparent, but when using gg_season, we can see a greater variance in pricing as well as a large drop in 
#Feb for some reason. The prices continually rise throughout the yearuntil the peak towards the end/beginning of the calender year. 
#The lag plot shows a positively correlated relationship with a decreasing number of related points to the early calender months.
#########################################################################################################################################
#us_gasoline
us_gasoline

#autoplot
gas <- us_gasoline
  autoplot(gas) +
  labs(title = "Total Number of Barrels vs Time") +
  xlab("Year")

#gg_season()
gas %>% gg_season(Barrels, labels = "both") +
  ylab("Cost") +
  ggtitle("Number of Barrels vs Time") 

#Closer look at specific years 
gasspec <- us_gasoline %>% 
  filter(year(Week)>=2005)
gasspec %>%
  gg_season(Barrels, period="year") #shortcut

#gg_subseries()
gas%>% gg_subseries(Barrels) + 
  ggtitle("Seasonal Plot of Number of Barrels") + 
  ylab("Countt") + 
  xlab("Time")  

#gg_lag()
gas %>% gg_lag(Barrels, geom = "point")

gas

#ACF()
gas %>% 
  ACF(Barrels, lag_max = 12) %>%
  autoplot() + labs(title ="ACF vs Lag") + xlab("Lag")

#Answer/Observations: We can see a gradual increase in the total number of barrels up until approximately year 2008, promoting a downward 
#trend. There does appear to be a lot of activity on the seasonal graphs. When using the subseries plot, we can look at each week's average
#number of barrels steadily increases to the warmer months of the year, and trends downward around Week 34, and 35. There are two significant
#increases at Weeks 42 and 51 that coincide almost perfectly with the major US holidays where most people travel; Thanksgiving and Christmas.
#Looking at the gg_lag() plot, there is nearly always a strong, positive correlation with, with only slight variance in lags 8 and 9.
#This can also be confirmed by the ACF() plot showing more than .8 acf for up to 12 lag.
#########################################################################################################################################