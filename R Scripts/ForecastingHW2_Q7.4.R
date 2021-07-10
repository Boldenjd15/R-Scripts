#######################################################################################################################################
#Forecasting Methods HW#2 Question 7.4
#Jeremy Bolden
#Due Date: 10/23/19
#
#The data set fma::fancy concerns the monthly sales figures of a shop which opened in January 1987 and sells gifts,
# souvenirs, and novelties. The shop is situated on the wharf at a beach resort town in Queensland, Australia.
# The sales volume varies with the seasonal population of tourists. There is a large influx of visitors to the town at
# Christmas and for the local surfing festival, held every March since 1988. Over time, the shop has expanded its
# premises, range of products, and staff.

#A) Produce a time plot of the data and describe the patterns in the graph. Identify any unusual or unexpected fluctuations in the time series.
#B) Explain why it is necessary to take logarithms of these data before fitting a model.
#C) Use R to fit a regression model to the logarithms of these sales data with a linear trend, seasonal dummies and a “surfing festival” dummy variable.
#D) Plot the residuals against time and against the fitted values. Do these plots reveal any problems with the model?
#E) Do boxplots of the residuals for each month. Does this reveal any problems with the model?
#F) What do the values of the coefficients tell you about each variable?
#G) What does the Ljung-Box test tell you about your model?
#H) Regardless of your answers to the above questions, use your regression model to predict the monthly sales for
#   1994, 1995, and 1996. Produce prediction intervals for each of your forecasts.
#I) Transform your predictions and intervals to obtain predictions and intervals for the raw data.
#J) How could you improve these predictions by modifying the model?
#######################################################################################################################################
#install packages and libraries to be called if not already done (had to install fma for this dataset)
# Load libraries
library(fpp2)         # An older forecasting framework
library(fpp3)         # A newer tidy forecasting framework
library(timetk)       # An even newer tidy forecasting framework I found, created within a year or so (unsure if i'll use)
library(tidyverse)    # Collection of data manipulation tools
library(tsibble)
library(tidyquant)    # Business Science ggplot theme
library(fma)
####################################################################################################################################
#A) Produce a time plot of the data and describe the patterns in the graph. Identify any unusual or unexpected fluctuations in the time series.
#First let's take a look at the "fma::fancy" data
fancydata <- fma::fancy
fancydata
fancydata %>% str()
plot(fancy)
# We can see that we are looking at a time series(ts) dataset so must ignore facets if we use the pipeline method
fancydata %>%
  autoplot(facets = FALSE)+
  ggtitle("Time Series Plot of Fancy Data")  #same plot as above, just different method

# Looking at the plot, we can see that there is a cyclic trend of exponential growth towards the later half of each year.
# following the massive increase there is a equalizing decrease to at or marginally higher than what the pevious year's drop was.
# Since the magnitude of pattern increases with the level, it is imperative to consider log transformation of this
# variable to make the pattern and its variance stable   #Answer
####################################################################################################################################
#B) Explain why it is necessary to take logarithms of these data before fitting a model.

#The increasing seasonal fluctuations in the data set mean we must take logarithms in order to stabilize the variance
# so it does not increase with the level of the series. This should eliminate the increasing spikes around the end of each year.

#Let's take a look
plot(log(fancydata))
#plot(log((log(fancydata))))    Testing the log of a logged time series for part C
#or
log(fancydata) %>%
  autoplot(facets = FALSE)+
  ggtitle("Time Series Plot of Fancy Data")

#The relationship now appears to be trending more linearly.   #Answer
####################################################################################################################################
#C) Use R to fit a regression model to the logarithms of these sales data with a linear trend, seasonal dummies and a “surfing festival” dummy variable.
log.fancy <- log(fancydata)    #Take the log of fancydata

#create the dummy variable for surfing festival
dummy.surf = rep(0, length(fancydata))
dummy.surf[seq_along(dummy.surf)%%12 == 3] <- 1
dummy.surf[3] <- 0
dummy.surf <- ts(dummy.surf, freq = 12, start=c(1987,1))
dummy.surf
log.fancy.df <- data.frame(log.fancy,  dummy.surf)   #create a data frame with dummy variables and log of fancy

fit <- tslm(log.fancy ~ trend + season + dummy.surf, data=log.fancy.df) #fit the model using tslm and the dummy variable created.

future.surf <- data.frame(dummy.surf = rep(0, 12))
future.surf[3,] <- 1
future.surf
forecast(fit, newdata=future.surf)   #Answer
fit
####################################################################################################################################
#D) Plot the residuals against time and against the fitted values. Do these plots reveal any problems with the model?
# Plot of the residuals against time
#Using the fit calculated above, simply plot the residuals vs time first
plot(residuals(fit),
     type='p')
abline(h = 0, col="red")

#There does appear to be some nonlinearity in the trend, however there are some distinctions. From a negative decrease in years 1989 to 1991
# and then an increase between 1991 to 1994. Potentionally seasonality or at least somewhat periodic in nature.

# Plot of the residuals against the fitted values
plot(as.numeric(fitted(fit)),
     residuals(fit),
     type='p')   # BE SURE TO REMEMBER TO USE AS.NUMERIC WHEN PLOTTING!!! TOOK ME FOREVER TO FIGURE OUT WHY IT WASN'T WORKING
#The residuals plotted against the fitted show no decerable pattern.    #Answer
####################################################################################################################################
#E) Do boxplots of the residuals for each month. Does this reveal any problems with the model?
#Boxplot of residuals
#??cycle()      #cycle() gives the positions in the cycle of each observation. (Taken from stats HW#3)

boxplot(resid(fit) ~ cycle(resid(fit)))

#Answer - There appears to be a dispargingly high variance in the residual min and max quartiles, even though the mean appears more consistent.
# There also appears to be several months with both extremes of the max and minimum value compared the average. There may be some slight
# seasonality due to the July appears to be an inflextion point compared to the variance of the other months.

####################################################################################################################################
#F) What do the values of the coefficients tell you about each variable?
summary(fit)

#From the output:
#
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
#(Intercept) 7.6196670  0.0742471 102.626  < 2e-16 ***
#trend       0.0220198  0.0008268  26.634  < 2e-16 ***
#season2     0.2514168  0.0956790   2.628 0.010555 *
#season3     0.2660828  0.1934044   1.376 0.173275
#...
#season12    1.9622412  0.0961066  20.417  < 2e-16 ***
#dummy.surf  0.5015151  0.1964273   2.553 0.012856 *
#
# Residual standard error: 0.179 on 70 degrees of freedom
#Multiple R-squared:  0.9567,	Adjusted R-squared:  0.9487
#F-statistic:   119 on 13 and 70 DF,  p-value: < 2.2e-16

#For Trend - there is an increasing significant trend in the data
#For dummy.surf - there is a statistically significant indication in the month of March that correlates to the same
# time the surfing festival takes place.
#For Seasons 2-12, all months are statistically significant, however not with March as it is possibly more explained by the festival.
#We may be able to test for multicolinearity but it should only confirm the aforementioned conclusion.    #Answer
####################################################################################################################################
#G) What does the Ljung-Box test tell you about your model?
LBtest <- Box.test(resid(fit), lag =10, type = "Ljung-Box", fitdf = 0)   #be sure to capitalize B!!
LBtest


fit %>% tidy()


#Answer -
# data:  resid(fit)
#X-squared = 69.779, df = 10, p-value = 4.891e-11
#Given that the p-value for the test is essentially zero, therefore we can reject the
# null hypothesis that our model
#shows an accurate fit to the data.
####################################################################################################################################
#H) Regardless of your answers to the above questions, use your regression model to predict the monthly sales for
#   1994, 1995, and 1996. Produce prediction intervals for each of your forecasts.
future.surf <- data.frame(dummy.surf = rep(0, 36))
predictions <- forecast(fit, newdata=future.surf)

plot(predictions)

####################################################################################################################################
#I) Transform your predictions and intervals to obtain predictions and intervals for the raw data.
predictions.tr <- exp(data.frame(predictions))

plot(predictions.tr) #This looks too uniform?
####################################################################################################################################
#J) How could you improve these predictions by modifying the model?
par(mfrow=c(1,2))
plot(log.fancy,
     xlab="Time", ylab="Log Sales", main="Fancy Store History")

plot(predictions$mean,
     xlab="Time", ylab="Log Sales",main="Fancy Store Forecast")

forecast <-ts(predictions.tr$Point.Forecast,freq=12,start=c(1994,1))

par(mfrow=c(1,2))
plot(fancy,
     xlab="Time",ylab="Sales", main="Fancy Store History")
plot(forecast,
     xlab="Time",ylab="Sales", main="Fancy Store Forecast")

#We may seem to be overfitting here. Seasonality seems to be covered well by the forecasts, however trend seems bit too
# high as we see the very high total sales number(y axis of the two graphs above). We may need to look at adding more
# variables and find a better fitting model. Or fixing the non-linearity of trend may also help.
####################################################################################################################################
