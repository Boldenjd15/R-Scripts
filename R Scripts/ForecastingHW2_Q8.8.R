#######################################################################################################################################
#Forecasting Methods HW#2 Question 8.8
#Jeremy Bolden
#Due Date: 10/23/19
#
#Question 8.8) Find an ETS model for the Gas data from aus_production and forecast the next few years.
# Why is multiplicative seasonality necessary here?
# Experiment with making the trend damped. Does it improve the forecasts?
#######################################################################################################################################
library(fpp3)
library(tsibble)
library(ggplot2)

aus_production   #take a look at the data first

#Have a user created time frame to look at. Running this multiple times should present similar but diff results for the model.
window1 <- as.integer(readline(prompt="Enter Start Year (1960-2010): "))   #select the start year

#Select the data we are going to model
gasdata <- aus_production%>%
  filter(year(Quarter) >= window1) %>%
  select(Quarter, Gas)
gasdata %>%
  autoplot()

#Generate the fit model using ETS()
fit <- gasdata %>%
  model(ETS(Gas))  #find the fit model using ETS function

report(fit)        #Report fit values
#Series: Gas
#Model: ETS(A,A,A)
#  Smoothing parameters:
#    alpha = 0.5538953
#    beta  = 0.0001056212
#    gamma = 0.0001000068
#
#  Initial states:
#        l        b        s1       s2       s3       s4
# 140.7059 1.014758 -12.77656 26.29588 10.36629 -23.8856
#
#  sigma^2:  30.438
#
#     AIC     AICc      BIC
#651.0201 653.5201 672.6806

#Let's take a look at the component for curiosity
components(fit) %>%
  autoplot() +
  ggtitle("ETS(M,N,M) Components")   #Plot the components

#plot the forecast for the next few quarters
fit %>%
  forecast(h = 8) %>%
  autoplot(gasdata) +
  ggtitle("Australian Gas Forecast") +
  ylab("Amount of Gas Produced")            #Plot of the forecast for the next few years


#Why is multiplicative seasonality necessary here?
# From the seasonal plots, we can easily see that the general spikes in sales show an exponential growth with a general
# return to a moderately low average. If this were additive seasonality, we would have a much greater trending average
# of sales over time than what we see here. 

########################################################################################################################
#Experiment with making the trend damped. Does it improve the forecasts?

#Changing the window size manually
recent_gas <- gasdata
recent_gas %>%
  model(STL(Gas ~ trend(window=7) + season(window=1),
            robust = TRUE)) %>%
  components() %>%
  autoplot()

#For comparison, let's try window = default(21) for trend, and change the season to 9, 19 and periodic (inf).
recent_gas %>%
  model(STL(Gas ~ trend(window=21) + season(window=9),
            robust = TRUE)) %>%
  components() %>%
  autoplot()

recent_gas %>% #default window for trend is = 21
  model(STL(Gas ~ trend() + season(window=19),
            robust = TRUE)) %>%
  components() %>%
  autoplot()

recent_gas %>% #default window for trend is = 21
  model(STL(Gas ~ trend() + season(window='periodic'), #Periodic is the same as setting window = infinity
            robust = TRUE)) %>%
  components() %>%
  autoplot()

#Alternatively...
#making it fancy... enter whichever window sizes would like in the console.
window1 <- as.integer(readline(prompt="Enter Window Size 1 of 3: "))
window2 <- as.integer(readline(prompt="Enter Window Size 2 of 3: "))
window3 <- as.integer(readline(prompt="Enter Window Size 3 of 3: "))


gas_volume <- recent_gas %>%
  mutate(
    MA1 = slide_dbl(Gas, mean, .size = window1, .align = "center"),
    MA2 = slide_dbl(Gas, mean, .size = window2, .align = "center"),
    MA3 = slide_dbl(Gas, mean, .size = window3, .align = "center"))

gas_volume %>%
  autoplot(Gas) +
  autolayer(gas_volume, `MA1`, color='green') +
  autolayer(gas_volume, `MA2`, color = "red") +
  autolayer(gas_volume, `MA3`, color='blue')+
  xlab("Year") + ylab("Amount Produced") + ggtitle("Total Canadian production") +
  guides(color=guide_legend(title="series"))

##No, changing the window does not really change the accuracy of the model as it is very periodic in nature. Looking at the seasonality
#would be a possible method to gain further insight into the data. 






