###Ex 7.4 The data set fma::fancy concerns the monthly sales figures of a shop

library("fpp3")
#install.packages("fma")
library("fma")

help(fancy)
fancy
fancy_ts <- fancy %>% as_tsibble()
  
#a. Produce a time plot of the data and describe the patterns in the graph
autoplot(fancy)+
  xlab("Year/Month") + ylab("Sales") +
  ggtitle("Monthly sales for a souvenir shop")

#b.Explain why it is necessary to take logarithms of these data before fitting a model
autoplot(log(fancy))+
  xlab("Year/Month") + ylab("Sales in log value") +
  ggtitle("Monthly sales for a souvenir shop")

##here is another option that I think is pretty intuitive:
fancy_ts_log <- fancy_ts %>%
  mutate(surf_fest = ifelse(grepl("Mar",index) & year(index) >= 1988, 1, 0))

#c.Use R to fit a regression model to the logarithms of these sales data with a linear trend,
#seasonal dummies and a “surfing festival” dummy variable.

#Create surf dummy variable
  
log_fancy <- fancy_ts %>%
  mutate(surf_fest = ifelse(grepl("Mar",index) & year(index) >= 1988, 1, 0))  

#fit a tslm 
fit <- log_fancy %>%
  model(TSLM(log(value) ~ trend() + season() + surf_fest))
report(fit)  

#d. Plot the residuals against time and against the fitted values. 
#Do these plots reveal any problems with the model?

# Check the residuals(Do the residuals appear to be uncorrelated and normally distributed?)
augment(fit) %>%
  ggplot(aes(x=index, y=.resid)) +
  geom_point() +
  labs(x = "Year/Month", y = "Residuals") +
  ggtitle("Residuals against Time plot: Monthly sales for a souvenir shop")

#Residual plots against fitted values
augment(fit) %>%
  ggplot(aes(x=log(.fitted), y=.resid)) +
  geom_point() +
  labs(x = "Fitted", y = "Residuals") +
  ggtitle("Scatterplots of residuals versus fitted values") 

#e. Do boxplots of the residuals for each month. Does this reveal any problems with the model?
augment(fit) %>%
  ggplot(aes(x=month(index), y=.resid, group=month(index))) +
  geom_boxplot() + scale_x_continuous(breaks = 1:12, labels = month.abb[1:12], name = "Month") +
  geom_point() +
  labs(x = "Month", y = "Residuals") +
  ggtitle("Boxplots: Monthly sales for a souvenir shop")
  
#f. What do the values of the coefficients tell you about each variable?
report(fit)

#g. What does the Ljung-Box test tell you about your model?

fit %>% gg_tsresiduals()

fit %>% tidy()

augment(fit)%>% features(.resid, ljung_box, lag = 10, dof = 5) #reject null hyphothese test

#h. Regardless of your answers to the above questions, 
#use your regression model to predict the monthly sales for 1994, 1995, and 1996. 
#Produce prediction intervals for each of your forecasts.

future_data <- new_data(fancy_ts, 36) %>%
  mutate(surf_fest = ifelse(row_number()%%12 == 3,1,0))

fc_fancy <- forecast(fit, new_data=future_data)

fc_fancy %>%
  autoplot(log_fancy) +
  ggtitle("Forecasts of monthly sales on logarithms value") +
  xlab("Year") + ylab("Sales")

#i. Transform your predictions and intervals to obtain predictions and intervals for the raw data.
fc_fancy
###The fable package will automatically back-transform the forecasts whenever a transformation has been used in the model definition. 
###The back-transformed forecast distribution is then a “transformed Normal” distribution.

##The back-transformation of prediction intervals is done automatically for fable models, 
#provided that you have used a transformation in the model formula.

#j. How could you improve these predictions by modifying the model?


  