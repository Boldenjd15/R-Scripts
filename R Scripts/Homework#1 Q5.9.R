#######################################################################################################################################
#Forecasting Methods HW#1 Question 5.9
#Jeremy Bolden
#Due Date: 9/11/20
#
#Question 5.9: Create a training set for household wealth (hh_budget) by witholding the last four years as a test set.
#   Fit all the appropriate benchmark methods to the training set and forecast the periods covered by the test set.
#   Compute the accuracy of your forecasts. Which method does best?
#   Do the residuals from the best method resemble white noise?
#######################################################################################################################################
#install packages to be called if not already done
library(fpp3)
library(tsibble)
library(ggplot2)

hh_budget     #take a look at the data

hh_budget %>%
  autoplot(Wealth)+
  ylab("Total Wealth")+ xlab("Time")+
  ggtitle("Wealth per Country Over Time")   #Create a quick plot of wealth of each country over time

print(hh_budget$Year) #confirm years

#forecast with the linear model
fit <- hh_budget %>%
  model(trend_model = TSLM(Wealth ~ trend()))
#######################################################################################################################################
#Part 1

#plot the forecasts after the observed data for 4 years
fit %>% forecast(h = "4 years") %>%
  autoplot(hh_budget)+
  ylab("Total Wealth")+ xlab("Time")+
  ggtitle("Wealth per Country Over Time")
  
# Set training data from the last four years of data using slice, and need to group by to show subsets
train <- hh_budget %>% filter_index("1995" ~ "2013")

budget_fit <-train %>%
  model(Mean = MEAN(Wealth), `Naive` = NAIVE(Wealth),Drift = RW(Wealth ~ drift()))

#######################################################################################################################################
#Part 2:

# Let's look at the fitted values and the residuals /// Taken from notes & adjusted nomenclature 
View(augment(budget_fit))

augment(budget_fit) %>%
  filter(.model=="Mean")

augment(budget_fit) %>%
  filter(.model=="Naive")


# Generate forecasts for 4 years
budget_fc <- budget_fit %>% forecast(h=16)

budget_fc %>%
  autoplot(filter_index(hh_budget, "2013" ~ .), level = NULL) +
  xlab("Year") + ylab("Wealth") +
  ggtitle("Forecasts for Wealth by Country") +
  guides(colour=guide_legend(title="Forecast"))
#######################################################################################################################################
#Part 3

accuracy(budget_fc, hh_budget)

#Answer/Observations: Looking at the plots produced from the training set, the drift method is clearly the best
#because the others just provide a horizontal line about the mean or towards the end of the plot data. This can be
#confirmed by looking at the lower RMSE numbers.
#######################################################################################################################################
#Part 4   

#White noise notes taken from the book::::
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + ggtitle("White noise")
y %>% ACF(wn) %>% autoplot()

# Comparison to the budget data

hh_budget #refresher on table levels

wn_budget <- hh_budget %>%
  select(Country, Year, Wealth)
wn_budget %>%
  autoplot(Wealth) +
  labs(title = "ACF Plot of Wealth by Country", subtitle = "Comparison to White Noise") 

wn_budget %>%
  ACF(Wealth, lag_max = 12) %>%
  autoplot() + labs(title = "ACF Comparison Plot") + xlab("Lag")

#Answer/Observations: Looking a white noise ACF plot, there does not appear to be any lags above the ACF dotted blue line. 
#The autocorrelation for white noise should be where "95% of the spikes in the ACF to lie within #±2/???T where T is the length 
#of the time series." (Forecasting: Principles and Practice, 2020) Based on this idea, we can see that the only 1 or two spikes seen
#over the ACF line occur for the first 1 or 2 lags. This implies that this approximation is not much better than white noise models.
#######################################################################################################################################