#######################################################################################################################################
#Forecasting Project
#Jeremy Bolden, Joe Gardner, Liliana Castillo, Beth Vasquez
#Due Date: 11/19/20
#
#This script will be used to analyze solar data collected from solar power production from the United States
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

# install packages to be called if not already done
# fpp3 is the shortcut to pickup the majority of the packages needed, even though it picks up sample data
require(fpp3)
require(tidyverse)
require(forecast)


# setup work:
# read in the data
us_power  <- read.csv('C:/Users/bolde/Desktop/!School_Work_Fall_2020/Forecasting Methods/forecasting Project/US_SOLAR_PRODUCTION.csv',
                         header= TRUE, sep = ",", encoding= "UTF-8")

# check structure and column names
str(us_power)

# make some changes
colnames(us_power)[1] <- "date" #need to change the date header
colnames(us_power)[2] <- "megawatts" # let's shorten the header for simplicity
us_power$date <- as.POSIXct(us_power$date, format = "%Y-%m-%d")
us_power <- as_tsibble(us_power)

# verify changes
str(us_power)

##################################################################################################################

# data understanding
# Plot the data
us_power %>%
  mutate(date = yearmonth(date)) %>%
  autoplot(megawatts) +
  ggtitle("US Daily Solar Production") +
  xlab("Time") +
  ylab("Megawatts")

# Filter from years 2013 forward due to the signficant trend
# from increased usage that would prevent an accurate forecast
us_power_subset <- us_power  %>%
  mutate(date= yearmonth(date)) %>%
  filter(year(date) >= 2013)

# replot to see the trimmed data
us_power_subset %>%
  mutate(date = yearmonth(date)) %>%
  autoplot(megawatts) +
  ggtitle("Plot of Daily Yield of Solar Years 2013+") +
  xlab("Time") +
  ylab("Megawatts")

# get a look at those seasons
us_power_subset %>%
  gg_season(megawatts, labels = "both") +
  ylab("megawatts") +
  ggtitle("Seasonal plot by year: Megawatts Produced")

# and break them down further
us_power_subset %>%
  gg_subseries(megawatts) +
  ylab("megawatts") +
  xlab("Year") +
  ggtitle("Seasonal subseries plot: Megawatts Produced")

# gg_lag
us_power_subset %>%
  gg_lag(megawatts, geom="point")

# trend (upwards) and seasonality together
# slow decline in ACF shows trend
# wave shape shows seasonality seasonality
us_power_subset %>%
  ACF(megawatts, lag_max = 48) %>%
  autoplot()

##################################################################################################################

# Make it easier to keep track!
#Create a training and test set of data for our forecast
train_solar <- us_power_subset %>%
  filter(year(date) <= 2018) %>%
  mutate(date= yearmonth(date))

test_solar <- us_power_subset %>%
  filter(year(date) > 2018) %>%
  mutate(date= yearmonth(date))

##################################################################################################################
#Let's looks at the decomposition of the data
# SEATS decomposition can be used with quarterly and monthly data
seats_dcmp <- us_power_subset %>%
  model(seats = feasts:::SEATS(megawatts)) %>%
  components()

autoplot(seats_dcmp) + xlab("Year") +
  ggtitle("SEATS decomposition of total US solar production")

# STL decomposition is probably the most robust method
STL_dcmp <- us_power_subset %>%
  model(STL(megawatts ~ trend(window=7) + season(window='periodic'),
            robust = TRUE)) %>%
  components()

autoplot(STL_dcmp) + xlab('Year')+
  ggtitle("STL decomposition of the total US solar production")

x11_dcmp <- us_power_subset %>%
  model(x11 = feasts:::X11(megawatts, type = "additive")) %>%
  components()

autoplot(x11_dcmp) + xlab("Year") +
  ggtitle("Additive X11 decomposition of US solar production")

#plot of the X11_DCMP
x11_dcmp %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = megawatts, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  xlab("Year") + ylab("Megawatts") +
  ggtitle("X11 Total Production of US Solar") +
  scale_colour_manual(values=c("green","yellow","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

#plot of the SEATS DCMP
# NOTE: Lines are somewhat hard to see. Shift to "Dark Mode for better visibility or change colors on line SCM...
seats_dcmp %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = megawatts, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  xlab("Year") + ylab("Megawatts") +
  ggtitle("SEATS Total Production of US Solar") +
  scale_colour_manual(values=c("green","yellow","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

#plot of the STL DCMP
# NOTE: Lines are somewhat hard to see. Shift to "Dark Mode for better visibility or change colors on line SCM...
STL_dcmp %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = megawatts, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  xlab("Year") + ylab("Megawatts") +
  ggtitle("STL Total Production of US Solar") +
  scale_colour_manual(values=c("green","yellow","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

#################################################################
#Let's start forcasting!
#Starting with the simple time series Linear Regression...
# We can do a simple time series linear regression with trend and season
# we clearly saw both in the autoplot
fit_TSLM <- us_power_subset %>%
  model(trend_model = TSLM(megawatts ~ trend()+ season()))

# We can forecast with the linear model (Example: 2 years)
fit_TSLM %>% forecast(h = "2 years")

# and we can plot the forecasts after the observed data
# this includes prediction intervals
fit_TSLM %>% forecast(h = "2 years") %>%
  autoplot(us_power_subset) +
  ggtitle("Forecast for US Solar Production") +
  ylab("Megawatts")

#Let's check the accuracy of our forecast...
accuracy(fit_TSLM)
#  .model      .type           ME    RMSE    MAE   MPE  MAPE  MASE  ACF1
#  <chr>       <chr>        <dbl>   <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>
#1 trend_model Training -9.92e-15 {{ 700.}}  563.  8.50  32.6 0.595 0.761


augment(fit_TSLM) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = megawatts, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(x = "Year", y = "Megawatts",
       title = "Solar Production")


#let's plot the fitted versus the actual values
augment(fit_TSLM) %>%
  ggplot(aes(x = megawatts, y = .fitted,
             colour = factor(month(date)))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Monthly Solar Production") +
  geom_abline(intercept=0, slope=1)

#####################################################################################################################################################
# Now let's try the Mean, NAIVE, SNAIVE  and SNAIVE+Drift models.
# Given that this is very seasonal data, SNAIVE with drift should fit best

# MEAN forecasts the average value of the training data
# NAIVE forecasts the last value
# SNAIVE forecasts the value one seasonal period back
# DRIFT forecasts the SNAIVE model with the trend values
power_fit <- train_solar %>%
  model(
    Mean = MEAN(megawatts),
    `Naïve` = NAIVE(megawatts),
    `Seasonal naïve` = SNAIVE(megawatts),
    Drift = SNAIVE(megawatts ~ drift())
  )
# Let's look at the fitted values and the residuals
augment(power_fit)

augment(power_fit) %>%
  filter(.model=="Mean")

augment(power_fit) %>%
  filter(.model=="Naive")

augment(power_fit) %>%
  filter(.model=="Seasonal Naive")

augment(power_fit) %>%
  filter(.model=="Drive")
# Generate forecasts for 3 years
power_fc <- power_fit %>% forecast(h=36)

# Plot forecasts against actual values
# The DRIFT forecast looks the best in this case!
power_fc %>%
  autoplot(train_solar , level = NULL) +
  autolayer((test_solar), color = "black") +
  ggtitle("Simple Forecasting Models") +
  xlab("Year") + ylab("Megawatts") +
  guides(colour=guide_legend(title="Forecast"))

#Let's check the accuracy of all four of our models so far...
accuracy(power_fc, test_solar)
#  .model          .type     ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
#  <chr>           <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#1 Drift           Test    20.1  642.  490. -2.34  7.83   NaN 0.462
#2 Mean            Test  3948.  4453. 3948. 53.3  53.3    NaN 0.772
#3 NaÃ¯ve          Test  3682.  4219. 3682. 49.0  49.0    NaN 0.772
#4 Seasonal naÃ¯ve Test  1298.  1606. 1304. 17.6  17.7    NaN 0.672
##########################################################################################################
#Looking at the ARIMA Model-

# Fit and test an ARIMA model
fit_arima <- test_solar %>%
  model(ARIMA(megawatts)) %>%
  report(fit_arima)
#Model: ARIMA(1,0,0)(0,1,0)[12] w/ drift
#Coefficients:
#         ar1  constant
#      0.7503  213.0943
#s.e.  0.0867   42.2820
#sigma^2 estimated as 119406:  log likelihood=-435.24
#AIC=876.48   AICc=876.91   BIC=882.77

gg_tsresiduals(fit_arima, lag_max = 16)

augment(fit_arima) %>%
  features(.resid, ljung_box, lag = 16, dof = 6)

#    .model           lb_stat lb_pvalue
#  <chr>              <dbl>     <dbl>
#1 ARIMA(megawatts)    22.3    0.0135

#Create the ARIMA model and report values
# For this we used the step-wise method to find the best fitting model
ARIMAfit1 <- train_solar %>%
  mutate(date = yearmonth(date))%>%
  model(ARIMA(megawatts)) %>%
  report(ARIMAfit)
#Series: megawatts
#Model: ARIMA(1,0,0)(0,1,0)[12] w/ drift
#
#Coefficients:
#         ar1  constant
#      0.7503  213.0943
#s.e.  0.0867   42.2820
#
#sigma^2 estimated as 119406:  log likelihood=-435.24
#AIC=876.48   AICc=876.91   BIC=882.77

#Now plot the ARIMA Fit model for the next few cycles
ARIMAfit1 %>%
  forecast(h=36) %>%
  autoplot(slice(us_power_subset, (n()-80):n()))

#Let's looks at some manually adjusted models
ARIMAfit2 <- train_solar %>%
  mutate(date = yearmonth(date))%>%
  model(ARIMA(megawatts ~ PDQ(0,0,0)))%>%
  report(ARIMAfit2)
#Model: ARIMA(1,1,0)
#
#Coefficients:
#         ar1
#      0.5869
#s.e.  0.0956
#
#sigma^2 estimated as 170626:  log likelihood=-528.13
#AIC=1060.26   AICc=1060.43   BIC=1064.78
ARIMAfit2 %>%
  forecast(h=36) %>%
  autoplot(slice(us_power_subset, (n()-80):n()))

ARIMAfit3 <- train_solar %>%
  mutate(date = yearmonth(date))%>%
  model(arima = ARIMA(megawatts ~ pdq(3,1,0) + PDQ(0,0,0)))%>%
  report()
#Model: ARIMA(3,1,0)
#
#Coefficients:
#         ar1     ar2      ar3
#      0.5760  0.1652  -0.2499
#s.e.  0.1135  0.1328   0.1178
#
#sigma^2 estimated as 164601:  log likelihood=-525.92
#AIC=1059.84   AICc=1060.44   BIC=1068.89

ARIMAfit4 <- train_solar %>%
  mutate(date = yearmonth(date))%>%
  model(arima = ARIMA(megawatts ~ pdq(0,1,3) + PDQ(0,0,0)))%>%
  report()
#Model: ARIMA(0,1,3)
#Coefficients:
#         ma1     ma2     ma3
#      0.6968  0.4644  0.7273
#s.e.  0.0994  0.1146  0.0955
#sigma^2 estimated as 274090:  log likelihood=-699.04
#AIC=1406.09   AICc=1406.55   BIC=1416.13

ARIMAfit4 <- us_power_subset %>%
  mutate(date = yearmonth(date))%>%
  model(arima = ARIMA(megawatts ~ pdq(3,1,1) + PDQ(0,0,0)))%>%
  report()
#Model: ARIMA(0,1,3)
#
#Coefficients:
#         ma1     ma2     ma3
#      0.4847  0.4655  0.4039
#s.e.  0.1100  0.1003  0.1253
#
#sigma^2 estimated as 159398:  log likelihood=-524.89
#AIC=1057.77   AICc=1058.38   BIC=1066.82

#Let's Generate a forecast for the ARIMA Model...
train_solar %>%
  model(ARIMA(megawatts)) %>%
  forecast(h="3 years") %>%
  autoplot(us_power_subset)+
  ggtitle("ARIMA Forecast")+
  xlab("Time") + ylab("Megawatts")
################################################################################################################


train_solar %>%
  features(megawatts, unitroot_nsdiffs)
#nsdiffs = 1

train_solar %>%
  features(difference(megawatts, 4), unitroot_ndiffs)
# ndiffs = 0

# The ACF and PACF show the effect of the differencing
train_solar %>%
  gg_tsdisplay(megawatts, plot_type='partial')

train_solar %>%
  gg_tsdisplay(megawatts %>% difference(4), plot_type='partial')

train_solar %>%
  gg_tsdisplay(megawatts %>% difference(4) %>% difference(), plot_type='partial')



################################################################################################################
# Fit and test an ETS model
fit_ets <- train_solar %>%
  model(ETS(megawatts)) %>%
  report(fit_ets)

fit_ets %>% gg_tsresiduals(lag_max = 16)

augment(fit_ets) %>%
  features(.resid, ljung_box, lag = 16, dof = 6)

ets1_fit <- train_solar %>%
  #stretch_tsibble(.init = 1) %>%
  model(
  ses = ETS(megawatts ~ error("A") + trend("N") + season("N")),
  "Holt's method" = ETS(megawatts ~ error("A") + trend("N") + season("N")),
  "Damped Holt's method" = ETS(megawatts ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  )

ets1_fc <- ets1_fit %>% forecast(h=36)

ets1_fc %>%
  autoplot(train_solar , level = NULL) +
  autolayer((test_solar), color = "black") +
  ggtitle("ETS Forecasting Models") +
  xlab("Year") + ylab("Megawatts") +
  guides(colour=guide_legend(title="Forecast"))

ets2_fit <- train_solar %>%
  model(`Damped Holt's method` = ETS(megawatts ~ error("A") + trend("Ad", phi = 0.9) + season("N")),
        Additive = ETS(megawatts ~ error("A") + trend("A") + season("A")),
        Multiplicative = ETS(megawatts ~ error("M") + trend("A") + season("M"))
  )

ets2_fc <- ets2_fit %>% forecast(h=36)
report(ets2_fit)

# Plot forecasts
ets2_fc %>%
  autoplot(train_solar , level = NULL) +
  autolayer(test_solar, color = "black") +
  ggtitle("Sample Forecasting Models") +
  xlab("Year") + ylab("Megawatts") +
  guides(colour=guide_legend(title="Forecast"))

#Clearly, the regular or Damped Holt's method does not provide any useful insight into the forecast and
# not a model reflective of measuring this type of seasonal data.
#Additive is better than Holt's, however it does not account for the increase in magnitude over time
#Multiplicative is the best model of the three as it shows the increase in magnitude with seasonality.

################################################################################################################
# Compare accuracy over all of the models
bind_rows(
  fit_arima %>%accuracy(),
  fit_ets %>%accuracy(),
  power_fit %>%accuracy(),
  ets2_fit %>%accuracy()
)

#.model               .type           ME  RMSE   MAE     MPE  MAPE  MASE   ACF1
#  <chr>                <chr>        <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>  <dbl>
#1 ARIMA(megawatts)     Training  6.21e+ 0  310.  202.  -1.44   7.21 0.221 0.0136
#2 ETS(megawatts)       Training  1.12e+ 1  217.  160.  -0.127  6.03 0.176 0.298
#3 Mean                 Training  1.59e-13 1830. 1502. -68.7   96.7  1.64  0.949
#4 NaÃ¯ve               Training  3.94e+ 1  507.  364.   1.79  13.3  0.399 0.578
#5 Seasonal naÃ¯ve      Training  9.13e+ 2 1044.  913.  31.1   31.1  1     0.717
#6 Drift                Training  3.64e-13  505.  379.  -6.79  14.7  0.415 0.717
#7 Damped Holt's method Training -7.71e+ 0  422.  290.   0.713 11.8  0.318 0.0131
#8 Additive             Training -1.93e+ 1  352.  272.   0.715 18.7  0.298 0.287

################### Extra Credit:::::

ESfit <- train_solar %>%
  model(Drift = SNAIVE(megawatts),
        ARIMA = ARIMA(megawatts),
        Multiplicative = ETS(megawatts ~ error("M") + trend("A") + season("M")))%>%
  mutate(COMB=combination_ensemble(Drift, ARIMA, Multiplicative, weights = "inv_var"))

ES_fc <- ESfit %>%
  forecast(h=36)

ES_fc %>% filter(.model=="Drift") %>% autoplot(us_power_subset)
ES_fc %>% filter(.model=="ARIMA") %>% autoplot(us_power_subset)

ES_fc %>% filter(.model=="COMB") %>%
  autoplot(us_power_subset) +
  ggtitle("Ensemble Forecast (ARIMA + Drift + Multiplicative)")+
  xlab("Time") + ylab("Megawatts")



ES_fc %>% accuracy(
  data = test_solar,
  measures = list(crps=CRPS, mape=MAPE, rmse=RMSE)
) %>%
  arrange(mape)
#when we added multiplicative ETS it made the forecast worse!
#  .model         .type  crps  mape  rmse
#  <chr>          <chr> <dbl> <dbl> <dbl>
#1 ARIMA          Test   352.  6.84  689.
#2 COMB           Test   708. 14.1  1277.
#3 Multiplicative Test   708. 14.1  1277.
#4 Drift          Test   888. 17.7  1606.

ESfit_2 <- train_solar %>%
  model(Drift = SNAIVE(megawatts),
        ARIMA = ARIMA(megawatts))%>%
  mutate(COMB=combination_ensemble(Drift, ARIMA, weights = "inv_var"))

ES_fc_2 <- ESfit_2  %>%
  forecast(h=36)
ES_fc_2 %>% filter(.model=="COMB") %>%
  autoplot(us_power_subset) +
  ggtitle("Ensemble Forecast (ARIMA + Drift)")+
  xlab("Time") + ylab("Megawatts")

ES_fc_2 %>% accuracy(
  data = test_solar,
  measures = list(crps=CRPS, mape=MAPE, rmse=RMSE)
) %>%
  arrange(mape)
#  .model .type  crps  mape  rmse
#  <chr>  <chr> <dbl> <dbl> <dbl>
#1 ARIMA  Test   352.  6.84  689.
#2 COMB   Test   475.  8.93  909.
#3 Drift  Test   888. 17.7  1606.
#The Mape score for the COMB plot went down after removing multiplicative model!!!




