#######################################################################################################################################
#Forecasting Methods HW#2 Question 3.10
#Jeremy Bolden
#Due Date: 10/23/19
#
#Question 3.10) This exercise uses the canadian_gas data (monthly Canadian gas production in billions of cubic metres, January
# 1960 – February 2005).
#
#A) Plot the data using autoplot(), gg_subseries() and gg_season() to look at the effect of the changing seasonality
# over time. What do you think is causing it to change so much?
#
#B) Do an STL decomposition of the data. You will need to choose a seasonal window to allow for the changing shape of
# the seasonal component.
#
#C) How does the seasonal shape change over time? [Hint: Try plotting the seasonal component using gg_season().]
#
#D) Can you produce a plausible seasonally adjusted series?
#
#E)Compare the results with those obtained using SEATS and X11. How are they different?
#######################################################################################################################################
#install packages to be called if not already done
library(fpp3)
library(tsibble)
library(ggplot2)
#starting with looking at the dataset canadian_gas
canadian_gas

#Part A)Plot the data using autoplot(), gg_subseries() and gg_season() to look at the effect of the changing seasonality
## over time. What do you think is causing it to change so much?

#Autoplot
  autoplot(canadian_gas)+
  labs(title = "Time series Analysis of Canadian Gas Consumption Over Time") +
  xlab(label = "Year")

#gg_season()
canadian_gas %>% gg_season(Volume, period = "year") #shortcut method
#Question - is this also the answer for part C?

#gg_subseries()
canadian_gas %>% gg_subseries(Volume) +
  ggtitle("Seasonal Plot of Volume") +
  xlab("Time") +
  ylab("Volume (m^3)")

#Answer - I believe that the production in gas in Canada fluctuates more based on the tempertaure/weather. During the
#warmer summer months, people are less likely to be heating their homes with gas and it's also offset by increased traveling.
#As you get closer to the end of the year, you begin to see a general rise again as the colder temperatures begin to close in. #Answer
###############################################################################################################################
#Part B) Do an STL decomposition of the data. You will need to choose a seasonal window to allow for the changing shape of
## the seasonal component.
recent_gas <- canadian_gas %>%
  filter(year(Month) >= 1990)
dcmp <- recent_gas %>%
  model(STL(Volume))
components(dcmp)
recent_gas %>%
  autoplot(Volume, color = 'gray') +
  autolayer(components(dcmp), trend, color = 'red') +
  xlab("Month") +
  ylab("Volume (m^3)") +
  ggtitle("Seasonal plot of amount of fuel produced after 1990")

#Plot all the components
components(dcmp) %>%
  autoplot() +
  xlab("Month")   #STL Decomp plots

#We can see the changing shape in the seasonality with the full dataset, so I only took the years from 1990 onward.
#^That's technically the answer the question, however I want to look deeper into the information and to practice

# Let's compare different window lengths
#making it fancy... enter whichever window sizes would like in the console.
window1 <- as.integer(readline(prompt="Enter Window Size 1 of 3: "))
window2 <- as.integer(readline(prompt="Enter Window Size 2 of 3: "))
window3 <- as.integer(readline(prompt="Enter Window Size 3 of 3: "))


gas_volume <- recent_gas %>%
  mutate(
    MA1 = slide_dbl(Volume, mean, .size = window1, .align = "center"),
    MA2 = slide_dbl(Volume, mean, .size = window2, .align = "center"),
    MA3 = slide_dbl(Volume, mean, .size = window3, .align = "center"))

gas_volume %>%
  autoplot(Volume) +
  autolayer(gas_volume, `MA1`, color='green') +
  autolayer(gas_volume, `MA2`, color = "red") +
  autolayer(gas_volume, `MA3`, color='blue')+
  xlab("Year") + ylab("z") + ggtitle("Total Canadian production") +
  guides(color=guide_legend(title="series"))

#Alternatively - changing the window size for the season portion of the STL model shows very interesting differences...
recent_gas %>%
  model(STL(Volume ~ trend(window=7) + season(window=1),
            robust = TRUE)) %>%
  components() %>%
  autoplot()

#For comparison, let's try window = default(21) for trend, and change the season to 9, 19 and periodic (inf).
recent_gas %>%
  model(STL(Volume ~ trend(window=21) + season(window=9),
            robust = TRUE)) %>%
  components() %>%
  autoplot()

recent_gas %>% #default window for trend is = 21
  model(STL(Volume ~ trend() + season(window=19),
            robust = TRUE)) %>%
  components() %>%
  autoplot()

recent_gas %>% #default window for trend is = 21
  model(STL(Volume ~ trend() + season(window='periodic'), #Periodic is the same as setting window = infinity
            robust = TRUE)) %>%
  components() %>%
  autoplot()

# Smaller .size or "window" values allow for more rapid changes in the plot. The higher the numbers, the less of a
# movement in the trend line. BOTH TREND AND SEASONAL WINDOWS SHOULD BE ODD NUMBER (according to the textbook)!!!
# Trend window is # the number of consecutive observations to be used when estimating the trend-cycle; season window
# is the number of # consecutive years to be used in estimating each value in the seasonal component. Setting the
# seasonal window to be # infinite is equivalent to forcing the seasonal component to be periodic
# season(window='periodic')(i.e., identical # across years). This results in a nearly uniform seaonality across the
# plot. When adjusting for a smaller sample size of the data, we do not have to change the window size as much Answer
############################################################
#Part C) How does the seasonal shape change over time? [Hint: Try plotting the seasonal component using gg_season().]
gas_seasonality <- gg_season(recent_gas)
gas_seasonality  #I don't this this is what he is looking for? same as ...
gg_season(recent_gas,Volume, period = "year")

#Let's break it down to just the seasonal component...
gas_season <- recent_gas %>%
  mutate(
   `12-MA` = slide_dbl(Volume, mean, .size = 12, .align = "cr"),
   `2x12-MA` = slide_dbl(`12-MA`, mean, .size = 2, .align = "cl")
  )

gas_season %>%
  autoplot(Volume, color='gray') +
  autolayer(gas_season, vars(`2x12-MA`), color='red') +
  xlab("Year") + ylab("Volume (m^3)") +
  ggtitle("Total Volume of Canadian Gas Production")

# Let's take the trend out
gas_season <- gas_season %>%
  mutate(
    trend = `2x12-MA`,
    detrend = Volume - trend
  )
#Plot detrended seasonality component
gas_season %>%
  autoplot(detrend) +
  xlab("Year") +
  ylab("Volume") +
  ggtitle("Detrended Total Volume of Gas Produced")
#detrended plot of the dataset.
#
# Answer - The plot shows us that between late 1970s and early 1990s, there was a greater variation in the amount of
# gas being produced during the higher and lower peaks and troughs of the year, with not much change in frequency.
# Around the mid 90s, we begin to see a slighly more consistent production variation, however the frequency of change in
# the inflection points on the graph increases significantly. This could easily mean that the production cycle is more
# closely in line with meeting the proper amount of demand for fuel consumption and not over or under producing fuel. #Answer
############################################################
#Part D)Can you produce a plausible seasonally adjusted series?
dcmp2 <- recent_gas %>%
  model(STL(Volume))
components(dcmp2)
recent_gas %>%
  autoplot(Volume, color = 'gray') +
  autolayer(components(dcmp2), trend, color = 'red') +
  xlab("Month") +
  ylab("Volume (m^3)") +
  ggtitle("Seasonal plot of amount of fuel produced")
#Answer - Given the detrended


############################################################
#Part E)Compare the results with those obtained using SEATS and X11. How are they different?
library(seasonal)
#For Seats -
# SEATS decomposition can be used with quarterly and monthly data

seats_dcmp <- recent_gas %>%
  model(seats = feasts:::SEATS(Volume)) %>%
  components()
autoplot(seats_dcmp) + xlab("Year") +
  ggtitle("SEATS decomposition of Total Gas Production in Canada")


#For X11 -
#install.packages("seasonal")     install the package if you have not already done so
x11_dcmp <- recent_gas %>%
  model (x11 = feasts:::X11(Volume, type = "additive")) %>%
  components()
autoplot(x11_dcmp) +
  xlab("Year") +
  ggtitle("X11 Decomp of Canadian Gas Production")

x11_dcmp %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Volume, color = "Data")) +
  geom_line(aes(y = season_adjust, color = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, color = "Trend")) +

  xlab("Year") + ylab("Volume (m^3)") +
  ggtitle("Total Volume in Canadian Gas Production") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))
x11_dcmp %>%
  gg_subseries(seasonal)
#The differeces are very minor when seeing the components from SEASTS and X11. We can see a slightly less linear
#increase with the trend line. There appears to be some of the residuals bleeding into the trend or seasonal portion of
#the decomp. That's not necessary a bad thing as we do not want to run into over-fitting of the model.




