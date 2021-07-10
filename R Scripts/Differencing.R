library("fpp3")

google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG") %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)

# Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)

# Data with trend and/or seasonality are called non-stationary
google_2015 %>% autoplot(Close)

google_2015 %>% ACF(Close) %>% autoplot()

google_2015 %>%
  features(Close, ljung_box, lag = 10)

# We can use differencing to make the resulting series stationary
google_2015 %>%
  mutate(diff_close = difference(Close)) -> google_2015
  
google_2015 %>% autoplot(diff_close)

google_2015 %>% ACF(diff_close) %>% autoplot()

google_2015 %>%
  features(diff_close, ljung_box, lag = 10)

# The Kwiatkowski, Phillips, Schmidt, & Shin (KPSS) test tells us if we have non-stationary data. 
# The goog data fails
google_2015 %>%
  features(Close, unitroot_kpss)

# but the differenced data passes
google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, unitroot_kpss)

# unitroot_ndiffs tells us whether we need single or double differencing at lag 1 to pass the KPSS test
google_2015 %>%
  features(Close, unitroot_ndiffs)

# Sometimes data has a seasonal trend
aus_total_retail <- aus_retail %>%
  summarise(Turnover = sum(Turnover)) %>%
  mutate(log_turnover = log(Turnover))

aus_total_retail %>% autoplot(log_turnover)

# unitroot_nsdiffs tells us whether we need single or double seasonal differencing to pass the KPSS test
aus_total_retail %>%
  features(log_turnover, unitroot_nsdiffs)

aus_total_retail <- aus_total_retail %>%
  mutate(diff12_log_turnover = difference(log_turnover, 12))

aus_total_retail %>% autoplot(diff12_log_turnover)

# unitroot_nsdiffs tells us we need single seasonal differencing, so we then apply ndiffs to the seasonally differenced data to see what lag 1 differencing we need
aus_total_retail %>%
  features(diff12_log_turnover, unitroot_ndiffs)

aus_total_retail <- aus_total_retail %>%
  mutate(diff1_diff12_log_turnover = difference(diff12_log_turnover, 1))

aus_total_retail %>% autoplot(diff1_diff12_log_turnover)

