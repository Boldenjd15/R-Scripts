Amtrak.data <- read.csv("C:\\Users\\jrmerric\\Dropbox\\Teaching\\Exec Ed\\Decision Analytics\\Forecasting\\2017\\Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

library("forecast")
library("forecastHybrid")

nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

par(mfrow = c(3, 1))

ESOpt <- ets(train.ts)
ESOpt.pred <- forecast(ESOpt, level=c(80,95), h = nValid)
plot(ESOpt.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1991,2004.25), main = "", flty = 2)
axis(1, at = seq(1991, 2004, 1), labels = format(seq(1991, 2004, 1)))
lines(ESOpt.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)


SARIMAOpt <- auto.arima(train.ts)
SARIMAOpt.pred <- forecast(SARIMAOpt, level=c(80,95), h=nValid)
plot(SARIMAOpt.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1991,2004.25), main = "", flty = 2)
axis(1, at = seq(1991, 2004, 1), labels = format(seq(1991, 2004, 1)))
lines(SARIMAOpt.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

ensembleForecast <- hybridModel(train.ts, models = "ae", weights = "cv.errors")
ensembleForecast.pred <- forecast(ensembleForecast, level=c(80,95), h=nValid)

plot(ensembleForecast.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1991,2004.25), main = "", flty = 2)
axis(1, at = seq(1991, 2004, 1), labels = format(seq(1991, 2004, 1)))
lines(ensembleForecast$fitted, lwd = 2, col = "blue")
lines(valid.ts)

accuracy(ESOpt.pred$mean, valid.ts)
accuracy(SARIMAOpt.pred$mean, valid.ts)
accuracy(ensembleForecast.pred$mean, valid.ts)

par(mfrow = c(1, 1))
ensembleForecast <- hybridModel(train.ts, models = "aenst", weights = "cv.errors")
plot(ensembleForecast, type = "fit")
plot(ensembleForecast, type = "models")

ensembleForecast.pred <- forecast(ensembleForecast, level=c(80,95), h=nValid)
accuracy(ESOpt.pred$mean, valid.ts)
accuracy(SARIMAOpt.pred$mean, valid.ts)
accuracy(ensembleForecast.pred$mean, valid.ts)

SARIMAOpt <- auto.arima(ridership.ts)
SARIMAOpt.pred <- forecast(SARIMAOpt, level=c(80,95), h=120)
plot(SARIMAOpt.pred, ylim = c(1300, 3500),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1991,2014.25), main = "", flty = 2)
SARIMAOpt.pred
SARIMAOpt




