library(forecast)
require(quantmod)
getSymbols('AMZN', from = '2003-01-01')
amzn <- diff(log(Cl(AMZN)))

#we built a function to iterate and select the model with less AIC

azfinal.aic <- Inf
azfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for(q in 1:4) {
  azcurrent.aic <- AIC(arima(amzn, order = c(p,d,q)))
  if (azcurrent.aic < azfinal.aic) {
    azfinal.aic <- azcurrent.aic
    azfinal.order <- c(p,d,q)
    azfinal.arima <- arima(amzn, order = azfinal.order)
    }
}
azfinal.order
acf(resid(azfinal.arima), na.action=na.omit)
pacf(resid(azfinal.arima), na.action=na.omit)
Box.test(resid(azfinal.arima), lag=20, type="Ljung-Box") #to see if we have a good fit (Ho: no autocorrelation)
plot(forecast(azfinal.arima, h=25))


#Now we do the same for S&P index
getSymbols("^GSPC", from="2013-01-01")
sp = diff(log(Cl(GSPC)))
plot(sp)

spfinal.aic <- Inf
spfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
  spcurrent.aic <- AIC(arima(sp, order=c(p, d, q)))
 if (spcurrent.aic < spfinal.aic) {
    spfinal.aic <- spcurrent.aic
    spfinal.order <- c(p, d, q)
    spfinal.arima <- arima(sp, order=spfinal.order)
    }
}
