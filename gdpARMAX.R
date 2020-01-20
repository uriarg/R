# Estimate a an ARMAX model for GDP growth
# Try adding the 10yr-3m treasury spread
# This program does a few interesting things
#    1) Puts data and lags into giant time series to keep sanity
#    2) Runs casual regression with lm() first

library(forecast)


# load US real GDP (source FRED)
gdp.data <- read.csv("USRealGDP.csv")
# Convert to TS, note this is quarterly 
gdp.ts <- ts(gdp.data$GDPC1,start=c(1947,1),freq=4)


# read 10 yr 
tb10 <-  read.csv("USTreasury10.csv")
tb3m  <- read.csv("USTreasury3M.csv")

# set up time series on 10 year series (use starting dates from excel file)
tb10.ts <- ts(tb10$GS10,start=c(1953,4),freq=12)
# shift to quarterly data: This works in R
tb10.ts <- window(tb10.ts,freq=4)
# repeat this for 3 month yield
tb3m.ts  <- ts(tb3m$TB3MS,start=c(1934,1),freq=12)
tb3m.ts <- window(tb3m.ts,freq=4)


# GDP growth rate
gdpg.ts <- diff(log(gdp.ts),1)
# Term structure slope
spread.ts <- tb10.ts - tb3m.ts

# bind all series together, and save to look at for example
# Note:  R will correctly line up dates, take a look
# Also, note use of lag function
allraw.ts <- cbind(gdp.ts,gdpg.ts,tb10.ts,tb3m.ts,lag(spread.ts,-1),
                  lag(gdpg.ts,-1))
# Set window with available data
all.ts <- window( allraw.ts, start=c(1953,3),end=(c(2019,2)))
# Set column names
colnames(all.ts) <- cbind("gdp","gdpg","tb10","tb3m","lagspread","laggdpg")


# Run casual regressions to show how this could be done
test.mod <- lm( gdpg ~ laggdpg, data=all.ts)
test2.mod <- lm( gdpg ~ laggdpg + lagspread, data=all.ts)

print(summary(test.mod))
print(summary(test2.mod))



T <- length(all.ts)

# set up training and validation periods
alltrain.ts <-   window(all.ts,end=c(1999,4))
allvalid.ts <-   window(all.ts,start=c(2000,1))

# find validation length (need to do this on one series)
# See use of named column value
nValid <- length(allvalid.ts[,"gdpg"])
# naive forecast = training sample mean
naive <- rep(mean(alltrain.ts[,"gdpg"]),nValid)


# full sample AR(1) + spread
fulldata.mod <- Arima(all.ts[,"gdpg"],order=c(1,0,0),xreg=all.ts[,"lagspread"])

# now w/o spread
fulldataNoSpread.mod <- Arima(all.ts[,"gdpg"],order=c(1,0,0))

# estimate on training data
traindata.mod <- Arima(alltrain.ts[,"gdpg"],order=c(1,0,0),
                       xreg=alltrain.ts[,"lagspread"])

# again, training data, but no spreaad
traindataNoSpread.mod <- Arima(alltrain.ts[,"gdpg"],order=c(1,0,0))

# one step ahead prediction in validation period ( 3 cases)
# train data model, with spread
onestep.mod <- Arima(allvalid.ts[,"gdpg"],
                     xreg=allvalid.ts[,"lagspread"],model=traindata.mod)
# train data model, with no spread
onestepNoSpread.mod <- Arima(allvalid.ts[,"gdpg"],model=traindataNoSpread.mod)
# full data, spread
onestepfulldata.mod <- Arima(allvalid.ts[,"gdpg"],
                      xreg = allvalid.ts[,"lagspread"],model=fulldata.mod)
# full data, no spread
onestepfulldataNoSpread.mod <- Arima(allvalid.ts[,"gdpg"],
                             ,model=fulldataNoSpread.mod)


plot(all.ts[,"gdpg"])
lines(fitted(traindata.mod),col="red")
lines(fitted(onestep.mod),col="blue")
grid()

# training comparison
print("Estimation: Train, Errors: train, Model: spread")
print(accuracy(traindata.mod))
print("Estimation: Train, Errors: train, Model: no spread")
print(accuracy(traindataNoSpread.mod))

# Validation comparison
# Training sample model estimation
print("Estimation: Train, Errors: validation, Model: spread")
print(accuracy(onestep.mod))
print("Estimation: Train, Errors: validation, Model: no spread")
print(accuracy(onestepNoSpread.mod))

# full sample model
print("Estimation: Full, Errors: validation, Model: spread")
print(accuracy(onestepfulldata.mod))
print("Estimation: Full, Errors: validation, Model: no spread")
print(accuracy(onestepfulldataNoSpread.mod))

# naive
print("Estimation: Train, Errors: validation, Model: naive")
print(accuracy(allvalid.ts[,"gdpg"],naive))



