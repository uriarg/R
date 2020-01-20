## The object of this notebook is to implement ARIMA+GARCH trading strategy for the S&P 500

# See: https://www.quantstart.com/articles/ARIMA-GARCH-Trading-Strategy-on-the-SP500-Stock-Market-Index-Using-R/

#The strategy is carried out on a "rolling" basis:
  
# 1) For each day, n, the previous k days of the differenced logarithmic returns of a stock market index are 
#   used as a window for fitting an optimal ARIMA and GARCH model.

#2) The combined model is used to make a prediction for the next day returns.

# 3) If the prediction is negative the stock is shorted at the previous close, while if it is positive it is longed.

# 4) If the prediction is the same direction as the previous day then nothing is changed.

install.packages("quantmod")
install.packages("lattice")
install.packages("timeSeries")
install.packages("rugarch")

library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)

getSymbols("^GSPC", from="2005-01-01") #download data for S&P
spReturns = diff(log(Cl(GSPC))) #Compute differenced log returns (making it stationary) 
spReturns[as.character(head(index(Cl(GSPC)),1))] = 0
plot(spReturns)

# We need to create a vector, forecasts to store our forecast values on particular dates. 
# We set the length foreLength to be equal to the length of trading data we have minus k, the window length:

windowLength = 500
foreLength = length(spReturns) - windowLength
forecasts <- vector(mode="character", length=foreLength)

# At this stage we need to loop through every day in the trading data and fit an appropriate ARIMA and GARCH model to the rolling window of length k. 

#this loop states that if we find a model p=0,q=0 we dont want it. Also note that we are
# fitting an ARMA structure given our data (i.e. S&P500 returns) is already differenced

for (d in 0:foreLength) {
  spReturnsOffset = spReturns[(1+d):(windowLength+d)]
  final.aic <- Inf #the function Inf eturn a vector of the same length as x, indicating which elements are finite or infinite. 
  final.order <- c(0,0,0)
  for (p in 0:5) for (q in 0:5) {  
    if ( p == 0 && q == 0) {
      next
    }
    arimaFit = tryCatch( arima(spReturnsOffset, order=c(p, 0, q)), 
                         error=function( err ) FALSE,
                         warning=function( err ) FALSE )
    
#This part of the function selects the optimal order by selecting the minimal AIC
                             
    if( !is.logical( arimaFit ) ) {
      current.aic <- AIC(arimaFit)
      if (current.aic < final.aic) {
        final.aic <- current.aic
        final.order <- c(p, 0, q)
        final.arima <- arima(spReturnsOffset, order=final.order)
      }
    } else { next 
      }
  }
  
  
  # In the next block of code we use rugarch library with a GARCH(1,1) model, the syntax 
  # requires to set a ugarchspec specification object that takes a model for the mean and the variance
  # the variance receive a GARCH(1,1) while the mean the ARMA(p,q) choosed above
  # We also choose the sged distribution for the errors.
  
 # Once we have chosen the specification we carry out the actual fitting of ARMA+GARCH using the ugarchfit command, 
 # which takes the specification object, the k returns of the S&P500 and a numerical optimisation solver. 
 # We have chosen to use 'hybrid', which tries different solvers in order to increase the likelihood of convergence:
 
  spec = ugarchspec(
    variance.model=list(garchOrder=c(1,1)),
    mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
    distribution.model="sged")       #The spec of ugarchfit function is set, see: ?ugarchspec
  fit = tryCatch(
    ugarchfit(
      spec, spReturnsOffset, solver = 'hybrid'
    ), error=function(e) e, warning=function(w) w
  )
  
# If the GARCH model does not converge then we simply set the day to produce a "long" prediction, which is clearly a guess.
# However, if the model does converge then we output the date and tomorrow's prediction direction (+1 or -1) 
# as a string at which point the loop is closed off.
# 
# In order to prepare the output for the CSV file I have created a string that contains the data 
# separated by a comma with the forecast direction for the subsequent day:
  
  if(is(fit, "warning")) {
    forecasts[d+1] = paste(index(spReturnsOffset[windowLength]), 1, sep=",")
    print(paste(index(spReturnsOffset[windowLength]), 1, sep=","))
  } else {
    fore = ugarchforecast(fit, n.ahead=1)
    ind = fore@forecast$seriesFor
    forecasts[d+1] = paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")
    print(paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=","))
  }
}

# The penultimate step is to output the CSV file to disk. 
# This allows us to take the indicator and use it in alternative backtesting software for further analysis, if so desired:

write.csv(forecasts, file="forecasts_test.csv", row.names=FALSE)  
  
# However, there is a small problem with the CSV file as it stands right now. 
# The file contains a list of dates and a prediction for tomorrow's direction.
# If we were to load this into the backtest code below as it stands, we would actually be 
# introducing a look-ahead bias because the prediction value would represent data not known at the time of the prediction.
# 
# In order to account for this we simply need to move the predicted value one day ahead. 
# I have found this to be more straightforward using Python. 
# Since I don't want to assume that you've installed any special libraries (such as pandas), I've kept it to pure Python.  
#   See the file in the same folder of the script and also the python code. 

#Now we use the python cleaned file: 

spArimaGarch = as.xts( 
  read.zoo(
    file="forecasts_new1.csv", format="%Y-%m-%d", header=F, sep=","
  )
)

# info about xts objects: https://www.datacamp.com/community/blog/r-xts-cheat-sheet

# We then create an intersection of the dates for the ARIMA+GARCH forecasts and the original set of returns from the S&P500.
# We can then calculate the returns for the ARIMA+GARCH strategy by multiplying the forecast sign (+ or -) with the return itself:

spIntersect = merge( spArimaGarch[,1], spReturns, all=F )
spArimaGarchReturns = spIntersect[,1] * spIntersect[,2]   

# Once we have the returns from the ARIMA+GARCH strategy we can create 
# equity curves for both the ARIMA+GARCH model and "Buy & Hold".
# Finally, we combine them into a single data structure:
  
spArimaGarchCurve = log( cumprod( 1 + spArimaGarchReturns ) )
spBuyHoldCurve = log( cumprod( 1 + spIntersect[,2] ) )
spCombinedCurve = merge( spArimaGarchCurve, spBuyHoldCurve, all=F )

#Finally, we can use the xyplot command to plot both equity curves on the same plot:

xyplot( 
  spCombinedCurve,
  superpose=T,
  col=c("darkred", "darkblue"),
  lwd=2,
  key=list( 
    text=list(
      c("ARIMA+GARCH", "Buy & Hold")
    ),
    lines=list(
      lwd=2, col=c("darkred", "darkblue")
    )
  )
)













  
  
  
