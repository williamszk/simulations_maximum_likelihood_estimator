#190618 script
#preliminaries ----------------------------------------------------------------------------
#https://www.youtube.com/watch?v=3m6vH_1vMpc

library(quantmod)
library(rmgarch)


#
#example from quantmod getSymbols ---------------------------------------------------------
## Not run:
setSymbolLookup(QQQ='yahoo',SPY='google')
# loads QQQQ from yahoo (set with setSymbolLookup)
# loads SPY from MySQL (set with setSymbolLookup)
getSymbols(c('QQQ','SPY'))
# loads Ford market data from yahoo (the formal default)
getSymbols('F')
# loads symbol from MySQL database (set with setDefaults)
getSymbols('DIA', verbose=TRUE, src='MySQL')
# loads Ford as time series class ts
getSymbols('F',src='yahoo',return.class='ts')
# load into a new environment
data.env <- new.env()
getSymbols("YHOO", env=data.env)
ls.str(data.env)
# constrain to local scope
try(local( {
  getSymbols("AAPL") # or getSymbols("AAPL", env=environment())
  str(AAPL)
}))
exists("AAPL") # FALSE
# assign into an attached environment
attach(NULL, name="DATA.ENV")
getSymbols("AAPL", env=as.environment("DATA.ENV"))
ls("DATA.ENV")
detach("DATA.ENV")
# directly return to caller
str( getSymbols("AAPL", env=NULL) )
str( getSymbols("AAPL", auto.assign=FALSE) ) # same
## End(Not run)

#get data from facebook using quantmod -------------------------------------------------------
#use the ticker to download the series
fb=getSymbols('FB', auto.assign = FALSE)
class(fb)
chartSeries(fb)
chartSeries(fb["201906/"])
fbClose = fb$FB.Close
class(fbClose)
chartSeries(fbClose["201905/"])
fbOpen = fb$FB.Open
fb1 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                  mean.model = list(armaOrder=c(1,1)),
                  distribution.model = "std")
fbGarch1 <- ugarchfit(spec = fb1, data = fbClose)

fbPredict <- ugarchboot(fbGarch1,n.ahead = 10, method = c("Partial","Full")[1])
plot(fbPredict,which=2)

#
#finis -------------------------------------------------------------------------------------------











