#190617 zivot

library(xts)
library(forecast)
library(quantmod)
library(rugarch)

#use data from tsay
ts_tsay <- read.csv("C:/Users/willi/Desktop/working/RAW_DATA/tsay/m-intc7308.txt", sep="")

apply(ts_tsay,2,class)

attach(ts_tsay)

date = as.character(date)

date2 = paste(substr(date,1,4), substr(date,5,6), substr(date,7,8), sep='-')
date3 = as.Date(date2)

ts2 = xts(x=rtn, order.by = date3)
plot(ts2)

#build the index price for the ts2 series
price_index = cumsum(rtn)
ts3 = xts(x=price_index, order.by = date3[])
plot(ts3)

#apply a GARCH model in the series ts2

























