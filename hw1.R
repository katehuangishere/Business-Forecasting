library(fpp2)
library(forecast)
library(ggplot2)

# Exercise 1
help(WWWusage)
WWWusage

usagefc <- naive(WWWusage, h=20)
usagefc

# (i)
autoplot(usagefc)+autolayer(usagefc, series='Naive forecast')
  ggtitle('Internet usage per minute')+
  xlab('Minutes')+ylab('number of users connected to a server')

# (ii)
ggAcf(residuals(usagefc))+ggtitle('Correlogram of residuals')

# (iii)
usageres <- residuals(usagefc)
Box.test(usageres, type='L', lag=8)

# (iv)
# There is significant evidence to reject null hypothesis
# H0: there is no autocorrelation with lag 8
# No seasonality (not seasonal data)

# Exercise 2
help(woolyrnq)

woolyrnq
woofc <- snaive(woolyrnq, h=12)
woofc

# (i)
autoplot(woofc)+autolayer(woofc, series='Seasonal naive forecast')
  ggtitle('Quarterly production of woolen yarn in Australia')+
  xlab('year')+ylab('tonnes of woolen yarn production')

# (ii)
woores <- residuals(woofc)
ggAcf(woores)+ggtitle('Residuals for seasonal naive forecast for Quarterly production of woolen yarn in Australia')

# (iii)
Box.test(woores, type='L', lag=8)

# (iv)
# There is significant evidence to reject the null hypothesis
# H0: there is autocorrelation up to lag 8

# Exercise 3
help(ibmclose)

ibmfc <- meanf(ibmclose, h=30)
ibmfc

# (i)
autoplot(ibmfc)+autolayer(ibmfc, series='Mean forecast')+
  ggtitle('Closing price of IBM stock')+
  xlab('days')+ylab('stock price')

# (ii)
ibmres <- residuals(ibmfc)
ggAcf(ibmres)+ggtitle('Residuals for daily closing IBM stock price')

# (iii)
Box.test(ibmres, type='L', lag=8)

# (iv)
# There is significant evidence to reject the null hypothesis
# Ho: there is autocorrelation up to lag 8
# No seasonality

# (v)
# naive Model
ibmfc1 <- naive(ibmclose, h=30)
ggAcf(residuals(ibmfc1))
Box.test(residuals(ibmfc1), type='L', lag=8)

# (vi)
# ARIMA method is better than mean method
arima_model <- auto.arima(ibmclose)
ibmfc2 <- forecast(arima_model, h=30)
ibmfc2

autoplot(ibmfc2)+ggtitle('ARIMA forecast for daily closing IBM stock price')+
  xlab('day')+ylab('stock price')

ibmres2 <- residuals(ibmfc2)
ggAcf(ibmres2)+ggtitle('Residuals for daily closing IBM stock price')

Box.test(ibmres2, type='L', lag=8)

# Exercise 4
# seasonal naive method is not a good way to forecast sunspotarea series

help(sunspotarea)
sunspotarea

autoplot(sunspotarea)+
  ggtitle('Annual average suspot area')+
  xlab('year')+ylab('millionths of a hemisphere')

sunres <- residuals(sunfc)
ggAcf(sunres)+ggtitle('Residuals for seasonal naive forecast for annual average sunspot area between 1875 and 2015')

Box.test(sunres, type='L', lag=8)

# Exercise 5
help(mcopper)
mcopper

# (i)
autoplot(mcopper)+
  ggtitle('Monthly copper prices')+
  xlab('year')+ylab('pounds per ton')

# (ii)
lambda <- BoxCox.lambda(mcopper)
mcopper2 <- BoxCox(mcopper, lambda)
mcopperfc <- rwf(mcopper2, h=60, drift=TRUE)

autoplot(mcopper2)+autolayer(mcopperfc, series='Drift forecast')+
  ggtitle('Monthly copper prices')+
  xlab('Year')+ylab('pounds per ton')

# (iii)


# (iv)
mcopperres <- residuals(mcopperfc)
ggAcf(mcopperres)+ggtitle('Correlogram of residuals')

# (v)
Box.test(mcopperres, type="L", lag=8)

# (vi)
# There is significant evidence to reject the null hypothesis
# H0: there is autocorrelation up to lag 8
# maybe some seasonality

# Exercise 6
help(huron)
huron

# (i)
huron1 <- window(huron, end=1955)
huron2 <- window(huron, start=1956)
huron1
huron2

# (ii)
autoplot(huron1)+ggtitle('Lake Huron level between 1875 and 1955 (reduced by 570 feet)')+
  xlab('year')+ylab('feet')

autoplot(huron2)+ggtitle('Lake Huron level between 1956 and 1972 (reduced by 570 feet)')+
  xlab('year')+ylab('feet')

# (iii)
# Because the time series of lake level heights has a trend, 
# and seasonal naive forecasting may produce inaccurate predictions for time series with trends or other non-seasonal patterns.

# (iv)
huronfc1 <- naive(huron1, 17)
huronfc2 <- rwf(huron1, h=17, drift=TRUE)
huronfc3 <- meanf(huron1, 17)

autoplot(huron2)+
  autolayer(huronfc1, series = 'Naive forecast', PI=FALSE)+
  autolayer(huronfc2, series = 'Drift forecast', PI=FALSE)+
  autolayer(huronfc3, series = 'Mean forecast', PI=FALSE)+
  ggtitle('Lake Huron level between 1956 and 1972 (reduced by 570 feet)')+
  xlab('year')+ylab('feet')

accuracy(huronfc1, huron2)
accuracy(huronfc2, huron2)
accuracy(huronfc3, huron2)

# (v)
# drift

# (vi)
huronres <- residuals(huronfc3)
autoplot(huronres)+ggtitle('Residuals of mean forecast')+
  xlab('year')

huronres2 <- residuals(huronfc2)
autoplot(huronres2)+ggtitle('Residuals of drift method forecast')

# (vii)
# yes: uncorrelated
# yes: normally distributed
ggAcf(huronres)+ggtitle('Correlogram of residuals')
Box.test(huronres2, type="L", lag=8)

ggAcf(huronres2)+ggtitle('Correlogram of residuals')

gghistogram(huronres)+ggtitle('Histogram of residuals')
gghistogram(huronres2)+ggtitle('Histogram of residuals')

# (viii)
# I wouldn't change drift forecast
checkresiduals(huronres)



