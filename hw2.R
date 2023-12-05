library(fpp2)

# 1
help(elecdaily)
elecdaily1 <- head(elecdaily, 31)
autoplot(elecdaily)

ggplot(as.data.frame(elecdaily1), aes(x=Temperature, y=Demand))+
  geom_point()+xlab('Degrees celsius')+ylab('Demand')

fit <- tslm(Demand~Temperature, data=elecdaily1)
fit

checkresiduals(fit)
# All these different lags are within the dotted lines -> no evidence there
# p-value is 0.3266, so not significant
# So, no evidence of autocorrelation

my.data <- data.frame(Temperature=c(15, 35))
my.data

forecast(fit, newdata=my.data)
# I will trust forecast the electricity demand if the temperature is 35 degrees.

ggplot(as.data.frame(elecdaily), aes(x=Temperature, y=Demand))+
  geom_point()+xlab('Degrees (celsius)')+ylab('Demand (GW per half hour)')
# The data spans roughly between 23 and 43
# If I'm looking to predict between 10 and 23 degrees, it might be wise to use different models.


# 2
help(shampoo)
shampoo

autoplot(shampoo)
# It's fluctuating, making it difficult for me to determine whether it exhibits seasonality or not.

fit.sham <- tslm(shampoo~trend)
fit.sham

checkresiduals(fit.sham)
# At the lag of 2, there seems to be a bit of autocorrelation
# p-value is 0.1769, so there isn't significant evidence at the 5% level or even a 10% level of autocorrelation.

ggplot(as.data.frame(cbind(res=residuals(fit.sham), fitted=fitted(fit.sham))), 
       aes(x=fitted, y=res))+
  geom_point()

fc.sham <- forecast(fit.sham, h=12)
fc.sham
autoplot(fc.sham)
# If the residuals aren't appearing normally distributed, especially with the left skewness, 
# it could impact the trustworthiness of prediction intervals.

fit.sham2 <- tslm(shampoo~trend+season)
fit.sham2
# Tells me how much I should expect the forecast variable to increase in each of these months with respect to month 1.
# Because most of these are negative, that means I should expect them to go down compared to month 1.

CV(fit.sham2)
CV(fit.sham)
# The original model has smaller AICc, which smaller is better.
# So, the message is, if we add all these dummy variables, it doesn't really give us a better model.
# We should just stick with the original model

# 3
autoplot(fancy)
# very strong seasonality
# the peak is in the end of each year
# also have an upward trend and heteroscedasticity, as the variance is increasing as time goes on.

# When I have variance that seems to depend on the level of the data, that's when it's appropriate to take algorithms.
# And also because it's financial data, that would be another reason why it's appropriate to take algorithms.

fancy2 <- log(fancy)
autoplot(fancy2)

festival <- cycle(fancy)==3
festival
festival[3] <- 0
festival

fit.surf <- tslm(fancy2~trend+season+festival)
fit.surf

checkresiduals(fit.surf)
# There's any heteroscedasticity, that's good.
# In ACF plot, the lags of 1, 2, and 3 months, got pretty high autocorrelation. (not great)
# Having autocorrelation, it doesn't mean the model is wrong, it just means the model could be better somehow.

ggplot(as.data.frame(cbind(res=residuals(fit.surf), fitted=fitted(fit.surf))), aes(x=fitted, y=res))+
  geom_point()
# I don't see any obvious correlation between the fitted values and the residuals. (that's good)

future.festival <- rep(0, 12)
future.festival

future.festival[3] <- 1
future.festival

new.data <- data.frame(festival=future.festival)
fc.surf <- forecast(fit.surf, newdata=new.data, h=12)
fc.surf
autoplot(fc.surf)

fc.surf2 <- exp(fc.surf$mean)
fc.surf2
autoplot(fancy)+autolayer(fc.surf2)

fit.surf2 <- tslm(fancy~trend+season+festival, lambda=0)
fit.surf2

fc.surf3 <- forecast(fit.surf2, newdata=new.data, h=12)
fc.surf3

# 4
writing
writing1 <- window(writing, end=c(1975, 12))
writing2 <- window(writing, start=c(1976, 1))
writing1
writing2
autoplot(writing1)

fit.writing <- tslm(writing1~trend+season)
fit.writing

checkresiduals(fit.writing)
# There's no heteroscedasticity observed in the plot of residuals against time.
# The presence of a few points just outside the blue dotted lines may not be a significant concern,
# especially given the non-significant p-value (0.2011) from the series test for autocorrelation.
# Additionally, the observation of residuals appearing somewhat normally distributed is a favorable aspect.

ggplot(as.data.frame(cbind(res=residuals(fit.writing), fitted=fitted(fit.writing))), aes(x=fitted, y=res))+
  geom_point()
# can't see any clear signs of a trend, so that's good.
# And that covers question (iii) and (iv), so no problems with the model.

fc.writing <- forecast(fit.writing, h=24)
fc.writing

autoplot(writing)+autolayer(fc.writing, PI=FALSE)
# The forecast looks accurate

accuracy(fc.writing, writing2)
sd(writing2)
# A test data size of 172, significantly larger than the RMSE of 51.82,
# indeed supports the notion of a robust forecast.
# It's a good forecast