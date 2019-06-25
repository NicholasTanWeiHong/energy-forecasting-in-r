## ------------------------------------------------------------------------
# Import key libraries
library(knitr)
library(tidyverse)
library(Quandl)

## ------------------------------------------------------------------------
# Query ICE Brent Contract prices from Quandl
crude <- read_csv("https://www.quandl.com/api/v3/datasets/CHRIS/ICE_B1.csv?api_key=rn2xyN_hG9XfxN_9ibFJ"
)

# View  the structure crude data.frame
str(crude)

# View the first few rows of the crude data.frame
head(crude)

# View the last few rows of the crude data.frame
tail(crude)

## ------------------------------------------------------------------------
# Convert the crude data.frame to an xts object
crude_xts <- xts(crude[,-1], order.by = as.POSIXct(crude$Date))

head(crude_xts)


## ------------------------------------------------------------------------
# Plot a time series of Brent Crude Oil prices
autoplot(crude_xts$Settle) +
  labs(title = "Historical Prices of Brent Crude Oil", subtitle = "From 1993-03-17 to 2019-06-07") +
  xlab("Date") +
  ylab("Price")


## ------------------------------------------------------------------------
# Generate a series of returns with diff()
crude_returns <- diff(crude_xts) / crude_xts[-length(crude_xts)]
head(crude_returns)

## ------------------------------------------------------------------------
# Plot a Histogram of Returns
ggplot(data = crude_returns, mapping = aes(x = Settle)) +
  geom_histogram(bins = 50) +
  labs(title = "Histogram of Brent Crude Oil Returns", subtitle = "From 1993-03-17 to 2019-06-07") +
  xlab("Brent Crude Oil Returns") +
  ylab("Frequency of Returns")


## ------------------------------------------------------------------------
# Group by Year and Plot a series of Boxplots
crude %>% 
  mutate(year = format(Date, "%Y"), returns = (Settle / lead(Settle) - 1)) %>% 
  group_by(year) %>% 
  ggplot(mapping = aes(x = year, y = returns, fill = year)) +
  geom_boxplot() +
  labs(title = "Boxplot of Historical Brent Crude Oil Returns", subtitle = "From 1993-03-17 to 2019-06-07") +
  xlab("Year") +
  ylab("Returns") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, size = 8))


## ------------------------------------------------------------------------
# Plot a QQ PLot of Returns
crude_returns %>% 
  ggplot(mapping = aes(sample = Settle)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Quantile-Quantile Plot of Brent Crude Oil Returns", subtitle = "From 1993-03-17 to 2019-06-07") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")


## ------------------------------------------------------------------------
library(forecast)

# Plot the ACF function of Brent Crude Oil with ggAcf
ggAcf(crude_xts$Settle) +
  labs(title = "ACF of Brent Crude Oil", subtitle = "From 1993-03-17 to 2019-06-07")
# Plot the PACF function of Brent Crude Oil with ggPacf
ggPacf(crude_xts$Settle) +
  labs(title = "PACF of Brent Crude Oil", subtitle = "From 1993-03-17 to 2019-06-07")


## ------------------------------------------------------------------------
# Fit an ARIMA model to the time series with auto.arima()
arima_fit <- auto.arima(crude_xts$Settle)

# Plot the forecasted prices of Brent Crude from the ARIMA model
arima_fit %>% 
  forecast() %>% 
  autoplot()


## ------------------------------------------------------------------------
# Generate a summary of the ARIMA model
summary(arima_fit)

## ------------------------------------------------------------------------
# Analyse the residuals of the ARIMA model
checkresiduals(arima_fit)

## ------------------------------------------------------------------------
# Print the accuracy of the ARIMA model
accuracy(arima_fit)


## ------------------------------------------------------------------------
# Fit an 'Error, Trend, Seasonality' model to Brent Crude Oil time series
ets_fit <- ets(crude_xts$Settle)

# Plot forecasts made by the ETS model
ets_fit %>% 
  forecast() %>% 
  autoplot()


## ------------------------------------------------------------------------
# Print a summary of the ETS Model
summary(ets_fit)

## ------------------------------------------------------------------------
# Analyse the residuals of the ETS Model
checkresiduals(ets_fit)

## ------------------------------------------------------------------------
# Prin the accuracy of the ETS model
accuracy(ets_fit)

## ------------------------------------------------------------------------
# Export the analysis to an R Script
purl("energy-forecasting-in-r.Rmd")

