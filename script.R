# time series forecasting with holt-winters
library(data.table)
library(jsonlite)
library(quantmod)
library(plotly)
library(htmlwidgets)
library(xts)

# crypto of choice
symbol <- "BTC"

# time interval
startTime <- as.numeric(as.POSIXct("2020-01-01 00:00:00")) * 1000
endTime <- as.numeric(as.POSIXct("2020-01-30 00:00:00")) * 1000

# url to retrieve data
endpoint <- paste0("https://api.binance.com/api/v3/klines?symbol=", symbol, "USDT&interval=1h&limit=20000&startTime=", startTime, "&endTime=", endTime)

# Download data
data_json <- fromJSON(endpoint)

# Convert the data into a data.table
data_dt <- as.data.table(data_json)
colnames(data_dt) <- c("Open time", "Open", "High", "Low", "Close", "Volume", "Close time", "Quote asset volume", "Number of trades", "Taker buy base asset volume", "Taker buy quote asset volume", "Ignore")

# convert times to proper format
data_dt[, c("Open time", "Close time") := lapply(.SD, function(x) as.POSIXct(as.numeric(x) / 1000, origin = "1970-01-01", tz = "GMT")), .SDcols = c("Open time", "Close time")]


# Save the data.table to a CSV file
fwrite(data_dt, paste0(symbol, "_hourly_price_history.csv"))

# dataframe
df <- as.data.frame(data_dt)

# format date and price values
df$'Close time' <- as.POSIXct(df$'Close time')
df$Close <- as.numeric(df$Close)

# time series
dfts <- as.xts(df$Close, order.by=df$'Close time')
jpeg('price.jpg',width=1500,height=1000)
plot(dfts, main="Hourly Close Prices from 2020-01-01",xlab="Time", ylab="Close Price")

# Estimated HoltWinters fitting
HW <- HoltWinters(dfts, gamma=FALSE)

# using forecast library
library(forecast)

HW_for <- forecast(HW, h=48, level=c(95))
jpeg('forecast.jpg',width=1000,height=800)
plot(HW_for)
lines(HW_for$fitted, lty=2, col="purple")