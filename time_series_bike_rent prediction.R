# importing libraries
library(forecast)
library(fpp2)
library(dplyr)
library(tseries)


#set working directory
setwd('C:/Users/HP/Desktop/edu/Time-Series-Ins-PBA/Time-Series-Ins-PBA/data')

#imported the bike data
bike_data<-read.csv("bike_data.csv")
str(bike_data)
View(bike_data)

# Create a new Date column as import dteday data as Date variable
bike_data$Date = as.Date(bike_data$dteday)


#check outliers
#plot fraph between date and cnt column and draw geometric line
ggplot(bike_data, aes(Date, cnt)) + geom_line() 

#import using ts (time series function)
count_ts = ts(bike_data[, c('cnt')])
# clean the outliers using tsclean function
bike_data$clean_cnt = tsclean(count_ts)


# Calculate moving average and create a new column namd cnt_ma
bike_data$cnt_ma = ma(bike_data$clean_cnt, order=7)


#30 observation per month (na.omit removes the missing values)
count_ma = ts(na.omit(bike_data$cnt_ma), frequency=30)

#t.window s.window(seasonal window)
decomp = stl(count_ma, s.window="periodic")


plot(decomp)

# removing seasonality
deseasonal_cnt <- seasadj(decomp)

# auto arima function without seasonality
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
fit
fcast <- forecast(fit, h=30)
plot(fcast)

# autoarima function with seasonality
fit2<-auto.arima(deseasonal_cnt, seasonal=TRUE)
fit2
fcast2 <- forecast(fit2, h=30)
plot(fcast2)





