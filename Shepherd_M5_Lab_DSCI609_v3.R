setwd("/Users/burrisfaculty/Desktop/DSCode/DSCI609")
library(dplyr)
library(hrbrthemes)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(plotly)
library(TSstudio)
library(forecast)
library(MLmetrics)
library(viridisLite)
library(viridis)
library(padr)
library(rmdformats)
library(recipes)
library(tseries)
crime_raw <- read.csv("Fouryears_all.csv")
#Create Subset of Data
crime_used <- crime_raw%>%
  select(Date,Primary.Type)%>%
  mutate(Date = ymd_hms(Date) ,
         Primary.Type = as.factor(Primary.Type))%>%
  mutate(Date = floor_date(Date, unit = 'hours'))%>%
  arrange(Date)%>%
  filter(Date >= "2019-09-01" & Date <= "2021-09-01")
  
tail(crime_used, 20)
head(crime_used,20 )


crime_group <- crime_used %>% group_by(Primary.Type)
crime_counts <- crime_group %>% summarise(Total = n())%>%
  arrange(-Total)
View(crime_counts)
#Bar Graph
#Full Count Data
ggplot(crime_counts, aes(x = reorder(Primary.Type, Total), y = Total)) + 
  labs(title = "Crime in Chicago", x = "Crime", y = "Number of Crimes")+
  geom_bar(fill = 'orange', color = 'black', stat = "identity") +
  theme_minimal()+
  coord_flip()
#Do top 5
plot_top5 <- crime_counts%>%
  head(5) %>%
  ggplot(aes(x = Total, y = reorder(Primary.Type, Total), text = paste("Crime: ", Primary.Type, "<br>",
                                                                       "Occurred: ", Total, " times")))+
  geom_col(aes(fill = Primary.Type), show.legend = FALSE)+
  scale_fill_viridis(option = "D", discrete = TRUE, direction = 1, begin = 0.9, end = 0.3)+
  labs(title = "Top 5 Crimes in Chicago", x = "Number of Occurences", y = "Crime")+
  theme_gray()
ggplotly(plot_top5,tooltip = 'text')
#Create Time Series Object
#Create Prediction Time Frame
#Crime of Concern: Theft
View(crime_used)
crime_used <- crime_used%>%ungroup()
crime_theft <- crime_used %>%
  filter(Primary.Type == "THEFT")%>%
  group_by(Date)%>%
  summarise(Theft = n())
View(crime_theft)
tail(crime_theft,10)
crime_theft <- crime_theft %>%
  pad(start_val = ymd_hms("2019-09-01 00:00:00"), end_val = ymd_hms("2021-09-01 00:00:00"))%>%
  mutate(Theft = replace_na(Theft, 0))
anyNA(crime_theft)

theft_train <- head(crime_theft,nrow(crime_theft)-760)

theft_test <- tail(crime_theft, 760)
theft_ts <- ts(crime_theft$Theft, frequency = 24)
theft_plot <- theft_train %>%ungroup()%>%
  ggplot(aes(x = Date, y = Theft))+
  geom_line(aes(color = Theft))+
  scale_x_datetime(name = "Date", date_breaks = waiver())+
  scale_y_continuous(breaks = seq(0,400,100))+
  theme_minimal()+
  labs(title = "Chicago Thefts", subtitle = "2019 - 2021")
ggplotly(theft_plot)
#Decompose
theft_ts_dec <- theft_ts %>%
  tail(760)%>%
  decompose()

theft_ts_dec %>%
  autoplot()
#Create Multi Seasonal Time Series
theft_multi <- msts(theft_train$Theft, seasonal.periods = c(24, #daily
                                                      24*7, #weekly
                                                      24*30)) #monthly
#Decompose MSTS Object
theft_multi_dec <- theft_multi%>%
  mstl()
theft_multi_dec %>%
  tail(760)%>%
  autoplot()

#Create Data Frame based on MSTS Object
df_theft_multi <- as.data.frame(theft_multi_dec)
plot1 <- df_theft_multi %>%
  mutate(day = theft_train$Date) %>%
  group_by(day)%>%
  summarise(seasonal = sum(Seasonal24 + Seasonal168 + Seasonal720)) %>%
  head(24*20) %>%
  ggplot(aes(x = day, y = seasonal), xlim = c(0,24)) +
  geom_point(col = "maroon") + geom_line(col = "red") + 
  theme_minimal()
  
plot1  

# 1. Daily Seasonaility
#Plots of Daily Seasonality of Thefts in Chicago
plot2 <- df_theft_multi %>%
  mutate(day = theft_train$Date, hour = hour(theft_train$Date))%>%
  group_by(hour) %>%
  summarise(seasonal = sum(Seasonal24 + Seasonal168 + Seasonal720)) %>%
  ggplot(aes(x = hour, y = seasonal), xlim = c(0,24)) +
  geom_point(col = "maroon") + geom_line(col = "red") +
  theme_minimal()
plot2
#Weekly Seasonality
#Which day of the week has the most thefts
plot3 <- df_theft_multi %>%
  mutate(day = theft_train$Date, week = weekdays(theft_train$Date)) %>%
  group_by(week) %>%
  summarise(seasonal = sum(Seasonal24 + Seasonal168 + Seasonal720)) %>%
  ggplot(aes(x = week, y = seasonal)) +
  geom_point() + geom_col() +
  theme_minimal()
plot3
#Monthly Seasonality
#Which months have the highest amount of thefts
plot4 <- df_theft_multi %>%
  mutate(day = theft_train$Date, month = month(theft_train$Date, label = T)) %>%
  group_by(month) %>%
  summarise(seasonal = sum(Seasonal24 + Seasonal168 + Seasonal720)) %>%
  ggplot(aes(x = month, y = seasonal)) + 
  geom_point() + geom_col() +
  theme_minimal()
plot4

#Check if the data is stationary

par(mfrow = c(1,2))
acf(theft_ts)
pacf(theft_ts)

par(mfrow = c(1,3))
ts.plot(theft_ts)
acf(theft_ts)
pacf(theft_ts)
#From the above graphs it appears that our data is not stationary.
#Use hypothesis testing to verify.
adf.test(theft_ts) #Reject H0 Time Series is Stationary
kpss.test(theft_ts, null = "Trend") #Reject H0 Time Series is not Stationary
kpss.test(theft_ts, null = "Level") #Reject H0

#Check to see number of differences to make the time series stationary
ndiffs(theft_ts, test = "adf") #Result 0 because the ADF test indicates stationary
ndiffs(theft_ts, test = "kpss") #Result 1, so we need 1 difference (linear) to make it stationary

theft_ts_stationary <- diff(theft_ts, differences = 1)
adf.test(theft_ts_stationary)
kpss.test(theft_ts_stationary, null = "Trend") #Now stationary because p-value greater than 0.05

#Modeling
#Multi Seasonal Holt_Winter (Exponential Smoothing)
theft_hw <-stlm(theft_multi, method = "ets")
#theft_hw
forecast_hw <- forecast(theft_hw, h = 365)

theft_multi %>%
  tail(365) %>%
  autoplot() +
  autolayer(forecast_hw, series = "Multi-Seasonal Holt Winter", color = "yellow")

#Time Series Model: Multi-Seasonal ARIMA
theft_arima <-stlm(theft_multi, method = "arima")
forecast_arima <- forecast(theft_arima, h = 365)
theft_multi %>%
  tail(365) %>%
  autoplot() +
  autolayer(forecast_arima, series = "Multi-Seasonal ARIMA", color = "blue")


#Time Series Model: Seasonal Naive Method
theft_snaive = snaive(y = theft_multi, h = 365)
forecast_snaive <- forecast(object = theft_snaive)
theft_multi %>%
  tail(365) %>%
  autoplot() +
  autolayer(forecast_snaive, series = "Seasonal Naive Method", color = "green")

#Compare which model is the best
accuracy(forecast_hw, theft_test$Theft)
accuracy(forecast_arima, theft_test$Theft)
accuracy(forecast_snaive,theft_test$Theft)
#Look for smallest errors... looks like arima is best for theft data
#Normality Test
ks.test(theft_arima$residuals, y = "pnorm", alternative = "two.sided")
#the residuals are not normally redistributed
Box.test(theft_arima$residuals, type = "Ljung-Box", lag = 2*365)
#There is autocorrelation in the residuals
#Save All Objects
save.image(file = "Crimedata.RData")


