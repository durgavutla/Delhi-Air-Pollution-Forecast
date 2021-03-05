# Delhi pollution forecasting


library(rmarkdown)
library(forecast)
library(fpp)
library(smooth)
library(readxl)



Airpollution <- read.csv(file.choose())

View(Airpollution)

# Data Cleaning
Airpollution <- Airpollution[-c(3, 4)]

Airpollution <- Airpollution[-c(1),]


colnames(Airpollution) = Airpollution[1, ] # the first row will be the header
Airpollution = Airpollution[-1, ]
View(Airpollution)

datetime <- Airpollution$date
a <- strptime(datetime, "%m/%d/%y %H:%M")
a
b <- strptime(datetime, "%m-%d-%Y %H:%M")
b
a[is.na(a)] <- b[!is.na(b)]
Airpollution$date <- a
Airpollution$date

str(Airpollution)

Airpollution$pm25 <- as.numeric(Airpollution$pm25)

View(Airpollution)

# sort from least recent date to most recent

AP <- Airpollution[order(Airpollution$date, decreasing = FALSE),]


View(AP)

str(AP)

plot(AP$pm25, type="o")

str(Airpollution)

# Missing values
sum(is.na(AP$pm25)) # 80 missing values

summary(AP)

#Simple imputation
install.packages("imputeTS")
library(imputeTS)
#AP$pm25 <-na_interpolation(Airpollution$pm25) 
#na_interpolation(Airpollution$pm25)
# Output is the time series with all NA's replaced by reasonable values.

AP$pm25 <- na_ma(AP$pm25, weighting = "simple")

#Airpollution$pm25 <- na_interpolation(Airpollution$pm25)
View(AP)
summary(AP)

install.packages("table1")
library(table1)
table1::label(AP$pm25) <- "Particulate Matter"
table1::table1(~pm25, data = AP)

############
#plot
ggplot_na_distribution(AP$pm25)
plot(AP$pm25, type="o")

# Changing the character variable into Date 
n <- AP
View(n)
x <- as.factor(n$date)
abis <- strptime(x, format = "%Y-%m-%d %H:%M")
n$date <- as.Date(abis, format = "%Y-%m-%d %H:%M")
View(n)

# boxplot for daily data
boxplot(n$pm25 ~ n$date)
# we see the outliers in the data

# boxplot for weekly data
library(ggplot2)
library(lubridate)

ggplot(n, aes(x=as.Date(n$date), y=n$pm25, group=ceiling_date(n$date, "week"))) + geom_boxplot() + scale_x_date(date_breaks = "1 week", date_labels="%Y-%m-%d")
# From the plot, we find that there are too many outliers or high unexpected values.

mean(AP$pm25)

install.packages("tidyquant")
library(tidyquant)
dfweek <- n %>% tq_transmute(mutate_fun = apply.weekly, FUN = mean, na.rm=TRUE)

dfweek

plot(dfweek, type = "o")

#####################

# Store the data in a time series format
# Decompose of time series to see trend and seasonality 
library(lubridate)
AP$date <- ymd_hms(AP$date)
ts_train <- AP$pm25 %>% ts(freq = 24)

ts_train %>%
  tail(24*7*4) %>%
  decompose() %>%
  autoplot()

# to see the complex seasonality using msts()
msts_pm25 <- AP$pm25 %>% msts(seasonal.periods = c(24, 24*7))
msts_pm25 %>% head(24*7*4) %>% mstl() %>% autoplot()



# Creating dummy variables
library(dplyr)
df <- tibble::tibble(time = AP$date)

suppressPackageStartupMessages(library(dplyr))
df_dummy <- df %>% 
  mutate(
    hours = lubridate::hour(time),
    dummy = 1)

tidyr::pivot_wider(data = df_dummy, names_from = hours, values_from = dummy, values_fill = list(dummy = 0))

df1 <- tidyr::pivot_wider(data = df_dummy, names_from = hours, values_from = dummy, values_fill = list(dummy = 0))

View(df1)
attach(AP)
AP1 <- cbind(df1,pm25)
View(AP1)

#input t
AP1["t"] <- c(1:2374)
View(AP1)

AP1["log_pm25"] <- log(AP1["pm25"])
AP1["t_square"] <- AP1["t"]*AP1["t"]
View(AP1)


##Data Partition
training <- AP1[1:1662,]
testing <- AP1[1663:2374,]

attach(AP1)


####Linear Model####

linearmodel <- lm(AP1$pm25~AP1$t, data = training)
summary(linearmodel)
linearpred <- data.frame(predict(linearmodel, interval = 'predict', newdata = testing))
View(linearpred)
rmselinear <- sqrt(mean((AP1$pm25-linearpred$fit)^2, na.rm = T))
rmselinear



####Exponential#####
expomodel <- lm(AP1$log_pm25~AP1$t, data = training)
summary(expomodel)
expopred <- data.frame(predict(expomodel, interval = 'predict', newdata = testing))
rmseexpo <- sqrt(mean((AP1$pm25-exp(expopred$fit))^2, na.rm = T))
rmseexpo

####Quadratic#####
quadmodel <- lm(AP1$pm25~AP1$t+AP1$t_square, data = training)
summary(quadmodel)
quadpred <- data.frame(predict(quadmodel, interval = 'predict', newdata = testing))
rmsequad <- sqrt(mean((AP1$pm25-quadpred$fit)^2, na.rm = T))
rmsequad

colnames(AP1)

####Additive Seasonality####
AP1$"0"
seaaddmodel <- lm(AP1$pm25~AP1$"0"+AP1$"1"+AP1$"2"+AP1$"3"+AP1$"4"+AP1$"5"+AP1$"6"+AP1$"7"+AP1$"8"+AP1$"9"+AP1$"10"+AP1$"11"+AP1$"12"+AP1$"13"+AP1$"14"+AP1$"15"+AP1$"16"+AP1$"17"+AP1$"18"+AP1$"19"+AP1$"20"+AP1$"21"+AP1$"22"+AP1$"23", data = training)
summary(seaaddmodel)
seaaddpred <- data.frame(predict(seaaddmodel, interval = 'predict', newdata = testing))
rmseseaadd <- sqrt(mean((AP1$pm25-seaaddpred$fit)^2, na.rm = T))
rmseseaadd 

####Additive Seasonality with Linear####
seaaddmodellinear <- lm(AP1$pm25~AP1$t+AP1$"0"+AP1$"1"+AP1$"2"+AP1$"3"+AP1$"4"+AP1$"5"+AP1$"6"+AP1$"7"+AP1$"8"+AP1$"9"+AP1$"10"+AP1$"11"+AP1$"12"+AP1$"13"+AP1$"14"+AP1$"15"+AP1$"16"+AP1$"17"+AP1$"18"+AP1$"19"+AP1$"20"+AP1$"21"+AP1$"22"+AP1$"23", data = training)
summary(seaaddmodellinear)
seaaddlinearpred <- data.frame(predict(seaaddmodellinear, interval = 'predict', newdata = testing))
rmseseaaddlinear <- sqrt(mean((AP1$pm25-seaaddlinearpred$fit)^2, na.rm = T))
rmseseaaddlinear 

####Additive Seasonality with Quadratic####
seaaddmodelquad <- lm(AP1$pm25~AP1$t+AP1$t_square+AP1$"0"+AP1$"1"+AP1$"2"+AP1$"3"+AP1$"4"+AP1$"5"+AP1$"6"+AP1$"7"+AP1$"8"+AP1$"9"+AP1$"10"+AP1$"11"+AP1$"12"+AP1$"13"+AP1$"14"+AP1$"15"+AP1$"16"+AP1$"17"+AP1$"18"+AP1$"19"+AP1$"20"+AP1$"21"+AP1$"22"+AP1$"23", data = training)
summary(seaaddmodelquad)
seaaddquadpred <- data.frame(predict(seaaddmodelquad, interval = 'predict', newdata = testing))
rmseseaaddquad <- sqrt(mean((AP1$pm25-seaaddquadpred$fit)^2, na.rm = T))
rmseseaaddquad 

####Multiplicative Seasonality####
multiseamodel <- lm(AP1$log_pm25~AP1$"0"+AP1$"1"+AP1$"2"+AP1$"3"+AP1$"4"+AP1$"5"+AP1$"6"+AP1$"7"+AP1$"8"+AP1$"9"+AP1$"10"+AP1$"11"+AP1$"12"+AP1$"13"+AP1$"14"+AP1$"15"+AP1$"16"+AP1$"17"+AP1$"18"+AP1$"19"+AP1$"20"+AP1$"21"+AP1$"22"+AP1$"23", data = training)
summary(multiseamodel)
multiseapred <- data.frame(predict(multiseamodel, interval = 'predict', newdata = testing))
rmsemultisea <- sqrt(mean((AP1$pm25-exp(multiseapred$fit))^2, na.rm = T))
rmsemultisea


####Multiplicative Seasonality Linear trend####
multiseaaddmodel <- lm(AP1$log_pm25~AP1$t+AP1$"0"+AP1$"1"+AP1$"2"+AP1$"3"+AP1$"4"+AP1$"5"+AP1$"6"+AP1$"7"+AP1$"8"+AP1$"9"+AP1$"10"+AP1$"11"+AP1$"12"+AP1$"13"+AP1$"14"+AP1$"15"+AP1$"16"+AP1$"17"+AP1$"18"+AP1$"19"+AP1$"20"+AP1$"21"+AP1$"22"+AP1$"23", data = training)
summary(multiseaaddmodel)
multiseaaddpred <- data.frame(predict(multiseaaddmodel, interval = 'predict', newdata = testing))
rmsemultiseaadd <- sqrt(mean((AP1$pm25-exp(multiseaaddpred$fit))^2, na.rm = T))
rmsemultiseaadd 

# Preparing table on model and it's RMSE values
table_rmse <- data.frame(c("rmselinear", "rmseexpo", "rmsequad", "rmseseaadd", "rmseseaaddlinear", "rmseseaaddquad", "rmsemultisea", "rmsemultiseaadd"),c(rmselinear, rmseexpo, rmsequad, rmseseaadd, rmseseaaddlinear, rmseseaaddquad, rmsemultisea, rmsemultiseaadd))
View(table_rmse)

colnames(table_rmse) <- c("model", "RMSE")
View(table_rmse)

# among all the models 
# we see that additive seasonality with quadratic has least RMSE value



#####heat map #########
install.packages('plyr')
library(plyr)
library(dplyr)
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
library(viridis)
library(scales)
library(tidyr)

sum(is.na(AP$pm25))

####

# Splitting date time

df$Time <- format(as.POSIXct(AP$date,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")

df$Date <- format(as.POSIXct(AP$date,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")

df$day <- format(as.POSIXct(AP$date,format="%Y:%m:%d %H:%M:%S"),"%d")

df$year <- format(as.POSIXct(AP$date,format="%Y:%m:%d %H:%M:%S"),"%Y")

df$month <- format(as.POSIXct(AP$date,format="%Y:%m:%d %H:%M:%S"),"%m")

df$hour <- format(as.POSIXct(AP$date,format="%Y:%m:%d %H:%M:%S"),"%H")


df2 <- cbind(df$hour, df$day, df$month, AP$pm25)
df2
df2 <- as.data.frame(df2)
View(df2)
df2$V3 <- as.numeric(df2$V3)
df2$V4 <- as.numeric(df2$V4)
str(df2)
attach(df2)

Jan <- df2[df2$V3<02,]
V3

View(Jan)

a <- ggplot(Jan , aes(x=Jan$V1, y=Jan$V2, fill = Jan$V4)) + 
  geom_tile(color = "white", size = 0.1) + 
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  scale_fill_viridis(name="pm25", option = "plasma") + 
  coord_equal() + 
  labs(x="hour", y="day", title="Jan") + 
  theme_tufte(base_family="Helvetica") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

a
Feb <- df2[df2$V3==02,]
View(Feb)

b <- ggplot(Feb , aes(x=Feb$V1, y=Feb$V2, fill = Feb$V4)) + 
  geom_tile(color = "white", size = 0.1) + 
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  scale_fill_viridis(name="pm25", option = "plasma") + 
  coord_equal() + 
  labs(x="hour", y="day", title="Feb") + 
  theme_tufte(base_family="Helvetica") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
b
March <- df2[df2$V3==03,]
View(March)

c <- ggplot(March, aes(x=March$V1, y=March$V2, fill = March$V4)) + 
  geom_tile(color = "white", size = 0.1) + 
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  scale_fill_viridis(name="pm25", option = "plasma") + 
  coord_equal() + 
  labs(x="hour", y="day", title="March") + 
  theme_tufte(base_family="Helvetica") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
c

Apr <- df2[df2$V3==04,]
View(Apr)

d <- ggplot(Apr , aes(x=Apr$V1, y=Apr$V2, fill = Apr$V4)) + 
  geom_tile(color = "white", size = 0.1) + 
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  scale_fill_viridis(name="pm25", option = "plasma") + 
  coord_equal() + 
  labs(x="hour", y="day", title="April") + 
  theme_tufte(base_family="Helvetica") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
d
install.packages('ggpubr')
library(ggpubr)
figure <- ggarrange(a,b,c,d, labels = c("Jan", "Feb", "March", "April"), 
                    ncol = 2, nrow = 2)
figure

# Test for stationarity
library(tseries)
library(quantmod)

data <- AP
data <- xts(data[,2], order.by = as.Date(data[,1]))
colnames(data) <- "ger"
View(data)
plot(data)

adf.test(data, alternative = 'stationary', k=1)

library(urca)
y <- ur.df(AP$pm25, type = "drift", selectlags = "AIC")
y
summary(y)

kpss.test(AP$pm25)
adf.test(AP$pm25)
#### p-value is less than 0.05
### therefore it is not stationary


# DIfference data to make data stationary on mean

plot(diff(AP$pm25))
plot(log_pm25)
plot(diff(log_pm25))

###plot ACF and PACF to identify potential AR and MA model
par(mfrow = c(1,2))
acf(ts(diff(training$log_pm25)), main='ACF pm25')
pacf(ts(diff(training$log_pm25)),main = 'PACF pm25')

acf(ts(training$pm25), main='ACF pm25')
pacf(ts(training$pm25),main = 'PACF pm25')

acf(ts(diff(training$pm25)), main='ACF pm25')
pacf(ts(diff(training$pm25)),main = 'PACF pm25')

# Since, there are enough spikes in the plots outside the insignificant zone (dotted horizontal lines) 
# we can conclude that the residuals are not random.
# This implies that there is information available in residuals to be extracted by AR and MA models.






# Identification of best fit arima model
######################################
library(forecast)
require(forecast)

auto.arima(ts(training$pm25), trace = TRUE)

ARIMAfit = auto.arima(ts(training$pm25), approximation = FALSE, trace = FALSE)
summary(ARIMAfit)
# (2,1,3)

train <- ts(training$pm25)
test <- ts(testing$pm25)

fit <- auto.arima(train, max.p = 5, max.q = 5, max.P = 5, max.Q = 5, max.d = 3, seasonal = TRUE, trace = TRUE, ic = 'aicc')
fit
summary(fit)
# (5,1,2)

fit1 <- auto.arima(train, max.p = 4, max.q = 4, ic = 'aicc')
fit1
summary(fit1)
# (4,1,2)

fit2 <- auto.arima(train, max.p = 5, max.q = 5, max.P = 5, max.Q = 5, max.d = 3, seasonal = TRUE, approximation = FALSE, stepwise = FALSE, ic = 'aicc')
fit2
# (2,1,3)

plot(forecast(fit, h = 24))

########
mytss <- ts(training$pm25, frequency = 24)
x <- auto.arima(mytss, seasonal = T)
summary(x)

#x <- auto.arima(mytss, seasonal = T, approximation = FALSE, stepwise = FALSE)
#summary(x)


### 1
arimamodel3 = Arima(ts(training$pm25), order = c(4,1,4))
arimamodel3
summary(arimamodel3)

data.train <- Arima(train, order =c(4,1,4))
data.train %>%
  forecast(h=24) %>%
  autoplot()+autolayer(test)

data.test <- Arima(test, model = data.train)
accuracy(data.test)
summary(data.test)
#### 2
arimamodel4 = Arima(ts(training$pm25), order = c(5,1,2))
arimamodel4
summary(arimamodel4)

data.test <- Arima(test, model = arimamodel4)
accuracy(data.test)

#### 3
arimamodel5 = Arima(ts(training$pm25), order = c(2,1,3))
arimamodel5
summary(arimamodel5)

data.test <- Arima(test, model = arimamodel5)
accuracy(data.test)

###### Seasonal 
data <- ts(AP1$pm25, frequency = 24)
trainn <- ts(training$pm25, frequency = 24)
testt <- ts(testing$pm25, frequency = 24)
auto.arima(trainn, seasonal = T, trace = TRUE)
x <- auto.arima(mytss, seasonal = T)
summary(x)
mytest <- ts(testing$pm25, frequency = 24) 
sarima <- Arima(mytest, order = c(4,0,0), seasonal = c(2,1,0))
accuracy(sarima)

## ARIMA(2,0,2)(1,1,1)[24] with drift         : Inf
##ARIMA(0,0,0)(0,1,0)[24] with drift         : 19378.41
##ARIMA(1,0,0)(1,1,0)[24] with drift         : 17704.8
##ARIMA(0,0,1)(0,1,1)[24] with drift         : 17994.33
##ARIMA(0,0,0)(0,1,0)[24]                    : 19379.31
#ARIMA(1,0,0)(0,1,0)[24] with drift         : 18039.71

#ARIMA(1,0,0)(2,1,0)[24] with drift         : 17597.85
#ARIMA(1,0,0)(2,1,1)[24] with drift         : Inf
#ARIMA(1,0,0)(1,1,1)[24] with drift         : Inf
#ARIMA(0,0,0)(2,1,0)[24] with drift         : 19159.26
#ARIMA(2,0,0)(2,1,0)[24] with drift         : 17570.47
#ARIMA(2,0,0)(1,1,0)[24] with drift         : 17674.52
#ARIMA(2,0,0)(2,1,1)[24] with drift         : Inf
#ARIMA(2,0,0)(1,1,1)[24] with drift         : Inf
#ARIMA(3,0,0)(2,1,0)[24] with drift         : 17567.98
#ARIMA(3,0,0)(1,1,0)[24] with drift         : 17668.83
#ARIMA(3,0,0)(2,1,1)[24] with drift         : Inf
#ARIMA(3,0,0)(1,1,1)[24] with drift         : Inf
#ARIMA(4,0,0)(2,1,0)[24] with drift         : 17547.51
#ARIMA(4,0,0)(1,1,0)[24] with drift         : 17657.78
#ARIMA(4,0,0)(2,1,1)[24] with drift         : Inf
#ARIMA(4,0,0)(1,1,1)[24] with drift         : Inf
#ARIMA(5,0,0)(2,1,0)[24] with drift         : 17550.17
#ARIMA(4,0,1)(2,1,0)[24] with drift         : 17549.24
#ARIMA(3,0,1)(2,1,0)[24] with drift         : 17564.19
#ARIMA(5,0,1)(2,1,0)[24] with drift         : Inf
#ARIMA(4,0,0)(2,1,0)[24]                    : 17546.08
#ARIMA(4,0,0)(1,1,0)[24]                    : 17656.31
#ARIMA(4,0,0)(2,1,1)[24]                    : Inf
#ARIMA(4,0,0)(1,1,1)[24]                    : Inf
#ARIMA(3,0,0)(2,1,0)[24]                    : 17566.83
#ARIMA(5,0,0)(2,1,0)[24]                    : 17548.71
#ARIMA(4,0,1)(2,1,0)[24]                    : 17547.79
#ARIMA(3,0,1)(2,1,0)[24]                    : 17562.8
#ARIMA(5,0,1)(2,1,0)[24]                    : Inf


sarima1 <- Arima(trainn, order = c(2,0,2), seasonal = c(1,1,1))
sarima1t <- Arima(testt, model = sarima1)
accuracy(sarima1t)

sarima2 <- Arima(trainn, order = c(0,0,0), seasonal = c(0,1,0))
sarima2t <- Arima(testt, model = sarima2)
accuracy(sarima2t)

sarima3 <- Arima(trainn, order = c(1,0,0), seasonal = c(1,1,0))
sarima3t <- Arima(testt, model = sarima3)
accuracy(sarima3t)

sarima4 <- Arima(trainn, order = c(0,0,1), seasonal = c(0,1,1))
sarima4t <- Arima(testt, model = sarima4)
accuracy(sarima4t)

sarima5 <- Arima(trainn, order = c(1,0,0), seasonal = c(0,1,0))
sarima5t <- Arima(testt, model = sarima5)
accuracy(sarima5t)

sarima6 <- Arima(trainn, order = c(1,0,0), seasonal = c(0,1,0))
sarima6t <- Arima(testt, model = sarima6)
accuracy(sarima6t)

sarima7 <- Arima(trainn, order = c(1,0,0), seasonal = c(0,1,0))
sarima7t <- Arima(testt, model = sarima7)
accuracy(sarima7t)

sarima8 <- Arima(trainn, order = c(1,0,0), seasonal = c(2,1,0))
sarima8t <- Arima(testt, model = sarima8)
accuracy(sarima8t)

sarima9 <- Arima(trainn, order = c(1,0,0), seasonal = c(2,1,1))
sarima9t <- Arima(testt, model = sarima9)
accuracy(sarima9t)

sarima10 <- Arima(trainn, order = c(1,0,0), seasonal = c(1,1,1))
sarima10t <- Arima(testt, model = sarima10)
accuracy(sarima10t)

sarima11 <- Arima(trainn, order = c(0,0,0), seasonal = c(2,1,0))
sarima11t <- Arima(testt, model = sarima11)
accuracy(sarima11t)

sarima12 <- Arima(trainn, order = c(2,0,0), seasonal = c(2,1,0))
sarima12t <- Arima(testt, model = sarima12)
accuracy(sarima12t)

sarima13 <- Arima(trainn, order = c(2,0,0), seasonal = c(1,1,0))
sarima13t <- Arima(testt, model = sarima13)
accuracy(sarima13t)

sarima14 <- Arima(trainn, order = c(2,0,0), seasonal = c(2,1,1))
sarima14t <- Arima(testt, model = sarima14)
accuracy(sarima14t)

sarima15 <- Arima(trainn, order = c(2,0,0), seasonal = c(1,1,1))
sarima15t <- Arima(testt, model = sarima15)
accuracy(sarima15t)

sarima16 <- Arima(trainn, order = c(3,0,0), seasonal = c(2,1,0))
sarima16t <- Arima(testt, model = sarima16)
accuracy(sarima16t)

sarima17 <- Arima(trainn, order = c(3,0,0), seasonal = c(1,1,0))
sarima17t <- Arima(testt, model = sarima17)
accuracy(sarima17t)

sarima18 <- Arima(trainn, order = c(3,0,0), seasonal = c(2,1,1))
sarima18t <- Arima(testt, model = sarima18)
accuracy(sarima18t)

sarima19 <- Arima(trainn, order = c(3,0,0), seasonal = c(1,1,1))
sarima19t <- Arima(testt, model = sarima19)
accuracy(sarima19t)

sarima20 <- Arima(trainn, order = c(4,0,0), seasonal = c(2,1,0))
sarima20t <- Arima(testt, model = sarima20)
accuracy(sarima20t)

sarima21 <- Arima(trainn, order = c(4,0,0), seasonal = c(1,1,0))
sarima21t <- Arima(testt, model = sarima21)
accuracy(sarima21t)

sarima22 <- Arima(trainn, order = c(4,0,0), seasonal = c(2,1,1))
sarima22t <- Arima(testt, model = sarima22)
accuracy(sarima22t)

sarima23 <- Arima(trainn, order = c(4,0,0), seasonal = c(1,1,1))
sarima23t <- Arima(testt, model = sarima23)
accuracy(sarima23t)

sarima24 <- Arima(trainn, order = c(5,0,0), seasonal = c(2,1,0))
sarima24t <- Arima(testt, model = sarima24)
accuracy(sarima24t)

sarima25 <- Arima(trainn, order = c(4,0,1), seasonal = c(2,1,0))
sarima25t <- Arima(testt, model = sarima25)
accuracy(sarima25t)

sarima26 <- Arima(trainn, order = c(3,0,1), seasonal = c(2,1,0))
sarima26t <- Arima(testt, model = sarima26)
accuracy(sarima26t)

sarima27 <- Arima(trainn, order = c(5,0,1), seasonal = c(2,1,0))
sarima27t <- Arima(testt, model = sarima27)
accuracy(sarima27t)

sarima28 <- Arima(trainn, order = c(4,0,0), seasonal = c(2,1,0))
sarima28t <- Arima(testt, model = sarima28)
accuracy(sarima28t)

sarima29 <- Arima(trainn, order = c(4,0,0), seasonal = c(1,1,0))
sarima29t <- Arima(testt, model = sarima29)
accuracy(sarima29t)

sarima30 <- Arima(trainn, order = c(4,0,0), seasonal = c(2,1,1))
sarima30t <- Arima(testt, model = sarima30)
accuracy(sarima30t)

sarima31 <- Arima(trainn, order = c(4,0,0), seasonal = c(1,1,1))
sarima31t <- Arima(testt, model = sarima31)
accuracy(sarima31t)

sarima32 <- Arima(trainn, order = c(3,0,0), seasonal = c(2,1,0))
sarima32t <- Arima(testt, model = sarima32)
accuracy(sarima32t)

sarima33 <- Arima(trainn, order = c(5,0,0), seasonal = c(2,1,0))
sarima33t <- Arima(testt, model = sarima33)
accuracy(sarima33t)

sarima34 <- Arima(trainn, order = c(4,0,1), seasonal = c(2,1,0))
sarima34t <- Arima(testt, model = sarima34)
accuracy(sarima34t)

sarima35 <- Arima(trainn, order = c(3,0,1), seasonal = c(2,1,0))
sarima35t <- Arima(testt, model = sarima35)
accuracy(sarima35t)

sarima36 <- Arima(trainn, order = c(5,0,1), seasonal = c(2,1,0))
sarima36t <- Arima(testt, model = sarima36)
accuracy(sarima36t)








#####Best Model #############
data <- ts(AP1$pm25)
modelfit <- Arima(data, order = c(4,1,4))
summary(modelfit)
  
write.csv(data, "time series data")

par(mfrow = c(1,1))  
 
acf(ts(modelfit$residuals), main = 'ACF Residual')
pacf(ts(modelfit$residuals), main = 'PACF Residual')

checkresiduals(modelfit)
plot(modelfit$residuals)

qqnorm(modelfit$residuals)
qqline(modelfit$residuals)

autoplot(forecast(modelfit))
# Let us check the normality of the residuals of our three models. 
# In the follwing figure the histograms of the residuals are shown, which does not reveal a serious deviation from normality.

autoplot(forecast(modelfit))


autoplot(modelfit)
autoplot(modelfit)
#
pred = predict(modelfit, n.ahead = 24)
pred

plot(data, type = 'l')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

library(forecast)
data.forecast <- forecast(modelfit, h=24)
data.forecast

plot(data.forecast)

View(AP)

######### Deployment ###################

str(data1)
library(shiny)
ui <- fluidPage(
  titlePanel("Forecast"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num", "id",1),
    ),
    mainPanel(
      tableOutput("distplot")
    )
  )
)

server <- function(input, output){
  output$distplot <- renderTable({
    Result <- read.csv(file.choose())
    look1 <- VLOOKUP(1, Result, id, new)
    look2 <- VLOOKUP(2, Result, id, new)
    look3 <- VLOOKUP(3, Result, id, new)
    look4 <- VLOOKUP(4, Result, id, new)
    look5 <- VLOOKUP(5, Result, id, new)
    look6 <- VLOOKUP(6, Result, id, new)
  })
}

shinyApp(ui = ui, server = server)

Result <- read.csv(file.choose())
View(Result)
str(Result)
base <- merge(Result, by = "id", by = "new")
Result$id <- as.numeric(Result$id)

VLOOKUP(1, Result, Result$id, Result$new)
df2 <- data.frame(CustID = c(1001,1002,1003,1004,1005),
                  Age = c(29,22,29,33,32),
                  Gender = c("F","M","F","F","M"))

VLOOKUP(1001, df2, CustID, Age)

VLOOKUP(1, Result, id, new)
View(df2)
View(Result)
rlang::last_error()

#########Rough work############

Data1 <- ts(log(AP1$pm25))
train1 <- ts(log(training$pm25))
test1 <- ts(log(testing$pm25))

auto.arima(train1, trace = TRUE)

Fitted <- Arima(train1, order = c(2,1,2))
summary(Fitted)
data.test <- Arima(test1, model = Fitted)
accuracy(data.test)

Final <- Arima(Data1, order = c(2,1,2))
summary(Final)
acf(Final$residuals, main = 'ACF Residual')
pacf(Final$residuals, main = 'PACF Residual')

checkresiduals(modelfit)

