# Original Data set:
# https://www.realtor.com/research/data/

# Sources:
# https://www.analyticsvidhya.com/blog/2015/12/complete-tutorial-time-series-modeling/
# https://anomaly.io/seasonal-trend-decomposition-in-r/

library(forecast)
library(plyr)
library(lattice)
library(zoo)
library(tseries)

# Read in Data Set
setwd("/Users/ericvela/Documents/Realtor Data")
national <- read.csv("RDC_InventoryCoreMetrics_US_Hist.csv")

# DATA TRANSFORMATIONS
#
# Format Month as MM/DD/YYYY
national$Month <- as.Date(national$Month, "%m/%d/%y")
# Rename Month Column to Date
names(national)[1] <- "Date"
# Create attribute that only includes months
national$MonthOnly <- format(national$Date, format = "%m")
# Create attribute that is Month Year
national$MonthYear <- format(national$Date, format = "%m-%y")
# Create Yearly filter dummy attributes
national$is2012 <- ifelse(national$Date < as.Date("2013-01-01"),1,0)
national$is2013 <- ifelse(national$Date < as.Date("2014-01-01") & national$Date >= as.Date("2013-01-01"),1,0)
national$is2014 <- ifelse(national$Date < as.Date("2015-01-01") & national$Date >= as.Date("2014-01-01"),1,0)
national$is2015 <- ifelse(national$Date < as.Date("2016-01-01") & national$Date >= as.Date("2015-01-01"),1,0)
national$is2016 <- ifelse(national$Date < as.Date("2017-01-01") & national$Date >= as.Date("2016-01-01"),1,0)
national$is2017 <- ifelse(national$Date < as.Date("2018-01-01") & national$Date >= as.Date("2017-01-01"),1,0)
national$is2018 <- ifelse(national$Date < as.Date("2019-01-01") & national$Date >= as.Date("2018-01-01"),1,0)
national$is2019 <- ifelse(national$Date < as.Date("2020-01-01") & national$Date >= as.Date("2019-01-01"),1,0)

# DATA VISUALIZATION (EDA)
#
# Plotting Date and New Listing Count
plot(national$Date, national$New.Listing.Count, 
     type = "o", 
     xlab = "Year", 
     ylab = "Number of New Listings", 
     main = "New Listings Over Time")

# Plotting Date and Price Increase Count
plot(national$Date, national$Price.Increase.Count, 
     type = "o",
     xlab = "Year",
     ylab = "Number of Listings with Price Increases",
     main = "Number of Listings with Price Increases Over Time")

# Plotting Date and Days on Market
plot(national$Date, national$Days.on.Market,
     type = "o",
     xlab = "Year",
     ylab = "Median Days on Market",
     main = "Median Days on Market Over Time")
abline(lm(national$Days.on.Market~national$Date), col = "red")


# Plotting Date and Median Listing Price
plot(national$Date, national$Median.Listing.Price,
     type = "o",
     xlab = "Year",
     ylab = "Median Listing Price",
     main = "Median Listing Price Over Time")
abline(lm(national$Median.Listing.Price~national$Date), col = "red")

# As Days on the market decreases, median listing price increases
cor(national$Days.on.Market, national$Median.Listing.Price)

# Find Max number of New Listings and the Date
# The maximum number of listings was in 2015-05-01 with 65572
max(national$New.Listing.Count)
national[which.max(national$New.Listing.Count),]$Date

#
# INSIGHT AND DESCRIPTIVE ANALYTICS
#
# Amount of New Listings per Year

# Amount of New Listings in 2012
barplot(national[national$is2012 == "1",]$New.Listing.Count,
        names.arg = sort(national[national$is2012 == "1",]$MonthYear, decreasing = FALSE),
        col = cm.colors(12),
        main = "Amount of New Listings in 2012",
        xlab = "Month-Year",
        ylab = "Amount of Listings",
        ylim = c(0,660000),
        las = 2)

# Amount of New Listings in 2013
barplot(national[national$is2013 == "1",]$New.Listing.Count,
        names.arg = sort(national[national$is2013 == "1",]$MonthYear, decreasing = FALSE),
        col = cm.colors(12),
        main = "Amount of New Listings in 2013",
        xlab = "Month-Year",
        ylab = "Amount of Listings",
        ylim = c(0,660000),
        las = 2)

# Amount of New Listings in 2014
barplot(national[national$is2014 == "1",]$New.Listing.Count,
        names.arg = sort(national[national$is2014 == "1",]$MonthYear, decreasing = FALSE),
        col = cm.colors(12),
        main = "Amount of New Listings in 2014",
        xlab = "Month-Year",
        ylab = "Amount of Listings",
        ylim = c(0,660000),
        las = 2)

# Amount of New Listings in 2015
barplot(national[national$is2015 == "1",]$New.Listing.Count,
        names.arg = sort(national[national$is2015 == "1",]$MonthYear, decreasing = FALSE),
        col = cm.colors(12),
        main = "Amount of New Listings in 2015",
        xlab = "Month-Year",
        ylab = "Amount of Listings",
        ylim = c(0,660000),
        las = 2)

# Amount of New Listings in 2016
barplot(national[national$is2016 == "1",]$New.Listing.Count,
        names.arg = sort(national[national$is2016 == "1",]$MonthYear, decreasing = FALSE),
        col = cm.colors(12),
        main = "Amount of New Listings in 2016",
        xlab = "Month-Year",
        ylab = "Amount of Listings",
        ylim = c(0,660000),
        las = 2)

# Amount of New Listings in 2017
barplot(national[national$is2017 == "1",]$New.Listing.Count,
        names.arg = sort(national[national$is2017 == "1",]$MonthYear, decreasing = FALSE),
        col = cm.colors(12),
        main = "Amount of New Listings in 2017",
        xlab = "Month-Year",
        ylab = "Amount of Listings",
        ylim = c(0,660000),
        las = 2)

# Amount of New Listings in 2018
barplot(national[national$is2018 == "1",]$New.Listing.Count,
        names.arg = sort(national[national$is2018 == "1",]$MonthYear, decreasing = FALSE),
        col = cm.colors(12),
        main = "Amount of New Listings in 2018",
        xlab = "Month-Year",
        ylab = "Amount of Listings",
        ylim = c(0,660000),
        las = 2)

# Amount of New Listings in 2019
barplot(national[national$is2019 == "1",]$New.Listing.Count,
        names.arg = sort(national[national$is2019 == "1",]$MonthYear, decreasing = FALSE),
        col = cm.colors(3),
        main = "Amount of New Listings in 2019",
        xlab = "Month-Year",
        ylab = "Amount of Listings",
        ylim = c(0,660000),
        las = 2)

# Amount of New Listings All Time
new.listings.all.time <- aggregate(New.Listing.Count ~ MonthYear, data = national, FUN = sum)
new.listings.all.time <- new.listings.all.time[order(-new.listings.all.time$New.Listing.Count),]
names(new.listings.all.time)[1] <- "MonthYear"
names(new.listings.all.time)[2] <- "Frequency.of.New.Listings"
new.listings.all.time <-transform(new.listings.all.time, Relative = prop.table(Frequency.of.New.Listings))
new.listings.all.time

# Yikes
barplot(new.listings.all.time$New.Listing.Count)

# Amount of New Listings by month Plot
new.listings.by.month <- aggregate(New.Listing.Count ~ MonthOnly, data = national, FUN = sum)
names(new.listings.by.month)[1] <- "Month"
names(new.listings.by.month)[2] <- "Frequency.of.New.Listings"
new.listings.by.month <-transform(new.listings.by.month, Relative = prop.table(Frequency.of.New.Listings))
new.listings.by.month$MonthAbb <- c("Jan","Feb","Mar",
                                    "Apr","May","Jun",
                                    "Jul","Aug","Sep",
                                    "Oct","Nov","Dec")
mean(new.listings.by.month$Frequency.of.New.Listings)
barplot(new.listings.by.month$Frequency.of.New.Listings,
        names.arg = new.listings.by.month$MonthAbb,
        col = cm.colors(12),
        ylab = "Frequency",
        xlab = "Month",
        main = "New Listings Per Month All Time",
        )


# Graph relative frequency per month for new listings
barplot(new.listings.by.month$Relative, 
        names.arg = new.listings.by.month$MonthAbb,
        xlab = "Month",
        ylab = "Relative Frequency",
        main = "Relative Frequency Per Month for New Listings",
        col = terrain.colors(12),
        ylim = c(0, .10))

#
# MODEL BUILDING
#

# Coerce Date and New Listings Count to a Time Series object
nldf <- subset(national, select = c(Date, New.Listing.Count))
nlts <- as.ts(read.zoo(nldf, FUN = as.yearmon))

plot(nlts,
     ylab = "New Listings",
     main = "New Listings Thus Far")

# unreliable trend, not run
#abline(reg=lm(nlts~time(nlts)), col = "red")

# aggregate the cycle year over year
plot(aggregate(nlts, FUN = mean))

# analyze seasonal effect
seas.effect <- boxplot(nlts~cycle(nlts))

# Check to see if the data is stationary
# 
# The data is stationary enough for now
adf.test(diff(log(nlts)), alternative="stationary", k=0)

# ACF plot
#
acf(log(nlts))

# use log trasnformation and regress on the difference of logs
# ACF plot cyts off after the first lag
acf(diff(log(nlts)))
# partial autocorrelation plot
pacf(diff(log(nlts)))

# ACF curve is getting cut off at Lag = 1
# p should be 0 (AR part of ARIMA) the data is not autoregressive


# fitted arima model
# Order is (p,d,q)
# 1st value (AR/p) of order should be 0 (no autocorrelation/autoregression)
# 2nd value (I/d) of order should be 1, we want differencing to remove non-stationarity,
# the data has time variance I = 1
# 3rd value (MA/q) of order should be 1 or 2 lags (1 seems better)
fit <- arima(log(nlts), order = c(0, 1, 2), seasonal = list(order = c(0, 1, 2), period = 12))
fit2 <- arima(log(nlts), order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
fit3 <- arima(log(nlts), order = c(0, 1, 3), seasonal = list(order = c(0, 1, 3), period = 12))

AIC(fit, fit2, fit3)
BIC(fit, fit2, fit3)

# Fit2 has the lowest combined AIC and BIC

# predict two years ahead (monthly)
pred <- predict(fit2, n.ahead = 2*12)

# natural log predicted power term (note: log base e of x)
e.pred <- 2.718^pred$pred

# plot previous data with logged predicted values
ts.plot(nlts,e.pred, 
        log = "y", 
        lty = c(1,3),
        ylab = "New Listings",
        main = "Forecasted New Listings in the United States \n (2 Year Prediction)")

