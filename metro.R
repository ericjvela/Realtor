# Original Data set:
# https://www.realtor.com/research/data/
#
# There are 500 DMAs listed in this data set

# Read in Data Set
setwd("/Users/ericvela/Documents/Realtor Data")
metro <- read.csv("RDC_InventoryCoreMetrics_Metro_Hist.csv")

metro$Month <- as.Date(metro$Month, "%m/%d/%y")

# Rename Month Column to Date
names(metro)[1] <- "Date"
# Create attribute that only includes months
metro$MonthOnly <- format(metro$Date, format = "%m")
# Create attribute that is Month Year
metro$MonthYear <- format(metro$Date, format = "%m-%y")
# Create Yearly filter dummy attributes
metro$is2012 <- ifelse(metro$Date < as.Date("2013-01-01"),1,0)
metro$is2013 <- ifelse(metro$Date < as.Date("2014-01-01") & metro$Date >= as.Date("2013-01-01"),1,0)
metro$is2014 <- ifelse(metro$Date < as.Date("2015-01-01") & metro$Date >= as.Date("2014-01-01"),1,0)
metro$is2015 <- ifelse(metro$Date < as.Date("2016-01-01") & metro$Date >= as.Date("2015-01-01"),1,0)
metro$is2016 <- ifelse(metro$Date < as.Date("2017-01-01") & metro$Date >= as.Date("2016-01-01"),1,0)
metro$is2017 <- ifelse(metro$Date < as.Date("2018-01-01") & metro$Date >= as.Date("2017-01-01"),1,0)
metro$is2018 <- ifelse(metro$Date < as.Date("2019-01-01") & metro$Date >= as.Date("2018-01-01"),1,0)
metro$is2019 <- ifelse(metro$Date < as.Date("2020-01-01") & metro$Date >= as.Date("2019-01-01"),1,0)

metro$CBSATitle <- as.character(metro$CBSATitle)
split_df <- data.frame(unlist(strsplit(metro$CBSATitle, ",", fixed = TRUE)))
ind <- seq(1, nrow(split_df)+1, by =2)
metro$DMA <- split_df[ind,]

ind2 <- seq(0, nrow(split_df)+2, by = 2)
metro$States <- split_df[ind2,]
metro$States <- trimws(metro$States, which = "left")

graphDMA <- function(DMAString) {
  par(mfrow=c(1,1))
  main_nl <- paste("Number of New Listings in \n",DMAString)
  plot(metro[metro$DMA == DMAString,]$Date, metro[metro$DMA == DMAString,]$New.Listing.Count,
       type = "o",
       xlab = "Year",
       ylab = "Number of New Listings",
       main = main_nl)
}

graphDMA("San Diego-Carlsbad")

#write.csv(metro, file = "metro_cleaned.csv")
