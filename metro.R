# Original Data set:
# https://www.realtor.com/research/data/
#
# There are 500 DMAs listed in this data set
library(zoo)
library(stringr)
library(sqldf)

# Read in Data Set
setwd("/Users/ericvela/Documents/Github/Realtor")
metro <- read.csv("RDC_InventoryCoreMetrics_Metro_Hist.csv")

metro$Month <- as.Date(metro$Month, "%m/%d/%y")

# Rename Month Column to Date
names(metro)[1] <- "Date"

metro <- subset(metro, select = c(1,2,3,4,15,16,17))

# Create attribute that only includes months
# metro$MonthOnly <- format(metro$Date, format = "%m")
# Create attribute that is Month Year
# metro$MonthYear <- format(metro$Date, format = "%m-%y")

# Divide each DMA by Quarters
metro$Quarter <- as.yearqtr(as.Date(metro$Date, "%m/%d/%Y"))

metro$CBSATitle <- as.character(metro$CBSATitle)

# take all strings before the comma (odd indexed strings) and create DMA attribute
split_df <- data.frame(unlist(strsplit(metro$CBSATitle, ",", fixed = TRUE)))
ind <- seq(1, nrow(split_df)+1, by =2)
metro$DMA <- factor(split_df[ind,])

# take all strings after the comma (even indexed strings) and create range of states per DMA
ind2 <- seq(0, nrow(split_df)+2, by = 2)
metro$States <- factor(split_df[ind2,])
metro$States <- trimws(metro$States, which = "left")


# Make a table displaying quarterly new listings per DMA

quart <- data.frame(xtabs(metro[metro$Quarter == "2012 Q2",]$New.Listing.Count ~ metro[metro$Quarter == "2012 Q2",]$DMA))
quart <- data.frame(quart)
names(quart)[1] <- "DMA"
names(quart)[2] <- "Q2.2012"

quart$Q3.2012 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2012 Q3",]$New.Listing.Count ~ metro[metro$Quarter == "2012 Q3",]$DMA))[2])
quart$Q4.2012 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2012 Q4",]$New.Listing.Count ~ metro[metro$Quarter == "2012 Q4",]$DMA))[2])

quart$Q1.2013 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2013 Q1",]$New.Listing.Count ~ metro[metro$Quarter == "2013 Q1",]$DMA))[2])
quart$Q2.2013 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2013 Q2",]$New.Listing.Count ~ metro[metro$Quarter == "2013 Q2",]$DMA))[2])
quart$Q3.2013 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2013 Q3",]$New.Listing.Count ~ metro[metro$Quarter == "2013 Q3",]$DMA))[2])
quart$Q4.2013 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2013 Q4",]$New.Listing.Count ~ metro[metro$Quarter == "2013 Q4",]$DMA))[2])

quart$Q1.2014 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2014 Q1",]$New.Listing.Count ~ metro[metro$Quarter == "2014 Q1",]$DMA))[2])
quart$Q2.2014 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2014 Q2",]$New.Listing.Count ~ metro[metro$Quarter == "2014 Q2",]$DMA))[2])
quart$Q3.2014 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2014 Q3",]$New.Listing.Count ~ metro[metro$Quarter == "2014 Q3",]$DMA))[2])
quart$Q4.2014 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2014 Q4",]$New.Listing.Count ~ metro[metro$Quarter == "2014 Q4",]$DMA))[2])

quart$Q1.2015 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2015 Q1",]$New.Listing.Count ~ metro[metro$Quarter == "2015 Q1",]$DMA))[2])
quart$Q2.2015 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2015 Q2",]$New.Listing.Count ~ metro[metro$Quarter == "2015 Q2",]$DMA))[2])
quart$Q3.2015 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2015 Q3",]$New.Listing.Count ~ metro[metro$Quarter == "2015 Q3",]$DMA))[2])
quart$Q4.2015 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2015 Q4",]$New.Listing.Count ~ metro[metro$Quarter == "2015 Q4",]$DMA))[2])

quart$Q1.2016 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2016 Q1",]$New.Listing.Count ~ metro[metro$Quarter == "2016 Q1",]$DMA))[2])
quart$Q2.2016 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2016 Q2",]$New.Listing.Count ~ metro[metro$Quarter == "2016 Q2",]$DMA))[2])
quart$Q3.2016 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2016 Q3",]$New.Listing.Count ~ metro[metro$Quarter == "2016 Q3",]$DMA))[2])
quart$Q4.2016 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2016 Q4",]$New.Listing.Count ~ metro[metro$Quarter == "2016 Q4",]$DMA))[2])

quart$Q1.2017 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2017 Q1",]$New.Listing.Count ~ metro[metro$Quarter == "2017 Q1",]$DMA))[2])
quart$Q2.2017 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2017 Q2",]$New.Listing.Count ~ metro[metro$Quarter == "2017 Q2",]$DMA))[2])
quart$Q3.2017 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2017 Q3",]$New.Listing.Count ~ metro[metro$Quarter == "2017 Q3",]$DMA))[2])
quart$Q4.2017 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2017 Q4",]$New.Listing.Count ~ metro[metro$Quarter == "2017 Q4",]$DMA))[2])

quart$Q1.2018 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2018 Q1",]$New.Listing.Count ~ metro[metro$Quarter == "2018 Q1",]$DMA))[2])
quart$Q2.2018 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2018 Q2",]$New.Listing.Count ~ metro[metro$Quarter == "2018 Q2",]$DMA))[2])
quart$Q3.2018 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2018 Q3",]$New.Listing.Count ~ metro[metro$Quarter == "2018 Q3",]$DMA))[2])
quart$Q4.2018 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2018 Q4",]$New.Listing.Count ~ metro[metro$Quarter == "2018 Q4",]$DMA))[2])

quart$Q1.2019 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2019 Q1",]$New.Listing.Count ~ metro[metro$Quarter == "2019 Q1",]$DMA))[2])
quart$Q2.2019 <- unlist(data.frame(xtabs(metro[metro$Quarter == "2019 Q2",]$New.Listing.Count ~ metro[metro$Quarter == "2019 Q2",]$DMA))[2])

write.csv(quart, file = "quarterly_new_listings.csv")

# test code
sum(metro[metro$Quarter == "2013 Q4" & metro$DMA == "Aberdeen",]$New.Listing.Count, na.rm = TRUE)

# Function to graph DMAs over time
graphDMA <- function(DMAString) {
  par(mfrow=c(1,1))
  main_nl <- paste("Number of New Listings in \n",DMAString, "Metro")
  plot(metro[metro$DMA == DMAString,]$Date, metro[metro$DMA == DMAString,]$New.Listing.Count,
       type = "o",
       xlab = "Year",
       ylab = "Number of New Listings",
       main = main_nl)
}

graphDMA("San Diego-Carlsbad")

quart[order(-quart$Q2.2012),]
quantile(quart$Q2.2012, probs = c(.95, 1))

metro[metro$States]


chisq.test(c(23, 30, 19,28), c(25,25,25,25))

rnorm(100, )