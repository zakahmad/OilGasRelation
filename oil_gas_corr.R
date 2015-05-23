
require(ggplot2)
require(gridExtra)
require(ggExtra)
# data files
gasFile <- 'gas-prices.csv'
usoFile <- 'wti-crude-oil-prices.csv'
# read data from files
gasDF <- read.csv(gasFile,stringsAsFactors=FALSE)
oilDF <- read.csv(usoFile,stringsAsFactors=FALSE)
# change Date data type from integer to Date
gasDF$DATE <- as.Date(gasDF$DATE)
oilDF$DATE <- as.Date(oilDF$DATE)
# change Prices data type numeric from char
gasDF$VALUE <- as.numeric(gasDF$VALUE)
oilDF$VALUE <- as.numeric(oilDF$VALUE)
# compute percent change
gasPctChg <- 100 * diff(gasDF$VALUE) / gasDF$VALUE[1:length(gasDF$VALUE) - 1]
oilPctChg <- 100 * diff(oilDF$VALUE) / oilDF$VALUE[1:length(oilDF$VALUE) - 1]
# delete first row
gasDF <- gasDF[-1,]
oilDF <- oilDF[-1,]
# Store percent change to DF
gasDF$PctChg <- gasPctChg
oilDF$PctChg <- oilPctChg
# remove rows with NAs
gasDF <- gasDF[complete.cases(gasDF),]
oilDF <- oilDF[complete.cases(oilDF),]
# shift oil price dates by 3 days to align with gas dates
oilDF$DATE <- oilDF$DATE+3
# merge oil and gas prices by date
oilgasDF <- merge(gasDF,oilDF,by="DATE",all.x=FALSE)
# rename columns to be more descriptive
colnames(oilgasDF)[2] <- "GasPrice"
colnames(oilgasDF)[3] <- "GasPctChg"
colnames(oilgasDF)[4] <- "OilPrice"
colnames(oilgasDF)[5] <- "OilPctChg"

# make scatter plot with OilPrice on the x-axis and GasPrice on y-axis
# use white background and grey major lines along with blue dots
# then make sure the x-axis runs in increments of 10 and y-axis in 0.5
p <- ggplot(oilgasDF, aes(OilPrice, GasPrice)) +
    theme(panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(colour = "grey"),
          axis.text.x = element_text(colour="black"),
          axis.text.y = element_text(colour="black")) +
    xlab("Crude Oil Price (USD per Barrel)") +
    ylab("Avg Nat'l Gas Price (USD per Gallon)") +
    geom_point(color="blue",size=1.5) +
    geom_abline(slope=3.5/140, intercept=0.5)+
    annotate("text", x = 95, y = 2.2, label = "Diagonal Trendline \n (not best-fit line)", size = 4) +
    scale_x_continuous(breaks = seq(floor(min(oilgasDF$OilPrice)/10)*10,ceiling(max(oilgasDF$OilPrice)/10)*10,by=10)) +
    scale_y_continuous(breaks = seq(floor(min(oilgasDF$GasPrice)),ceiling(max(oilgasDF$GasPrice)),by=0.5))
ggMarginal(p, type = "histogram")

# make scatter plot with OilPctChg on the x-axis and GasPctChg on y-axis
# use white background and grey major lines along with blue dots
p <- ggplot(oilgasDF, aes(OilPctChg, GasPctChg)) +
    theme(panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(colour = "grey"),
          axis.text.x = element_text(colour="black"),
          axis.text.y = element_text(colour="black")) +
    xlab("Percent Change in Crude Oil Prices (%)") +
    ylab("Percent Change in Avg Gas Prices (%)") +
    geom_point(color="blue",size=1)
# add histograms of OilPrice and GasPrice to the top and right of the plot, respectively
ggMarginal(p, type = "histogram")

# create correlogram of lagging GasPctChg against OilPctChg
ccf(oilgasDF$GasPctChg,oilgasDF$OilPctChg,lag.max=10,main="",ylab="Correlation",xlab="Lag Period (week)")

# correlation between oil price and gas price
round(cor(oilgasDF$OilPrice,oilgasDF$GasPrice),2)

# correlation between change in oil price and change in gas price
round(cor(oilgasDF$OilPctChg,oilgasDF$GasPctChg),2)

