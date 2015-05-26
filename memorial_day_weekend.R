## 
# In this progam, we perform a statistical analysis to see if there is any evidence 
# that average gas prices do increase during Memorial Day weekend (the last weekend
# of each May)
# This code is used in the blog post at:
# http://www.exzakly.com/2015/05/26/memorial-day-weekend-gas-prices
##
# Copyright by Zak Ahmad, 2015 under MIT License
##

# load libraries
require(lubridate)
require(ggplot2)
require(car)

# This function takes a vector x and will shift all elements to the left (or up) 
# n number of times. The "new" elements created from the shifting are zeros.
shift <- function(x, n){
    # The function simply drops the first n elements of the vector x then appends
    # n zeros to the end of the vector x and returns it
    c(x[-(seq(n))], rep(0, n))
}

##### DATA PREPARATION #####

# data files
gasFile <- 'gas-prices.csv'
# read data from files
gasDF <- read.csv(gasFile,stringsAsFactors=FALSE)
# change Date data type from integer to Date
gasDF$DATE <- as.Date(gasDF$DATE)
# change Prices data type numeric from char
gasDF$VALUE <- as.numeric(gasDF$VALUE)
# extract month from date
gasDF$Month <- month(gasDF$DATE)
# find length (number of rows) of gas prices dataframe
len <- dim(gasDF)[1]
# compute percent change, the first entry is zero
gasDF$PctChg[1] <- 0
gasDF$PctChg[2:len] <- 100 * diff(gasDF$VALUE) / gasDF$VALUE[1:len - 1]
# compute the change in month values, this is will actually find first Monday of each month
gasDF$MDiff[1] <- 0
gasDF$MDiff[2:len] <- diff(gasDF$Month)
# drop first row since we do not want the zero we just placed to affect our analysis
gasDF <- gasDF[-1,]
# shift up by 1 to align with last Monday of each month
gasDF$MDiff <- shift(gasDF$MDiff,1)

# drop rows with NAs
gasDF <- gasDF[complete.cases(gasDF),]

# get indices of last Monday of each May in data
ind <- which(gasDF$MDiff != 0 & gasDF$Month == 5)

# get percent change of memorial-day weekend (last week of each May)
mmdPctChg <- gasDF$PctChg[ind]
# get all other percent change values which are not the last Monday of each month
otherPctChg <- gasDF$PctChg[-ind]

# display summary of Memorial Day Weekend sample
summary(mmdPctChg)
# display summary of remaining weeks sample
summary(otherPctChg)

# to use ggplot to make a density plot of both percent-change data vectors, we combine them into a single dataframe
pctChgDF <- rbind(data.frame(PctChg = mmdPctChg, Week = "Memorial Day Week"), data.frame(PctChg = otherPctChg, Week = "All Other Weeks"))
# now create plot
ggplot(pctChgDF, aes(PctChg, fill = Week)) +
    geom_density(alpha = 0.3) +
    xlab("Weekly Percent Change in Avg Gas Prices (%)") +
    ylab("Relative Frequency") +
    scale_x_continuous(breaks = seq(from = -10, to = 20, by = 5)) +
    scale_y_continuous(breaks = seq(from = 0, to = 0.35, by = 0.05))


##### ANALYSIS #####

# since both samples are normally distributed we will do a two-sample t-test, using t.test command,
# to determine if two the means are equal

# to provide the t.test command with the correct inputs, we have to determine if the variances are equal
# for that, we will use the levene test

# the levene test assumes all variances are equal under the null hypothesis. we will assume the mean is the
# center of both samples (changing to median doesn't change the results)
leveneTest(PctChg ~ Week, data=pctChgDF, center=mean)

# since the p-value returned by the leveneTest function shows suggests that we should accept the null hypothesis
# because it is > 0.05. Therefore, the variances are equal.

# under the null hypothesis, the t-test assumes that difference in means is zero (or, the means are equal)  which is specified by the mu=0 argument
# further, we will do a one-sided t-test since we want to verify the claim that "gas prices are higher on memorial
# day weekend". So, we will specify a difference in means greater than zero as the alternative hypothesis (in 
# other words, the mean of mmdPctChg and greater than otherPctChg), hence the alternative="greater" argument.
t.test(mmdPctChg, otherPctChg, alternative="greater", mu=0, var.equal=TRUE)

##### CONCLUSION #####

# the p-value returned by the two-sample one-sided t-test is >0.05 so we will accept the null hypothesis
# therefore, there is no evidence of rise in avg gas prices during memorial day weekend as suggested by the data