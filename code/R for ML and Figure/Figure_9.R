###############################################################################
# Man versus Machine Learning - Forecast EPS
###############################################################################
###############################################################################
# Clean workplace
###############################################################################
rm(list=ls())
wd <- "C:\\Users\\ASUS\\Desktop\\data\\generated excel"
setwd(wd)
gc()
###############################################################################
# Functions
###############################################################################
source("Functions.R")
###############################################################
# Read Data
###############################################################
library(data.table)
library(lubridate)
library(tidyquant)
library(dplyr)
library(stargazer)
library(xtable)
library(DescTools)

RF <- fread('ret_avg_bias_equal.csv')
RF[, Mkt := `Mkt-RF`]
RF['']
f.dates.year.month.ff(RF)
f.datadate.date_match.ff(RF)

setkey(RF, date_match)
vars <- c('date_match', 'LS', 'Mkt', 'RF')
merged_dt <- RF[ , ..vars]


cumret <- function(ret){
  ret[1] <- 0
  return(cumprod(1+ret))
}



png(file="CumretsRF.png", width=1280, height=720)

plot(merged_dt[, date_match], log(cumret(merged_dt[, -LS])),
     # ylim = c(-1 , 2),
     ylim = c(0 , 6),
     type = 'l',
     ylab = 'Log Cumulative Return of the Portfolios', col = 'blue',
     xlab = '')# )

merged_dt[, lines(date_match, log(cumret(Mkt/100)), col = 'black')]

legend('topleft', legend=c("Long-Short Conditional Bias",'Market'),
       col=c("blue", 'black'),lty=1)
dev.off()

