###############################################################################
# Man versus Machine Learning - Forecast EPS
###############################################################################
###############################################################################
# Clean workplace
###############################################################################
rm(list=ls())
wd <-"C:\\Users\\ASUS\\Desktop\\data\\generated excel"
setwd(wd)
gc()

###############################################################
# Read Data
###############################################################
library(data.table)
library(lubridate)
library(tidyquant)
library(dplyr)
library(stargazer)
library(xtable)
library(ggplot2)



dt <- fread('ret_avg_bias.csv')
# dt <- fread('ret_port_score.csv')
port_returns <- dt[, .(rankdate, LS)]
port_returns[, months_ahead :=as.double( 1:.N)]


p<-(ggplot(data=port_returns, aes(x=months_ahead, y=LS)) 
    + geom_line(color = "blue")
    + geom_smooth(color = "red")
    + geom_hline(yintercept=0)
    + xlab('Months After Portfolio Formation') 
    + ylab('Average Return') 
    +  theme(legend.direction='vertical'))
p
# p<-p+geom_ribbon(aes(ymin=port_returns$lower, ymax=port_returns$upper), linetype=2, alpha=0.1)

ggsave(paste0('Decay_BE', '.png'), p, width = 16, height = 9)

