###############################################################################
# Man versus Machine Learning - Forecast EPS
###############################################################################
###############################################################################
# Clean workplace
###############################################################################
rm(list=ls())
wd <- "C:\\Users\\ASUS\\Desktop\\data\\dt"
setwd(wd)
gc()
###############################################################################

###############################################################
# Read Data
###############################################################
library(data.table)
library(lubridate)
library(tidyquant)
library(dplyr)
library(stargazer)
library(xtable)
##############################
# Figures
##############################
names_figure <- c("BEQ1", 'BEQ2', 'BEQ3', 'BEA1s', 'BEA2s')
legends_figure <- c("One-quarter-ahead","Two-quaters-ahead","Three-quarters-ahead", 
                    "One-year-ahead","Two-years-ahead")

be_dt <- fread('BE_PERMNO_DATE.csv')
# be_dt would contain the permno-date observations
a<- be_dt[.N > 100,
         lapply(.SD, base::mean, na.rm = TRUE, trim = .01),
         by = rankdate, .SDcols = names_figure]
fwrite(a, 'AverageBE.csv')
a <- fread('AverageBE.csv')
b <- melt(a, id.vars = 'rankdate', variable.factor = TRUE)
# melt 其实就是gather
library(ggplot2)
plot_bias <- ggplot(data=b, mapping=aes(x=rankdate, y=value, group = variable, color = variable)) + 
  geom_line() + 
  theme_bw() + xlab('Date') + ylab('') + 
  scale_color_discrete(
    labels = legends_figure) + 
  scale_x_discrete(breaks=c('1990-01', '2000-01', '2010-01', '2020-01'))+
  theme(legend.title=element_blank(), 
        legend.position= c(0.15,.83),
        legend.direction='vertical', 
        legend.text=element_text(size=20),
        text = element_text(size=20),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        )
plot_bias
ggsave(paste0('BiasAFML', '.png'), plot_bias, width = 16, height = 9)


be_dt <- fread('BE_PERMNO_DATE_real.csv')
# be_dt would contain the permno-date observations
a<- be_dt[.N > 100,
          lapply(.SD, base::mean, na.rm = TRUE, trim = .01),
          by = rankdate, .SDcols = names_figure]
# fwrite(a, 'AverageBE_real.csv')
# a <- fread('AverageBE_real.csv')
b <- melt(a, id.vars = 'rankdate', variable.factor = TRUE)

plot_bias <- ggplot(data=b, mapping=aes(x=rankdate, y=value, group = variable, color = variable)) + 
  geom_line() + 
  theme_bw() + xlab('Date') + ylab('') + 
  scale_color_discrete(
    labels = legends_figure) + 
  scale_x_discrete(breaks=c('1990-01', '2000-01', '2010-01', '2020-01'))+
  theme(legend.title=element_blank(), 
        legend.position= c(0.15,.83),
        legend.direction='vertical', 
        legend.text=element_text(size=20),
        text = element_text(size=20),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )
plot_bias
ggsave(paste0('BiasAEREAL', '.png'), plot_bias, width = 16, height = 9)
