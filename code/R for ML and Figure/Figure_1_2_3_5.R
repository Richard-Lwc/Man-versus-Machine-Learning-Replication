###############################################################################
# Man versus Machine Learning - Forecast EPS
###############################################################################
###############################################################################
# Clean work space
###############################################################################
cat("\f")  
rm(list=ls())

wd <- "C:\\Users\\ASUS\\Desktop\\Man versus ML"
setwd(wd)
gc()
###############################################################
# Libraries
###############################################################
library(data.table)
library(lubridate)
library(ranger)
library(DescTools)
library(dplyr)
library(caret)
library(iml)
library(npregfast)
library(pdp)
library(vip)
library(ggplot2)
library(scatterplot3d)
library(plotly)
library(parttree)
library(rpart)
library(tidyquant)
library(stargazer)
library(xtable)
library(rpart.plot)
###############################################################
# Read Data
###############################################################

ipi <- fread('ipi.csv')
rgdp <- fread('rgdp.csv')
rpc <- fread('rpc.csv')
unemp <- fread('unemp.csv')

# find out the mode of each row so as to affirm the real number of all dates
mode.find <- function(x){
  nums <- table(x)
  if (length(nums) > 1){
    return(as.numeric(names(nums[which.max(nums)])))
  }
  return(NA)
}

real_ipi = data.frame(DATE=ipi$DATE, ipi=apply(ipi, 1, mode.find))
real_rgdp = data.frame(DATE=rgdp$DATE, rgdp=apply(rgdp, 1, mode.find))
real_rpc = data.frame(DATE=rpc$DATE, rpc=apply(rpc, 1, mode.find))
real_unemp = data.frame(DATE=unemp$DATE, unemp=apply(unemp, 1, mode.find))

dt <- fread('Q1_RF.csv')

dt[, predict_rf_mean := NULL]
p <- 'Q1'

year_month <- function(name, d){
  return(paste(name, as.character(year(d)) %>% substring(3), 'M', as.character(month(d)), sep = ''))
}
RUC_year_quarter <- function(d){
  if(month(d) %in% c(2, 3, 4))
    return(paste('RUC', as.character(year(d)) %>% substring(3), 'Q1', sep = ''))
  else if(month(d) %in% c(5, 6, 7))
    return(paste('RUC', as.character(year(d)) %>% substring(3), 'Q2', sep = ''))
  else if(month(d) %in% c(8, 9, 10))
    return(paste('RUC', as.character(year(d)) %>% substring(3), 'Q3', sep = ''))
  else if(month(d) %in% c(11, 12))
    return(paste('RUC', as.character(year(d)) %>% substring(3), 'Q4', sep = ''))
  else
    return(paste('RUC', as.character(year(d) - 1) %>% substring(3), 'Q4', sep = ''))
}
numeric_vars <- which(sapply(dt,is.numeric))

to_predict <- c('realized_linear_bias')

not_for_plot <- c('PERMNO', 'rankdate', 'USFIRM',
                  'CFACPR', 'CFACSHR', 'VALUE',"ANN_CFACSHR",
                  'Linear_Forecast', 'realized_linear_bias', 'FPI', 'adj_actual')

predictors <- setdiff(names(dt)[numeric_vars], union(to_predict, not_for_plot))

setkey(dt, rankdate, PERMNO)

begin_year <- 1986
last_year <- 2019

t_m <- ifelse(p %in% c('A2'), 24, 12)


###################
# Prediction
###################

dt[, realized_linear_bias := adj_actual - Linear_Forecast]
unique_dates <- dt[year(STATPERS) %>% between(begin_year, last_year), sort(unique(STATPERS))]

l_dates <- length(unique_dates)

length_loop <- l_dates

vars_winsorize <- c(predictors)

last_date <- unique_dates[l_dates] %m+% months(2)

ipi_ob_date <- c('DATE', year_month('IPT', last_date))
rgdp_ob_date <- c('DATE', year_month('ROUTPUT', last_date))
rpc_ob_date <- c('DATE', year_month('RCON', last_date))
unemp_ob_date <- c('DATE', RUC_year_quarter(last_date))
I = ipi[, ..ipi_ob_date]
names(I)[2] = 'IPI'
# I$IPI <- ifelse(is.na(I$IPI),real_ipi$ipi, I$IPI) # fill the test data
G = rgdp[, ..rgdp_ob_date]
names(G)[2] = 'RGDP'
# G$RGDP <- ifelse(is.na(G$RGDP), real_rgdp$rgdp, G$RGDP)
P = rpc[, ..rpc_ob_date]
names(P)[2] = 'RPC'
# P$RPC <- ifelse(is.na(P$RPC), real_rpc$rpc, P$RPC)
U = unemp[, ..unemp_ob_date]
names(U)[2] = 'UNEMP'
# U$UNEMP <- ifelse(is.na(U$UNEMP), real_unemp$unemp, U$UNEMP)
dt <- merge(dt, I, by.x = 'rankdate', by.y = 'DATE') %>% 
  merge(G, by.x = 'rankdate', by.y = 'DATE') %>% 
  merge(P, by.x = 'rankdate', by.y = 'DATE') %>% 
  merge(U, by.x = 'rankdate', by.y = 'DATE') 

dt <- dt[year(STATPERS) >= begin_year & year(STATPERS) <= last_year]

# replace the NA with the industry median
dt[, (vars_winsorize) := lapply(vars_winsorize, function(x) {
  x <- get(x)
  
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}), by = .(rankdate, industry)]

# there is still NA, cuz there may be one industry in a month with no available value
# Then, I have to replace them with last month's value
dt[, (vars_winsorize) := nafill(nafill(.SD, type = "locf"), type = "nocb"), .SDcols = vars_winsorize, by=PERMNO]
# industry median one more time to further replace the missing data
dt[, (vars_winsorize) := lapply(vars_winsorize, function(x) {
  x <- get(x)
  
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}), by = .(rankdate, industry)]

# Winsorize at the 1% level and standardize
dt[, (vars_winsorize) := lapply(vars_winsorize, function(x) {
  x <- get(x)
  
  x <- Winsorize(x, na.rm = TRUE, probs = c(.01,.99))
  return(x)
}), by = rankdate]
# there are still few missing value, omit them would not cost much (check with the summarise function)
dt <- na.omit(dt)
###################
# Figures 1 and 5
###################
to_predict <- c('realized_linear_bias')
vars_rf <- c('IPI', 'RGDP', 'RPC', 'UNEMP', to_predict, predictors)
set.seed(200)
plot_data <- dt[sample(.N, 20000), ..vars_rf]

standardization <- preProcess(plot_data)
plot_data <- predict(standardization, plot_data)

model_1 <- ranger(formula = realized_linear_bias ~ .,
                  data = as.data.frame(plot_data),
                  num.trees =  2000,
                  replace = FALSE,
                  min.node.size = 1)

############################

pdp_plot_wrapper <- function(model, var.name, x_text, yaxis.varname = 'EPS\nForecast'){
  partial_price <- pdp::partial(model, pred.var = var.name)
  plot_price <- autoplot(partial_price, smooth = FALSE) + xlab(x_text) +
    ylab(yaxis.varname) +  theme(legend.direction='vertical', legend.text=element_text(size=20), 
                                 text = element_text(size=20)) + 
    geom_hline(yintercept = 0, color="black")
  plot_price_2 <- plot_price + geom_smooth()
  # geom_smooth(): Aids the eye in seeing patterns in the presence of overplotting
  plot_price_2$layers[[1]] <- NULL
  plot(plot_price_2)
  save_name <- paste0('PDP_Linear_Error_', var.name, '_', yaxis.varname, '.png')
  ggsave(save_name, plot_price_2, width = 16, height = 9)
  return(plot_price_2)
}

test <- pdp_plot_wrapper(model_1, 'MEANEST', "Analysts' Forecast",
                         "Linear Forecast Error")

test <- pdp_plot_wrapper(model_1, 'past_eps', 'Past EPS',  "Linear Forecast Error")


################################
to_predict <- c('adj_actual')
vars_rf <- c('IPI', 'RGDP', 'RPC', 'UNEMP', to_predict, predictors)
set.seed(200)
plot_data <- dt[sample(.N, 20000), ..vars_rf]

standardization <- preProcess(plot_data)
plot_data <- predict(standardization, plot_data)

model_mean <- ranger(formula = adj_actual ~ .,
                     replace = FALSE, 
                     oob.error = FALSE,
                     min.node.size = 1,
                     data = as.data.frame(plot_data),
                     num.trees = 2000   
                     )

test <- pdp_plot_wrapper(model_mean, 'MEANEST', "Analysts' Forecast",
                         "Realized EPS")

test <- pdp_plot_wrapper(model_mean, 'past_eps', 'Past EPS',  "Realized EPS")

# Figure 5
pd <- partial(model_mean, pred.var = c("past_eps", "PRC")) 

# pd_small <- pd

plot_ly() %>% 
  add_trace(data = pd,  x=pd$past_eps, y=pd$PRC,
            z=pd$yhat, intensity = pd$yhat, type="mesh3d") %>% layout(
              scene = list(
                xaxis = list(title = "Past Standardized EPS"),
                yaxis = list(title = "Past Standardized Price"),
                zaxis = list(title = "Forecasted EPS")
              ))



###################
# Figure 2
###################
plot_data[, eps := adj_actual]
plot_data[, past_price_std := PRC]
plot_data[, past_eps_std := past_eps]
# model_picture$method
model_picture <- rpart(formula = eps ~ past_eps_std + past_price_std, data = as.data.frame(plot_data), cp = 0, maxdepth = 3)
# cp: complexity parameter. Any split that does not decrease the overall lack of
# fit by a factor of cp is not attempted. For instance, with anova splitting, this
# means that the overall R-squared must increase by cp at each step. 
png('EPS_Decision_Tree.png', width = 768, height = 432)
rpart.plot(model_picture, main = 'EPS', tweak = 1.5)

dev.off()


###################
# Figure 3
###################

p3 = ggplot(data = plot_data, aes(x = past_eps, y = PRC, colour = eps)) + 
  geom_point(alpha = 2) + guides(colour = 'none') + 
  geom_parttree(data = model_picture, aes(fill=eps), alpha = 0.2) + 
  theme(legend.direction='vertical', legend.text=element_text(size=20), 
                           text = element_text(size=20)) + 
  xlab('Past Standardized EPS') + 
  ylab('Past Standardized Price')
p3 
ggsave(paste0('RandomForestRegionPoints', '.png'), p3, width = 16, height = 9)

p2 = (
  ggplot(data = plot_data, aes(x = past_eps, y = PRC, colour = eps)) + 
    guides(colour = 'none') + 
    geom_parttree(data = model_picture, aes(fill=eps), alpha = 0.7) +
    theme(legend.direction='vertical', legend.text=element_text(size=20), 
                             text = element_text(size=20)) + 
    xlab('Past Standardized EPS') + 
    ylab('Past Standardized Price')
  # + scale_fill_continuous(limits = range(train_loop$eps))
)
p2 <- p2 + geom_point(alpha = 0) + guides(colour = 'none')
p2

ggsave(paste0('RandomForestRegionNoPoints', '.png'), p2, width = 16, height = 9)
