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
library(tidyverse)
library(data.table)
library(lubridate)
library(ranger)
library(DescTools)
library(caret)
library(iml)
library(npregfast)
library(pdp)
library(vip)
library(ggplot2)
library(scatterplot3d)
library(rpart)
###############################################################
# Read Data
###############################################################

ipi <- fread('ipi.csv')
rgdp <- fread('rgdp.csv')
rpc <- fread('rpc.csv')
unemp <- fread('unemp.csv')

# find out the mode of each row so as to affirm the real number of all dates
# then I can put them into the test set as the test set should contain the ground truth
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

A1 <- fread('A1_.csv')
A2 <- fread('A2_.csv')
Q1 <- fread('Q1_.csv')
Q2 <- fread('Q2_.csv')
Q3 <- fread('Q3_.csv')


periods <- c('A1', 'A2', 'Q1', 'Q2', 'Q3')
# the function to standarize the the release date of the data 
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

###############################################################
# Define predictors and related vectors
###############################################################

numeric_vars <- which(sapply(Q1,is.numeric))

to_predict <- c('adj_actual')

not_predictors <- c("PERMNO", "CFACPR", "CFACSHR", 'VALUE',
                    "FPI", "adj_actual", "USFIRM", "ANN_CFACSHR")

predictors <- setdiff(names(Q1)[numeric_vars], union(to_predict, not_predictors))
vars_winsorize <- c(predictors)
vars_to_keep_loop <-  union(c('rankdate', 'IPI', 'RGDP', 'RPC', 'UNEMP'), union(to_predict, predictors))
vars_to_keep_test <- c('rankdate', 'PERMNO','IPI', 'RGDP', 'RPC', 'UNEMP', predictors, to_predict)
vars_rf <- c('IPI', 'RGDP', 'RPC', 'UNEMP', to_predict, predictors)

begin_year <- 1986
last_year <- 2019

###############################################################
# handling missing values
###############################################################
for (p in periods){
  dt <- get(p)
  setkey(dt, rankdate, PERMNO)
  unique_dates <- dt[year(STATPERS) %>% between(begin_year, last_year), sort(unique(STATPERS))]
    
  l_dates <- length(unique_dates)
  
  length_loop <- l_dates
  
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
  # sort(dt %>% summarise(across(everything(), ~sum(is.na(.)))))
  # Winsorize at the 1% level and standardize
  dt[, (vars_winsorize) := lapply(vars_winsorize, function(x) {
    x <- get(x)
    
    x <- Winsorize(x, na.rm = TRUE, probs = c(.01,.99))
    return(x)
  }), by = rankdate]
  # there are still few missing value, omit them would not cost much (check with the summarise function)
  dt <- na.omit(dt)
  
  ####################
  
  gc()
  
  ###############################################################
  # Running random forest
  ###############################################################
  
  for (i in 1:length_loop) {
    #####
    last_loop <- unique_dates[i] # would minus one later
    first_loop <- unique_dates[i] %m-% months(ifelse(p == 'A2', 24, 12))
    last_loop_test <- unique_dates[i] # e.g., when last_loop = 200201, then observable data only last to 200112 
    
    ipi_ob_date <- c('DATE', year_month('IPT', last_loop))
    rgdp_ob_date <- c('DATE', year_month('ROUTPUT', last_loop))
    rpc_ob_date <- c('DATE', year_month('RCON', last_loop))
    unemp_ob_date <- c('DATE', RUC_year_quarter(last_loop))
    I = ipi[, ..ipi_ob_date]
    names(I)[2] = 'IPI'
    I$IPI <- ifelse(is.na(I$IPI),real_ipi$ipi, I$IPI) # fill the test data
    G = rgdp[, ..rgdp_ob_date]
    names(G)[2] = 'RGDP'
    G$RGDP <- ifelse(is.na(G$RGDP), real_rgdp$rgdp, G$RGDP)
    P = rpc[, ..rpc_ob_date]
    names(P)[2] = 'RPC'
    P$RPC <- ifelse(is.na(P$RPC), real_rpc$rpc, P$RPC)
    U = unemp[, ..unemp_ob_date]
    names(U)[2] = 'UNEMP'
    U$UNEMP <- ifelse(is.na(U$UNEMP), real_unemp$unemp, U$UNEMP)
    dt <- merge(dt, I, by.x = 'rankdate', by.y = 'DATE') %>% 
      merge(G, by.x = 'rankdate', by.y = 'DATE') %>% 
      merge(P, by.x = 'rankdate', by.y = 'DATE') %>% 
      merge(U, by.x = 'rankdate', by.y = 'DATE') 
   
    
    print(Sys.time())
    print(c(first_loop, last_loop %m-% months(1), last_loop_test))
    #####
    train_loop <- dt[YearMonth(STATPERS) >= YearMonth(first_loop)
                     & YearMonth(STATPERS) < YearMonth(last_loop),
                     ..vars_to_keep_loop]
    
    print(train_loop[, .N])
    
    test_loop <- dt[YearMonth(STATPERS) == YearMonth(last_loop_test), ..vars_to_keep_test]
    useful_col <- names(test_loop)[complete.cases(t(test_loop))]
    
    train_data <- as.data.frame(train_loop[   , ..vars_rf])
    col_train <- intersect(names(train_data)[complete.cases(t(train_data))], useful_col)
    train_data <- train_data[, col_train]
    
    # Default preprocessing is scaling, so no extra specified parameter
    standardization <- preProcess(train_data)
    train_data <- predict(standardization, train_data)
    
    col_test <- union(c('rankdate', 'PERMNO'), col_train)
    test_loop <- test_loop[, ..col_test]
    test_loop <- predict(standardization, test_loop)
    print(test_loop[, .N])
    
    #####
    # Linear Regression
    linear_model <- lm(formula = adj_actual ~ .,
                       data = train_data)

    test_loop[, Linear_forecast :=  predict(linear_model, test_loop)]
    test_loop[, realized_linear_bias := adj_actual - Linear_forecast]

    #####
    # Random Forest
    
    model_corrected <- ranger(formula = adj_actual ~ .,
                         replace = FALSE,
                         sample.fraction = ifelse(p %in% c('A2'), 0.15, ifelse(p %in% c('A1', 'Q1'), 0.6, 0.02)),
                         oob.error = FALSE,
                         importance = 'impurity_corrected',
                         data = train_data,
                         num.trees =  2000,
                         max.depth = ifelse(p %in% c('Q2', 'Q3'), 10, 15),
                         min.node.size = 5)
    vimp <- sort(model_corrected$variable.importance, TRUE)
    predictors_imp <- names(vimp[1:3])
    print(vimp[1:3])
    
    vimp <- as.data.frame(vimp)
    names(vimp) <- last_loop
    vimp$fin_ratio <- row.names(vimp)
    if (i == 1){
      mvimp <- vimp
    } else{
      mvimp <- merge(mvimp, vimp, by='fin_ratio', all = TRUE)
    }
    
    model_prediction <- ranger(formula = adj_actual ~ .,
                              replace = FALSE,
                              sample.fraction = ifelse(p %in% c('A2'), 0.15, ifelse(p %in% c('A1', 'Q1'), 0.6, 0.02)),
                              oob.error = FALSE,
                              always.split.variables = predictors_imp,
                              data = train_data,
                              num.trees =  2000,
                              max.depth = ifelse(p %in% c('Q2', 'Q3'), 10, 15),
                              min.node.size = 5)
    
    test_loop[, predictions_rf := predict(model_prediction, test_loop)$predictions]
    dt[, c('IPI', 'RGDP', 'RPC', 'UNEMP') := NULL]
    setkey(dt, rankdate, PERMNO)
    setkey(test_loop, rankdate, PERMNO)
    dt[test_loop, c('Linear_Forecast', 'realized_linear_bias', 'predict_rf_mean') := .(Linear_forecast, realized_linear_bias, predictions_rf)]
    gc()
    # break # Comment to run all
  }
  assign(p, dt)
  assign(paste('mvimp', p, sep = ''), mvimp)
}


A1 %>% fwrite('A1_RF.csv')
A2 %>% fwrite('A2_RF.csv')
Q1 %>% fwrite('Q1_RF.csv')
Q2 %>% fwrite('Q2_RF.csv')
Q3 %>% fwrite('Q3_RF.csv')

mvimpA1 %>% fwrite('A1_imp.csv')
mvimpA2 %>% fwrite('A2_imp.csv')
mvimpQ1 %>% fwrite('Q1_imp.csv')
mvimpQ2 %>% fwrite('Q2_imp.csv')
mvimpQ3 %>% fwrite('Q3_imp.csv')

#################################
# Generate importance graphs
#################################

A1_imp <- fread('A1_imp.csv', header = TRUE)
A2_imp <- fread('A2_imp.csv', header = TRUE)
Q1_imp <- fread('Q1_imp.csv', header = TRUE)
Q2_imp <- fread('Q2_imp.csv', header = TRUE)
Q3_imp <- fread('Q3_imp.csv', header = TRUE)

for (p in periods){
  vimp <- get(paste(p, '_imp', sep = ''))
  vimp <- melt(vimp, id = 'fin_ratio', variable.name = 'date')
  vimp <- as.data.table(vimp %>% group_by(fin_ratio) %>% summarise(imp = mean(value, na.rm = TRUE)))
  vimp[, sum_importance := sum(pmax(imp, 0))]
  vimp[, vimp_normalized := imp / sum_importance]
  setorder(vimp, -vimp_normalized)
  plot_features <- vimp[1:10] %>% ggplot(aes(x = reorder(fin_ratio, imp), y = vimp_normalized)) +
    ylim(0, .25) + xlab("") +
    ylab("Feature Importance") + geom_col(fill = "lightblue") + coord_flip()
  ggsave(paste0('FeatureImportance', p, '.png'), plot_features, width = 16, height = 9)
  
}


