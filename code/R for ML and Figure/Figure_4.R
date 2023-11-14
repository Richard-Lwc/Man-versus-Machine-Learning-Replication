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
# set.seed(1)
###############################################################
# Libraries
###############################################################
library(data.table)
library(lubridate)
library(ranger)
library(DescTools)
library(dplyr)
library(ranger)
library(caret)
library(iml)
library(npregfast)
library(pdp)
library(vip)
library(ggplot2)
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



A1 <- fread('A1_.csv')
A2 <- fread('A2_.csv')
Q1 <- fread('Q1_.csv')
Q2 <- fread('Q2_.csv')
Q3 <- fread('Q3_.csv')


periods <- c('A2', 'Q2', 'Q3')

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

numeric_vars <- which(sapply(A1,is.numeric))

to_predict <- c('adj_actual')

not_predictors <- c("PERMNO", "CFACSHR", 'VALUE',
                    "FPI", "adj_actual", "USFIRM", "ANN_CFACSHR")

predictors <- setdiff(names(A1)[numeric_vars], union(to_predict, not_predictors))

vars_winsorize <- c(predictors)
vars_to_keep_loop <-  union(c('rankdate', 'IPI', 'RGDP', 'RPC', 'UNEMP'), union(to_predict, predictors))
vars_to_keep_test <- c('rankdate', 'PERMNO','IPI', 'RGDP', 'RPC', 'UNEMP', predictors, to_predict)
vars_rf <- c('IPI', 'RGDP', 'RPC', 'UNEMP', to_predict, predictors)

###############################################################
# handling missing values
###############################################################
for (p in periods){
  dt <- get(p)
  setkey(dt, rankdate, PERMNO)
  # last_loop: 1986.1
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
  # dt %>% summarise(across(everything(), ~sum(is.na(.))))
  dt[, (vars_winsorize) := lapply(vars_winsorize, function(x) {
    x <- get(x)
    
    x <- Winsorize(x, na.rm = TRUE, probs = c(.01,.99))
    return(x)
  }), by = rankdate]
  # there are still few missing value, omit them would not cost much (check with the summarise function)
  dt <- na.omit(dt)
  # until January 1986, thus should use 198602 since at this time we could get data in January 1986
  last_loop <- dt[YearMonth(STATPERS) == 198602, sort(unique(STATPERS))]
  first_loop <- last_loop %m-% months(ifelse(p == 'A2', 24, 12))
  last_loop_test <- last_loop
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

  # Winsorize at the 1% level and standardize

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
  
  # To avoid training and test set have different available columns
  col_train <- intersect(names(train_data)[complete.cases(t(train_data))], useful_col)
  train_data <- train_data[, col_train]

  # Default prepocessing is scaling
  standardization <- preProcess(train_data)
  train_data <- predict(standardization, train_data)
  col_test <- union(c('rankdate', 'PERMNO'), col_train)
  test_loop <- test_loop[, ..col_test]
  test_loop <- predict(standardization, test_loop)

  print(test_loop[, .N])
  #####

  # Random Forest
  nboots <- 8

  ###########
  # ntrees
  cvtrees <- c(1, 25, c(1:6)*50, c(4:10)*100, c(6:10)*200)
  cvtres_result <- matrix(0, length(cvtrees), nboots)

  for (indexcv in 1:length(cvtrees)) {
    ntrees <- cvtrees[indexcv]
    for (indexb in 1:nboots) {
      model_corrected <- ranger(formula = adj_actual ~ .,
                                replace = FALSE,
                                oob.error = FALSE,
                                importance = 'impurity_corrected',
                                max.depth = 7,
                                sample.fraction = ifelse(p %in% c('A2'), 0.15, ifelse(p %in% c('A1'), 0.05, 0.01)),
                                data = train_data,
                                num.trees =  ntrees,
                                min.node.size = 5)
      vimp <- sort(model_corrected$variable.importance, TRUE)
      predictors_imp <- names(vimp[1:5])

      model_prediction <- ranger(formula = adj_actual ~ .,
                                 replace = FALSE,
                                 oob.error = FALSE,
                                 always.split.variables = predictors_imp,
                                 max.depth = 7,
                                 sample.fraction = ifelse(p %in% c('A2'), 0.15, ifelse(p %in% c('A1'), 0.05, 0.01)),
                                 data = train_data,
                                 num.trees = ntrees,
                                 min.node.size = 5)

      test_loop[, predictions_rf := predict(model_prediction, test_loop)$predictions]
      cvtres_result[indexcv, indexb] <- oosR2 <- test_loop[, 1 - mean((adj_actual - predictions_rf)^2)/(mean((adj_actual - mean(adj_actual))^2))]

    }
    print(c(indexcv, mean(cvtres_result[indexcv, ])))
  }
  # avg_cv_r2 <- apply(cvtres_result, 1, mean)
  dtcv <- as.data.table(cvtres_result)
  dtcv[, nforest := cvtrees]

  # fwrite(dtcv, 'cvntrees.csv')
  # dtcv <- fread('cvntrees.csv')
  test_data_long <- melt(dtcv, id="nforest")
  plot_cv <- ggplot(data=test_data_long, aes(x=nforest, y=value)) +
    stat_summary(fun.data ="mean_cl_normal", geom = 'smooth', se = TRUE) +
    labs(x = 'Number of Trees', y = 'Out-of-Sample R2')
  # se: 绘制置信区间, mean_cl_normal: 计算置信区间
  plot_cv
  ggsave(paste0('CVNTrees', p, '.png'), plot_cv, width = 16, height = 9)

  ###########
  # depth
  ###########
  cvdepth <- c(1:20)
  cvdepth_result <- matrix(0, length(cvdepth), nboots)
  for (indexcv in 1:length(cvdepth)) {
    ndepth <- cvdepth[indexcv]
    for (indexb in 1:nboots) {
      model_corrected <- ranger(formula = adj_actual ~ .,
                                replace = FALSE,
                                oob.error = FALSE,
                                importance = 'impurity_corrected',
                                sample.fraction = ifelse(p %in% c('A2'), 0.7, ifelse(p %in% c('A1', 'Q1'), 0.6, 0.02)),
                                data = train_data,
                                num.trees = 2000,
                                max.depth = ndepth,
                                min.node.size = 5)
      vimp <- sort(model_corrected$variable.importance, TRUE)
      predictors_imp <- names(vimp[1:5])


      model_prediction <- ranger(formula = adj_actual ~ .,
                                 replace = FALSE,
                                 oob.error = FALSE,
                                 always.split.variables = predictors_imp,
                                 sample.fraction = ifelse(p %in% c('A2'), 0.15, ifelse(p %in% c('A1', 'Q1'), 0.6, 0.02)),
                                 data = train_data,
                                 num.trees = 2000,
                                 max.depth = ndepth,
                                 min.node.size = 5)

      test_loop[, predictions_rf := predict(model_prediction, test_loop)$predictions]
      cvdepth_result[indexcv, indexb] <- oosR2 <- test_loop[, 1 - mean((adj_actual - predictions_rf)^2)/(mean((adj_actual - mean(adj_actual))^2))]

    }
    print(c(indexcv, mean(cvdepth_result[indexcv, ])))
  }
  # avg_cv_r2 <- apply(cvdepth_result, 1, mean)
  dtcv <- as.data.table(cvdepth_result)
  dtcv[, ndepth := cvdepth]
  test_data_long <- melt(dtcv, id="ndepth")
  plot_cv <- ggplot(data=test_data_long, aes(x=ndepth, y=value)) +
    stat_summary(fun.data ="mean_cl_normal", geom = 'smooth', se = TRUE) +
    labs(x = 'Depth', y = 'Out-of-Sample R2')
  plot_cv
  ggsave(paste0('CVndepth', p,  '.png'), plot_cv, width = 16, height = 9)

  # #########
  # nperc
  # #########
  cvperc <- c(c(1:15), c(2:10)*10)/100
  cvperc_result <-matrix(0, length(cvperc), nboots)

  for (indexcv in 1:length(cvperc)) {
    nperc <- cvperc[indexcv]
    for (indexb in 1:nboots) {
      model_corrected <- ranger(formula = adj_actual ~ .,
                                replace = FALSE,
                                sample.fraction = nperc,
                                max.depth = 18,
                                oob.error = FALSE,
                                importance = 'impurity_corrected',
                                data = train_data,
                                num.trees = 2000,
                                min.node.size = 5
                                )
      vimp <- sort(model_corrected$variable.importance, TRUE)
      predictors_imp <- names(vimp[1:5])

      model_prediction <- ranger(formula = adj_actual ~ .,
                                 replace = FALSE,
                                 sample.fraction = nperc,
                                 max.depth = 18,
                                 oob.error = FALSE,
                                 always.split.variables = predictors_imp,
                                 data = train_data,
                                 num.trees = 2000,
                                 min.node.size = 5
                                 )

      test_loop[, predictions_rf := predict(model_prediction, test_loop)$predictions]
      cvperc_result[indexcv, indexb] <- oosR2 <- test_loop[, 1 - mean((adj_actual - predictions_rf)^2)/(mean((adj_actual - mean(adj_actual))^2))]

    }
    print(c(indexcv, mean(cvperc_result[indexcv, ])))
  }
  # avg_cv_r2 <- apply(cvperc_result, 1, mean)
  dtcv <- as.data.table(cvperc_result)
  dtcv[, nperc := cvperc]
  # fwrite(dtcv, 'cvnperc.csv')
  # dtcv <- fread('cvnperc.csv')
  test_data_long <- melt(dtcv, id="nperc")
  plot_cv <- ggplot(data=test_data_long, aes(x=nperc, y=value)) +
    stat_summary(fun.data ="mean_cl_normal", geom = 'smooth', se = TRUE)  +
    labs(x = 'Fractions of observations to sample', y = 'Out-of-Sample R2')
  plot_cv
  ggsave(paste0('CVnperc', p,  '.png'), plot_cv, width = 16, height = 9)
}


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



