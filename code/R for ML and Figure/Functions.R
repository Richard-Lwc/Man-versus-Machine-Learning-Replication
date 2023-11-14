###############################################################################
# Functions
###############################################################################
f.round.up.month <- function(datadate){
  x <- ceiling_date(datadate, 'month') - 1
  return(x)
}

f.add.month.end <- function(datadate, nmonths = 1){
  x <- ceiling_date(add_with_rollback(datadate, months(nmonths),
                                      roll_to_first = FALSE,
                                      preserve_hms = FALSE), 'month') - 1 
  return(x)
}

f.names.lags <- function(name_list, lags = 1:4){
  return(paste(name_list, 'lag', lags, sep = '_'))
}

f.replace.na.std <- function(x){
  mad_x <- mad(x, na.rm = TRUE)
  if(is.na(mad_x)){
    return(x)
  }
  med_x <- median(x, na.rm = TRUE)
  x <- f.na.replace.median(x)
  if (mad_x != 0){
    x <- (x - med_x)/mad_x
    x <- winsorize(x, standardized = TRUE)
  }
  return(as.numeric(x))
}
f.na.replace.median <- function(value){
  return(ifelse(is.na(value), median(value, na.rm=TRUE), value))
}
f.na.replace.0 <- function(value){
  return(ifelse(is.na(value), 0, value))
}
f.min.not.0 <- function(x){return(min(x, na.rm = TRUE) != 0)}
f.robust.stand <- function(x){
  if (mad(x) == 0){
    return(x)
  }else{
    return(robStandardize(x))
  }
}
f.robust.stand.na <- function(x){
  if (mad(x) == 0){
    return(rep(NA, length(x)))
  }else{
    return(robStandardize(x))
  }
}

f.alpha.t.double <- function(alpha.v.double, ntiles, ntiles2){
  a.t <- matrix(t(alpha.v.double), nrow = ntiles*2, ncol = ntiles2)
  temp <- paste0("Quintile_2_", c(1:ntiles2))
  names.a.t <- temp[floor(seq(1,ntiles2 +.5 ,.5))]
  names.a.t[seq(1,ntiles2 +.5 ,.5) %% 1 != 0] <- "remove"
  rownames(a.t) <- names.a.t
  colnames(a.t) <- paste0("Quintile_1_", c(1:ntiles))
  return(a.t)
}

robust.errors.list <- function(list.reg){
  
  test <- unlist(list.reg, recursive = FALSE)
  l <- length(list.reg)
  robust.std.l <- vector(mode = "list", length = l)
  for(i in 1:l){
    cov.temp <- vcovHAC(test[[i]])
    robust_se.temp    <- sqrt(diag(cov.temp))
    robust.std.l[[i]] <- robust_se.temp
  }
  return(unlist(robust.std.l, recursive = FALSE))
}
robust.errors <- function(list.reg){
  
  if (typeof(list.reg[[1]]) == "list"){
    test <- unlist(list.reg, recursive = FALSE)
    l <- length(list.reg)
  } else{test <- list(list.reg)
  l <- 1}
  robust.std.l <- vector(mode = "list", length = l)
  for(i in 1:l){
    cov.temp <- vcovHAC(test[[i]])
    robust_se.temp    <- sqrt(diag(cov.temp))
    robust.std.l[[i]] <- robust_se.temp
  }
  return(robust.std.l)
}
table.format.ref <- function(list.reg){
  name.table <- paste0(deparse(substitute(list.reg)), ".tex")
  l.std.errors <- robust.errors(list.reg)
  list.reg.to.format <- unlist(list.reg, recursive = FALSE)
  stargazer(list.reg.to.format, align=TRUE, font.size = "small",
            report = "vc*t", se = l.std.errors,  omit.stat = c("f", "ser"), 
            intercept.bottom = FALSE,
            summary = FALSE, rownames = FALSE, out = name.table)
}
f.capm.ort <- function(dt){
  a <- lm(trt1m - RF ~ `Mkt-RF`, data = dt)
  e <- a$residuals + a$coefficients[1]
  return(e)
}

f.roll.mult <- function(r){
  dt <- as.matrix(1 + r/100)
  n <- length(dt)
  if ( n < 12){return(rep(NaN, n))}
  return((roll_prod(dt, 12)-1)*100)
}

f.roll.mult.w <- function(r, w){
  dt <- as.matrix(1 + r/100)
  n <- length(dt)
  if ( n < w){return(rep(NaN, n))}
  return((roll_prod(dt, w)-1))
}

f.roll.mean <- function(r, per){
  dt <- as.matrix(1 + r/100)
  n <- length(dt)
  if ( n < 12){return(rep(NaN, n))}
  return((rollmean(dt, 12)-1)*100)
}
f.roll.5 <- function(dt, wind = 60){
  n <- dt[, .N]
  if ( wind > n){return(rep(NaN, n))}
  z3.FM.xts.x <- as.xts(dt[, .(date_match, `Mkt-RF`)])#as.xts(s.x)
  z3.FM.xts.y <- as.xts(dt[, .(date_match,trt1m - RF)])  #as.xts(s.y)
  result <- roll_lm(z3.FM.xts.x, z3.FM.xts.y, wind)
  test.re <- as.data.table(result$coefficients[, 2])[, `Mkt-RF`]
  return(test.re)
}
f.roll.5 <- function(dt, wind = 60){
  n <- dt[, .N]
  if ( wind > n){return(rep(NaN, n))}
  z3.FM.xts.x <- as.xts(dt[, .(date_match, `Mkt-RF`)])#as.xts(s.x)
  z3.FM.xts.y <- as.xts(dt[, .(date_match,trt1m - RF)])  #as.xts(s.y)
  result <- roll_lm(z3.FM.xts.x, z3.FM.xts.y, wind)
  test.re <- as.data.table(result$coefficients[, 2])[, `Mkt-RF`]
  return(test.re)
}

setcolfirst = function(DT, ...){
  nm = as.character(substitute(c(...)))[-1L]
  setcolorder(DT, c(nm, setdiff(names(DT), nm)))
}

f.alpha.matrix <- function(alpha.list, ntiles, ntiles2){
  a.matrix <- matrix(alpha.list[,1], nrow = ntiles2, ncol = ntiles)
  rownames(a.matrix) <- paste0("Quartile2_", c(1:ntiles2))
  colnames(a.matrix) <- paste0("Quartile1_", c(1:ntiles))
  return(a.matrix)
}
f.t.matrix <- function(alpha.list, ntiles, ntiles2){
  a.matrix <- matrix(alpha.list[,2], nrow = ntiles2, ncol = ntiles)
  rownames(a.matrix) <- paste0("Quartile2_", c(1:ntiles2))
  colnames(a.matrix) <- paste0("Quartile1_", c(1:ntiles))
  return(a.matrix)
}
quantile.inf.f <- function(x, n){
  #n <- ntiles -1
  a <- quantile(x, probs = seq(0, 1, by = 1/n),
                na.rm = T, names = F)
  return(c(- Inf, a[2:n], Inf))
}
f.quartile <- function(ntiles, dt, var_sort, v){
  pmin(ntiles, pmax(1, findInterval(v,
                                    quantile(dt[ exchg == 11, # NYSE
                                                get(var_sort)],
                                             probs = seq(0, 1, by = 1/ntiles),
                                             na.rm = TRUE))))
}
f.quartile.all <- function(ntiles, dt, var_sort, v){
  pmin(ntiles, pmax(1, findInterval(v,
                                    quantile(dt[ ,
                                                 get(var_sort)],
                                             probs = seq(0, 1, by = 1/ntiles),
                                             na.rm = TRUE))))
}

f.cor.SD <- function(SD){
  # Computes average correlation for a set of securities
  # Average of individual securities
  return(mean(cor(SD), na.rm = TRUE))
}

f.interval.all <- function(ntiles, v){
  # Discretizes the probability in ntiles so if 20, first one would correspond to <= .5, two to .05<x<=.1...
  vect <- seq(0, 1, 1/ntiles)
  findInterval( v,  vect)
}

close_to  <- function(vect, n = .05){
  return(which.min((vect - n)^2))
}
f.which.n <- function(vect, n = .1){
  
}

f.interval.uneven <- function(ntiles, v){
  vect <- c(0,  .05, .25, 1)
  vect <- c(.05, seq(.1, 1, .3))
  vect <- c(.05,  seq(.1, .6, .1), 1)
  vect <- c(.02, .25, 1)
  # vect <- c(.025, .2, .4, .6, .8, 1)
  findInterval( v,  vect)
}

v <- seq(0, 1, .1)

findInterval(0, v)

f.quartile_list <- function(var_sort, ntiles, dt, v){
  pmin(ntiles, pmax(1, findInterval(v,
                                    quantile(dt[, # NYSE
                                                 get(var_sort)],
                                             probs = seq(0, 1, by = 1/ntiles),
                                             na.rm = TRUE))))
}
f.sharp <- function(x){
  return(mean(x, na.rm = T)*sqrt(12)/sd(x, na.rm = T))
}
f.sharpe.mult <- function(excess_return_matrix){
  means <- apply(excess_return_matrix, 2, mean)
  s2 <- means %*% solve(cov(excess_return_matrix)) %*% means
  s <- sqrt(s2)*sqrt(12)
  return(s)
}

f.n.shift.june <- function(fyr){
  date.month <- fyr
  n.shift <- (date.month < 6)*(6 - date.month) + (date.month > 5)*(18 -date.month)
  return(n.shift)
}
f.n.shift.june.alt <- function(fyr){
  date.month <- fyr
  n.shift <- 18 -date.month
  return(n.shift)
}
f.ntile_na <- function(var_name, n){
  return(ntile_na(get(var_name), n))
}
ntile_na <- function(x,n)
{
  notna <- !is.na(x)
  out <- rep(NA_real_,length(x))
  out[notna] <- ntile(x[notna],n)
  return(out)
}
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

f.alpha <- function(llm, n.f = 3){
  aux.1 <- unlist(llm, recursive = FALSE) # Models are accesible from here
  aux.2 <- lapply(aux.1, coeftest, vcov = vcovHAC)
  coeff.m <- matrix(unlist(aux.2, recursive = FALSE), nrow = length(llm), byrow = TRUE)
  t.number <- (n.f + 1)*2 + 1
  alpha.m <- coeff.m[, c(1, t.number)]
  return((alpha.m))
}

f.alpha.ind <- function(llm, n.f = 3){
  aux.2 <- lapply(llm, coeftest, vcov = vcovHAC)
  coeff.m <- matrix(unlist(aux.2, recursive = FALSE), nrow = length(llm), byrow = TRUE)
  t.number <- (n.f + 1)*2 + 1
  alpha.m <- coeff.m[, c(1, t.number)]
  return((alpha.m))
}
f.format.reg <- function(lm, title.name = "",
                         out.name = paste0(deparse(substitute(lm)), ".tex")){
  cov1         <- vcovHAC(lm)
  robust_se    <- sqrt(diag(cov1))
  stargazer(lm, font.size = "small",  report = "vc*t",   omit.stat = "f",
            se        = list(robust_se), intercept.bottom = FALSE,
            summary = FALSE, rownames = FALSE,
            title = title.name,
            out  = out.name)
}
f.market.share <-function(dt, by.name = "quartile",date.name = "fyear"){
  sum.name = paste0("total.market.cap.", by.name)
  dt[, (sum.name) := sum(market.cap), by = c(date.name, by.name)]
  share = paste0("share.", by.name)
  share.mkt.t <- dt[, .(share = get(sum.name)/total.market.cap),
                    by = c(date.name, by.name)][order(get(date.name))]
  share.mkt.t <- unique(share.mkt.t)
  share.mkt <- share.mkt.t[, mean(share), by = by.name][order(get(by.name))]
  return(share.mkt)
}
f.return.weights <-function(dt, by.name, date.name){
  dt[, total.market.cap.quar := sum(market.cap, na.rm = TRUE), by = c(date.name, by.name)]
  dt[, weight := market.cap/total.market.cap.quar ]
}

# source('Code/Functions_Portfolio.R')
# source('Code/Function_Weights.R')

f.return.one.way <- function(dt, return.name, by.name, date.name){
  dt.r.m <- dt[,.(returns = mean(get(return.name), na.rm = TRUE)),
               by = c(date.name, by.name)]
  dcast.str <-  paste(date.name, "~", by.name)
  dt.r <- dcast(dt.r.m, dcast.str, value.var = "returns")
  return(dt.r)
}
f.return.simple <- function(dt, return.name, date.name){
  dt.r.m <- dt[,.(returns = mean(get(return.name), na.rm = TRUE)),
               by = c(date.name)]
  return(dt.r.m)
}
f.return.simple.weights <- function(dt, return.name, weight.name, date.name){
  dt.r.m <- dt[,.(returns = weighted.mean(get(return.name),
                                          get(weight.name), na.rm = TRUE)),
               by = c(date.name)]
  return(dt.r.m)
}
f.return.one.way.cond <- function(dt, return.name, condition, date.name){
  dt.r.m <- dt[get(condition) == 1, .(returns = mean(get(return.name), na.rm = TRUE)),
               by = date.name][order(get(date.name))]
  return(dt.r.m)
}


f.return.one.way.cond.weights <- function(dt, return.name, weight.name, condition, date.name){
  dt.r.m <- dt[get(condition) == 1,.(returns = weighted.mean(get(return.name), get(weight.name), na.rm = TRUE)),
               by = date.name][order(get(date.name))]
  return(dt.r.m)
}

f.return.one.way.weights <- function(dt, return.name, weight.name,
                                     by.name, date.name){
  dt.r.m <- dt[,.(returns = weighted.mean(get(return.name),
                                          get(weight.name), na.rm = TRUE)),
               by = c(date.name, by.name)]
  dcast.str <-  paste(date.name, "~", by.name)
  dt.r <- dcast(dt.r.m, dcast.str, value.var = "returns")
  return(dt.r)
}
f.return.multiple.weights <- function(dt, return.name, weight.name, by.name, date.name){
  dt.r.m <- dt[,.(returns = weighted.mean(get(return.name), get(weight.name), na.rm = TRUE)),
               by = c(date.name, by.name)]
  # Make double groups
  dt.r.m[, grp := do.call(paste,c(.SD, sep = "_")), .SDcols = by.name]
  dcast.str <-  paste(date.name, "~", "grp")
  dt.r <- dcast(dt.r.m, dcast.str, value.var = "returns")
  return(dt.r)
}
f.return.multiple <- function(dt, return.name, by.name, date.name){
  dt.r.m <- dt[,.(returns = mean(get(return.name), na.rm = TRUE)),
               by = c(date.name, by.name)]
  # Make double groups
  dt.r.m[, grp := do.call(paste,c(.SD, sep = "_")), .SDcols = by.name]
  dcast.str <-  paste(date.name, "~", "grp")
  dt.r <- dcast(dt.r.m, dcast.str, value.var = "returns")
  return(dt.r)
}
f.sic.coarse.2 <- function(x){ # Converts from four digits to two
  x <- trunc(x/100)
  return(x)
}
f.sic.coarse.3 <- function(x){ # Converts from four digits to three
  x <- trunc(x/10)
  return(x)
}
f.datadate.date_match.ff <- function(dt){
  dt[, better_date := ymd(datadate)]
  dt[, try_date := ceiling_date(better_date, "month")]
  dt[, date_match := try_date - 1]
  setkey(dt, date_match)
}
f.dates.port <- function(dt, date.name = date_match){}
f.dates.year.month <- function(dt, date.name = "datadate", 
                               format.date = "%Y%m%d"){
  dt[, date:= (as.Date(as.character(get(date.name)) # Date realized
                       , format = format.date))]
  dt[, date.month := format(date, "%m" )]
  dt[, date.year := format(date, "%Y" )]
  setkey(dt, date.year, date.month)
}
f.dates.year.month.ff <- function(ff, date.name = "Date", format.date = "%Y%m%d"){
  ff[, datadate   := as.Date(paste(as.character(get(date.name)), "01", sep = "")
                             , format = format.date)]
  ff[, date.month := format(datadate, "%m" )]
  ff[, date.year  := format(datadate, "%Y" )]
  setkey(ff,date.year, date.month)
}
f.dates.year.month.qf <- function(ff, date.name = "Date", format.date = "%Y%m%d"){
  ff[, datadate   := as.Date(paste(as.character(get(date.name)), "01", sep = "")
                             , format = format.date)]
  ff[, date.month := format(datadate, "%m" )]
  ff[, date.year  := format(datadate, "%Y" )]
  setkey(ff,date.year, date.month)
}
fCumReturns <- function(r){ # Calculates cumulative returns
  #Returns in %
  l <- length(r) + 1
  cr <- rep(0, l)
  cr[1] <- 1
  for(i in 2:l){
    cr[i] <- cr[i-1]*(1+r[i-1]/100)
  }
  return(cr)
}
fCumReturnsRf <- function(r, rf){ # Calculates cumulative returns
  #Returns in %
  r <- r + rf
  l <- length(r) + 1
  cr <- rep(0, l)
  cr[1] <- 1
  for(i in 2:l){
    cr[i] <- cr[i-1]*(1+r[i-1]/100)
  }
  return(cr)
}

fLogReturns <- function(r){ # Takes r in 1 + r in %
  l.r <- 100*(log(1 + r/100))
  return(l.r)
}
fYearAvailableCompustat <- function(m, y){
  # Gives the year available to investors at June of the next year
  # for a filling in a year 
  if (m < 6){ # Jan -  May
    # (e.g. if in May 1963, use available in 1963,
    # porfolis formed in 1962)
    a.year <- y   - 1
  } else {
    # c.year <- as.numeric(j)
    a.year <- y
  }
}
ff1.reg <- function(dt, vars.include.reg, print.val = TRUE){
  models <- dt[, lapply(.SD, ff1.reg.single, dt, print.val),
               .SDcols = vars.include.reg]
  names(models) <- vars.include.reg
  return(models)
}
ff1.reg.single <- function(r, z, print.val = TRUE){
  model <- lm((r -`RF`) ~ `Mkt-RF`, data = z)
  if (print.val){print(summary(model))}
  return(list(model))
}
ff3.reg <- function(dt, vars.include.reg, print.val = TRUE){
  models <- dt[, lapply(.SD, ff3.reg.single, dt, print.val),
               .SDcols = vars.include.reg]
  names(models) <- vars.include.reg
  return(models)
}
rob.reg <- function(dt, vars.include.reg, print.val = TRUE){
  models <- dt[, lapply(.SD, rob.reg.single, dt, print.val),
               .SDcols = vars.include.reg]
  names(models) <- vars.include.reg
  return(models)
}
rob.lux.reg <- function(dt, vars.include.reg, print.val = TRUE){
  models <- dt[, lapply(.SD, rob.lux.reg.single, dt, print.val),
               .SDcols = vars.include.reg]
  names(models) <- vars.include.reg
  return(models)
}
qFactors.reg <- function(dt, vars.include.reg, print.val = TRUE){
  models <- dt[, lapply(.SD, qFactors.reg.single, dt, print.val),
               .SDcols = vars.include.reg]
  names(models) <- vars.include.reg
  return(models)
}
ff5.reg <- function(dt, vars.include.reg, print.val = TRUE){
  models <- dt[, lapply(.SD, ff5.reg.single, dt, print.val),
               .SDcols = vars.include.reg]
  names(models) <- vars.include.reg
  return(models)
}
sic58_20.reg <- function(dt, vars.include.reg, print.val = TRUE){
  models <- dt[, lapply(.SD, sic58_20.reg.single, dt, print.val),
               .SDcols = vars.include.reg]
  names(models) <- vars.include.reg
  return(models)
}
Lux.reg <- function(dt, vars.include.reg, print.val = TRUE){
  models <- dt[, lapply(.SD, Lux.reg.single, dt, print.val),
               .SDcols = vars.include.reg]
  names(models) <- vars.include.reg
  return(models)
}
out.stargazer <- function(output.file, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=TRUE)
}
ff3.reg.single <- function(r, z, print.val = TRUE){
  model <- lm((r -`RF`) ~ `Mkt-RF`+ SMB + HML, data = z)
  if (print.val){
    print(summary(model))
    }
  return(list(model))
}
rob.reg.single <- function(r, z, print.val = TRUE){
  model <- lm((r -`RF`) ~ MKTRF+ SMB + MGMT + PERF, data = z)
  if (print.val){
    print(summary(model))
  }
  return(list(model))
}

rob.lux.reg.single <- function(r, z, print.val = TRUE){
  model <- lm((r -`RF`) ~ MKTRF+ SMB + MGMT + PERF + Demand, data = z)
  if (print.val){
    print(summary(model))
  }
  return(list(model))
}
qFactors.reg.single <- function(r, z, print.val = TRUE){
  model <- lm((r -`RF`) ~ MKT+ ME + IA + ROE, data = z)
  if (print.val){
    print(summary(model))
  }
  return(list(model))
}
ff5.reg.single <- function(r, z, print.val = TRUE){
  # m.alpha <- matrix(NA,1,1)
  models <- lm((r -`RF`) ~ `Mkt-RF`+ SMB + HML + RMW + CMA, data = z)
  if (print.val){print(summary(models))}
  # m.alpha <- models[1,1]
  # m.alpha[2] <- models[1,3]
  return(list(models))
}
sic58_20.reg.single <- function(r, z, print.val = TRUE){
  # m.alpha <- matrix(NA,1,1)
  models <- lm((r -`RF`) ~ `Mkt-RF`+ SMB + HML + RMW + CMA + SIC58_21, data = z)
  if (print.val){print(summary(models))}
  # m.alpha <- models[1,1]
  # m.alpha[2] <- models[1,3]
  return(list(models))
}
Lux.reg.single <- function(r, z, print.val = TRUE){
  # m.alpha <- matrix(NA,1,1)
  models <- lm((r -`RF`) ~ `Mkt-RF`+ SMB + HML + RMW + CMA + Lux_RF, data = z)
  if (print.val){print(summary(models))}
  # m.alpha <- models[1,1]
  # m.alpha[2] <- models[1,3]
  return(list(models))
}



