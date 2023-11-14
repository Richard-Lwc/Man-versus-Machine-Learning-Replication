********Table 2 The term structure of earnings forecasts via machine learning**************

*... is your local directory*
cd "C:\\Users\\ASUS\\Desktop\\Man versus ML"
clear matrix 
matrix tbl = J(10,9, .)
* 10行9列的矩阵
matrix colnames tbl =  "RF" "AF" "AE" "(RF-AE)" "(AF-AE)" "(RF-AE)^2" "(AF-AE)^2" "(AF-RF)/P" "N"

matrix rownames tbl = "One-quarter-ahead" "\textit{t}-stat" "Two-quarters-ahead" "\textit{t}-stat" "Three-quarters-ahead" "\textit{t}-stat" "One-year-ahead" "\textit{t}-stat" "Two-years-ahead" "\textit{t}-stat" 
* \textit斜体

import excel using "Q1_FE.xlsx", ///
firstrow clear
gen t=_n
tsset t

local port predict_rf_mean	analyst_forecast adj_actual	RF_dif	af_dif	sqr_RF_dif  sqr_af_dif bias count
forvalues i=1/9 {
local var: word `i' of `port'
reg `var' 
* reg只跟一个变量是为了求均值以及方便地求标准差
matrix tbl[1,`i'] = _b[_cons]
}
*_cons is always equal to the number 1 when used directly and refers to the intercept term when used indirectly, as in _b[_cons].
newey RF_dif, lag(3)
matrix tbl[2,4] =_b[_cons]/_se[_cons]

newey af_dif, lag(3)
matrix tbl[2,5] =_b[_cons]/_se[_cons]

newey bias, lag(3)
matrix tbl[2,8] =_b[_cons]/_se[_cons]



import excel using "Q2_FE.xlsx", ///
firstrow clear
gen t=_n
tsset t

local port predict_rf_mean	analyst_forecast adj_actual	RF_dif	af_dif	sqr_RF_dif	sqr_af_dif bias	count
forvalues i=1/9 {
local var: word `i' of `port'
reg `var' 
matrix tbl[3,`i'] = _b[_cons]
}

newey RF_dif, lag(3)
matrix tbl[4,4] =_b[_cons]/_se[_cons]

newey af_dif, lag(3)
matrix tbl[4,5] =_b[_cons]/_se[_cons]

newey bias, lag(3)
matrix tbl[4,8] =_b[_cons]/_se[_cons]


import excel using "Q3_FE.xlsx", ///
firstrow clear
gen t=_n
tsset t

local port predict_rf_mean	analyst_forecast adj_actual	RF_dif	af_dif	sqr_RF_dif	 sqr_af_dif	bias count
forvalues i=1/9 {
local var: word `i' of `port'
reg `var' 
matrix tbl[5,`i'] = _b[_cons]
}

newey RF_dif, lag(3)
matrix tbl[6,4] =_b[_cons]/_se[_cons]

newey af_dif, lag(3)
matrix tbl[6,5] =_b[_cons]/_se[_cons]

newey bias, lag(3)
matrix tbl[6,8] =_b[_cons]/_se[_cons]




import excel using "A1_FE.xlsx", ///
firstrow clear
gen t=_n
tsset t

local port predict_rf_mean	analyst_forecast adj_actual	RF_dif	af_dif	sqr_RF_dif	sqr_af_dif bias	count
forvalues i=1/9 {
local var: word `i' of `port'
reg `var' 
matrix tbl[7,`i'] = _b[_cons]
}

newey RF_dif, lag(12)
matrix tbl[8,4] =_b[_cons]/_se[_cons]

newey af_dif, lag(12)
matrix tbl[8,5] =_b[_cons]/_se[_cons]

newey bias, lag(12)
matrix tbl[8,8] =_b[_cons]/_se[_cons]



import excel using "A2_FE.xlsx", ///
firstrow clear
gen t=_n
tsset t

local port predict_rf_mean	analyst_forecast adj_actual	RF_dif	af_dif	sqr_RF_dif 	sqr_af_dif bias	count
forvalues i=1/9 {
local var: word `i' of `port'
reg `var' 
matrix tbl[9,`i'] = _b[_cons]
}

newey RF_dif, lag(12)
matrix tbl[10,4] =_b[_cons]/_se[_cons]

newey af_dif, lag(12)
matrix tbl[10,5] =_b[_cons]/_se[_cons]

newey bias, lag(12)
matrix tbl[10,8] =_b[_cons]/_se[_cons]

frmttable using Table2, statmat(tbl)  sdec(3\2\3\2\3\2\3\2\3\2)

