********Table 5 Portfolios sorted on conditional bias**************

*... is your local directory*
cd "C:/Users/ASUS/Desktop/data"

clear
clear matrix

matrix A = J(6, 6, .)
matrix rownames A =  "Mean" "\textit{t}-stat" "CAPM Beta"  "Mean"  "\textit{t}-stat"  "CAPM Beta"


import excel using "ret_avg_bias_equal.xlsx", ///
firstrow clear

gen t=_n
tsset t

***mean***
local port B C D E F LS
forvalues i=1/6 {
local var: word `i' of `port'
reg `var' , vce(robust)
matrix A[1,`i'] = _b[_cons]*100
matrix A[2,`i'] =_b[_cons]/_se[_cons]
reg `var'  Mkt, vce(robust)
matrix A[3,`i'] = _b[Mkt]*100
}


import excel using "ret_port_score_equal.xlsx", ///
firstrow clear

gen t=_n
tsset t

***mean***
local port B C D E F LS
forvalues i=1/6 {
local var: word `i' of `port'
reg `var' , vce(robust)
matrix A[4,`i'] = _b[_cons]*100
matrix A[5,`i'] =_b[_cons]/_se[_cons]
reg `var'  Mkt, vce(robust)
matrix A[6,`i'] = _b[Mkt]*100
}

frmttable using Table5_equal, statmat(A) sdec(2\2\2\2\2\2) ctitle("Quintile", "1", "2", "3", "4", "5", "5-1")




********Table 6 Time-series tests with common asset pricing models**************

import excel using "ret_avg_bias_equal.xlsx", ///
firstrow clear

gen t=_n
tsset t

 
clear matrix 
matrix tbl = J(6,6, .)

matrix rownames tbl =  Intercept Mkt_RF SMB HML RMW CMA


***CAPM alpha***
reg LS Mkt, vce(robust) 
matrix tbl[1,1] = _b[_cons]*100
matrix tbl[1,2] =_b[_cons]/_se[_cons]
matrix tbl[2,1] = _b[Mkt]*100
matrix tbl[2,2] =_b[Mkt]/_se[Mkt]


***FF3 alpha***
reg LS Mkt SMB HML, vce(robust) 
matrix tbl[1,3] = _b[_cons]*100 
matrix tbl[1,4] =_b[_cons]/_se[_cons]
matrix tbl[2,3] = _b[Mkt]*100
matrix tbl[2,4] =_b[Mkt]/_se[Mkt]
matrix tbl[3,3] = _b[SMB]*100 
matrix tbl[3,4] =_b[SMB]/_se[SMB]
matrix tbl[4,3] = _b[HML]*100 
matrix tbl[4,4] =_b[HML]/_se[HML]


***FF5 alpha***
reg LS Mkt SMB HML RMW CMA, vce(robust) 
matrix tbl[1,5] = _b[_cons]*100 
matrix tbl[1,6] =_b[_cons]/_se[_cons]
matrix tbl[2,5] = _b[Mkt]*100
matrix tbl[2,6] =_b[Mkt]/_se[Mkt]
matrix tbl[3,5] = _b[SMB]*100
matrix tbl[3,6] =_b[SMB]/_se[SMB]
matrix tbl[4,5] = _b[HML]*100
matrix tbl[4,6] =_b[HML]/_se[HML]
matrix tbl[5,5] = _b[RMW]*100
matrix tbl[5,6] =_b[RMW]/_se[RMW]
matrix tbl[6,5] = _b[CMA]*100
matrix tbl[6,6] =_b[CMA]/_se[CMA]



frmttable, clear
frmttable using Table6A_equal, statmat(tbl) title("A.Average BE")







import excel using "ret_port_score_equal.xlsx", ///
firstrow clear

gen t=_n
tsset t

 
clear matrix 
matrix tbl = J(6,6, .)

matrix rownames tbl =  Intercept Mkt_RF SMB HML RMW CMA



***CAPM alpha***
reg LS Mkt, vce(robust) 
matrix tbl[1,1] = _b[_cons]*100
matrix tbl[1,2] =_b[_cons]/_se[_cons]
matrix tbl[2,1] = _b[Mkt]*100
matrix tbl[2,2] =_b[Mkt]/_se[Mkt]



***FF3 alpha***
reg LS Mkt SMB HML, vce(robust) 
matrix tbl[1,3] = _b[_cons]*100 
matrix tbl[1,4] =_b[_cons]/_se[_cons]
matrix tbl[2,3] = _b[Mkt]*100
matrix tbl[2,4] =_b[Mkt]/_se[Mkt]
matrix tbl[3,3] = _b[SMB]*100 
matrix tbl[3,4] =_b[SMB]/_se[SMB]
matrix tbl[4,3] = _b[HML]*100 
matrix tbl[4,4] =_b[HML]/_se[HML]


***FF5 alpha***
reg LS Mkt SMB HML RMW CMA, vce(robust) 
matrix tbl[1,5] = _b[_cons]*100 
matrix tbl[1,6] =_b[_cons]/_se[_cons]
matrix tbl[2,5] = _b[Mkt]*100
matrix tbl[2,6] =_b[Mkt]/_se[Mkt]
matrix tbl[3,5] = _b[SMB]*100
matrix tbl[3,6] =_b[SMB]/_se[SMB]
matrix tbl[4,5] = _b[HML]*100
matrix tbl[4,6] =_b[HML]/_se[HML]
matrix tbl[5,5] = _b[RMW]*100
matrix tbl[5,6] =_b[RMW]/_se[RMW]
matrix tbl[6,5] = _b[CMA]*100
matrix tbl[6,6] =_b[CMA]/_se[CMA]

frmttable, clear
frmttable using Table6B_equal, statmat(tbl) title("B.BE score")

 

 

