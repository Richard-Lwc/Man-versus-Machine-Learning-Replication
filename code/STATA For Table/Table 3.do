
********Table 3 Fama-MacBeth Regressions**************
*... is your local directory*
cd "C:\\Users\\ASUS\\Desktop\\Man versus ML"


clear matrix 
matrix tbl = J(21,4, .)
matrix colnames tbl =  "(1)" "(2)" "(1)" "(2)" 

matrix rownames tbl = "Bias" "\textit{t}-stat" "LNsize" "\textit{t}-stat" "LNbeme" "\textit{t}-stat" "Ret1" "\textit{t}-stat" "Ret12_7" "\textit{t}-stat"  "IA" "\textit{t}-stat"   "IVOL" "\textit{t}-stat" "Retvol" "\textit{t}-stat"  "Turnover" "\textit{t}-stat"  "Intercept" "\textit{t}-stat"  "R-sqr (\%)"


import excel using "FMret_avg1.xlsx", ///
firstrow clear


qui reg avg_bias, vce(robust)
matrix tbl[1,1] = _b[_cons]
matrix tbl[2,1] =_b[_cons]/_se[_cons]

qui reg intercept , vce(robust)
matrix tbl[19,1] = _b[_cons]*100
matrix tbl[20,1] =_b[_cons]/_se[_cons]

qui reg r_sqr, vce(robust)
matrix tbl[21,1] = _b[_cons]*100


/*
import excel using "FMret_avg2.xlsx", ///
cellrange(A1:M408) firstrow clear

qui reg avg_bias, vce(robust)
matrix tbl[1,2] = _b[_cons]
matrix tbl[2,2] =_b[_cons]/_se[_cons]

qui reg lnsize , vce(robust)
matrix tbl[3,2] = _b[_cons]*100
matrix tbl[4,2] =_b[_cons]/_se[_cons]

qui reg lnbeme , vce(robust)
matrix tbl[5,2] = _b[_cons]*100
matrix tbl[6,2] =_b[_cons]/_se[_cons]

qui reg ret1 , vce(robust)
matrix tbl[7,2] = _b[_cons]*100
matrix tbl[8,2] =_b[_cons]/_se[_cons]

qui reg ret12_7, vce(robust)
matrix tbl[9,2] = _b[_cons]*100
matrix tbl[10,2] =_b[_cons]/_se[_cons]

qui reg IA, vce(robust)
matrix tbl[11,2] = _b[_cons]
matrix tbl[12,2] =_b[_cons]/_se[_cons]

qui reg  ivol , vce(robust)
matrix tbl[13,2] = _b[_cons]
matrix tbl[14,2] =_b[_cons]/_se[_cons]

qui reg retvol, vce(robust)
matrix tbl[15,2] = _b[_cons]
matrix tbl[16,2] =_b[_cons]/_se[_cons]

qui reg turnover, vce(robust)
matrix tbl[17,2] = _b[_cons]*100
matrix tbl[18,2] =_b[_cons]/_se[_cons]

qui reg intercept , vce(robust)
matrix tbl[19,2] = _b[_cons]*100
matrix tbl[20,2] =_b[_cons]/_se[_cons]

qui reg r_sqr, vce(robust)
matrix tbl[21,2] = _b[_cons]*100
*/






import excel using "FMret_score1.xlsx", ///
firstrow clear


qui reg bias_score, vce(robust)
matrix tbl[1,3] = _b[_cons]*100
matrix tbl[2,3] =_b[_cons]/_se[_cons]

qui reg intercept , vce(robust)
matrix tbl[19,3] = _b[_cons]*100
matrix tbl[20,3] =_b[_cons]/_se[_cons]

qui reg r_sqr, vce(robust)
matrix tbl[21,3] = _b[_cons]*100


/*
import excel using "FMret_score2.xlsx", ///
cellrange(A1:M408) firstrow clear

qui reg bias_score, vce(robust)
matrix tbl[1,4] = _b[_cons]*100
matrix tbl[2,4] =_b[_cons]/_se[_cons]

qui reg lnsize , vce(robust)
matrix tbl[3,4] = _b[_cons]*100
matrix tbl[4,4] =_b[_cons]/_se[_cons]

qui reg lnbeme , vce(robust)
matrix tbl[5,4] = _b[_cons]*100
matrix tbl[6,4] =_b[_cons]/_se[_cons]

qui reg ret1 , vce(robust)
matrix tbl[7,4] = _b[_cons]*100
matrix tbl[8,4] =_b[_cons]/_se[_cons]

qui reg ret12_7, vce(robust)
matrix tbl[9,4] = _b[_cons]*100
matrix tbl[10,4] =_b[_cons]/_se[_cons]

qui reg IA, vce(robust)
matrix tbl[11,4] = _b[_cons]
matrix tbl[12,4] =_b[_cons]/_se[_cons]

qui reg  ivol , vce(robust)
matrix tbl[13,4] = _b[_cons]
matrix tbl[14,4] =_b[_cons]/_se[_cons]

qui reg retvol, vce(robust)
matrix tbl[15,4] = _b[_cons]
matrix tbl[16,4] =_b[_cons]/_se[_cons]

qui reg turnover, vce(robust)
matrix tbl[17,4] = _b[_cons]*100
matrix tbl[18,4] =_b[_cons]/_se[_cons]

qui reg intercept , vce(robust)
matrix tbl[19,4] = _b[_cons]*100
matrix tbl[20,4] =_b[_cons]/_se[_cons]

qui reg r_sqr, vce(robust)
matrix tbl[21,4] = _b[_cons]*100
*/



frmttable using Table3, statmat(tbl)  sdec(3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3)






