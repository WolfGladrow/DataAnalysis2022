print('file: NHST-pairedWilcoxon.R')
# paired Wilcoxon test: Wilcoxon (1945) original data (6/2018)')
A = c(209,200,177,169,159,169,187,198)
B = c(151,168,147,164,166,163,176,188)
d = A-B  # differences 58 32 30  5 -7  6 11 10
r = sign(d)*rank(abs(d)) # 8  7  6  1 -3  2  5  4
source('WilcoxonPaired.R')
WilcoxonPaired(d)
# -----------------------------------------------------------------------------
# Results:
# "file: NHST-pairedWilcoxon.R"
# "---------- function WilcoxonPaired ----------"
# "n = " "8"   
# "TminusObs = " "3"           
# "TplusObs = " "33"         
# "T_0.05(2)n = "       "3"                   " (critical T value)"
# "H0 rejected -> difference not equal 0"
# "Estimate p-value by Monte Carlo simulation:"
# "p-value = "     "0.0385"    
# "z1, z2 = "      "-2.1004"        "2.1004"         " (normal app.)"
# "p-value = "     "0.0357"    
# "H0 rejected (alpha = 0.05) -> difference not equal 0"
# -----------------------------------------------------------------------------
