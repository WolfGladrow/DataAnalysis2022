print('file: VarRatioTestCountData.R')
# apply variance ratio test to count data fromZar (2010, Example 8.7)
x1 = c(41,35,33,36,40,46,31,37,34,30,38) # number of moths in trap #1
x2 = c(52,57,62,55,64,57,56,55,60,59)    # number of moths in trap #2
out3 = var.test(x1,x2); p = out3$p.value; print(c(round(p,4),'p'))
out3a = var.test(x2,x1); p2 = out3a$p.value; print(c(round(p2,4),'p2'))
alpha = 0.05
if(p < alpha) print(c('p < alpha = ',alpha,': reject H0'))
if(p >= alpha) print(c('p >= alpha = ',alpha,': do not reject H0'))
# -----------------------------------------------------------------------------
# Results:
# "file: VarRatioTestCountData.R"
# "0.4401" "p"     
# "0.4401" "p2"    
# "p >= alpha = " "0.05"  ": do not reject H0"
# -----------------------------------------------------------------------------
