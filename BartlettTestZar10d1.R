print('file: BartlettTestZar10d1.R')
# Bartlett test: Zar (2010) animal weights example 10.1
# H0: variances in each of the groups are the same
x1 = c(60.8,67.0,65.0,68.6,61.7)
x2 = c(68.7,67.7,75.0,73.3,71.8)
x3 = c(69.6,77.1,75.2,71.5)
x4 = c(61.9,64.2,63.1,66.7,60.3)
out = bartlett.test(list(x1,x2,x3,x4))
p = out$p.value; print(c(round(p,4),'p')) 
# ----------------------------------------------------------------
# Result:
# "0.9243" "p" 
#
# Remarks:
# Note the input to bartlett.test() in form of a list
# p > 0.05 -> H0 not rejected on the level of significance alpha = 0.05
# ----------------------------------------------------------------