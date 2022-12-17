print('file: Fligner-KilleenZar10d1.R')
print(date())
# Equal variances? Fligner-Killeen test (non-parametric): Zar (2010) animal weights example 10.1
x1 = c(60.8,67.0,65.0,68.6,61.7)
x2 = c(68.7,67.7,75.0,73.3,71.8)
x3 = c(69.6,77.1,75.2,71.5)
x4 = c(61.9,64.2,63.1,66.7,60.3)
out = fligner.test(list(x1,x2,x3,x4))
p = out$p.value; print(c(round(p,4),'p'))
# -----------------------------------------------------------------------------
# Results:
# "file: Fligner-KilleenZar10d1.R"
# "Sat Dec 17 13:57:09 2022"
# "0.5714" "p"
# -----------------------------------------------------------------------------
# Remarks:
# p = 0.57 is larger than alpha = 0.05 -> Do not reject H0
# -----------------------------------------------------------------------------