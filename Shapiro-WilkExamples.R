print('file: Shapiro-WilkExamples.R')
# Shapiro-Wilk test
y1 = c(-0.68644781, -0.82379154, -0.98416919, -2.02230891, -0.43507791, -0.76655674,  
       1.22178443,  0.09767100, -0.93391714, -1.23458941,  0.09188711,  0.56736177,
       -0.55276453, -0.07969400,  0.11767092,  2.07541230, 1.76443875,  0.60249792, 
       -1.29916116, -0.30322121, -0.77935252, -0.97190317,  0.84580262,  0.28698246,
       1.15160104,  0.35533328,  0.32936546,  1.68584964, 0.18260973,  1.93600509)
y2 = c(-0.43677762, -0.25606172, -0.05539436, -0.35955890, 2.52475885, -0.22159314,
       -0.17360687, -0.24747086, -0.25016363, -0.39612793, -0.22807506, -0.35896958,
       -0.23775192,  0.34039082,  1.31051033, -0.05988203, -0.15271093,  0.22672744,
       -0.15967543, -0.13572103, -0.44215444, -0.40168018,  0.44666595, -0.53978945,
       -0.45903502, -0.47862169, -0.22521386, -0.38695104, -0.44321571, -0.28632236)
y3 = c(0.09188711,  0.56736177, -0.55276453, -0.07969400,  0.11767092,  2.07541230,
       1.76443875,  0.60249792, -1.29916116, -0.30322121, -0.68644781, -0.82379154, 
       -0.98416919, -2.02230891, -0.43507791, -0.76655674, 1.22178443,  0.09767100, 
       -0.93391714, -1.23458941, -0.43677762, -0.25606172, -0.05539436, -0.35955890,
       2.52475885, -0.22159314, -0.17360687, -0.24747086, -0.25016363, -0.39612793)
print(' --------------------------------------------------- ')
print('Sample y1:')
print('Komogorov test for original data set (not standardized)')
KS1 = ks.test(y1,'pnorm',exact=TRUE)
print(c(round(KS1$p.value,4),' p_KS'))
print('Komogorov test for standardized data set')
y1s = (y1 - mean(y1))/sd(y1) # standardize
KS1s = ks.test(y1s,'pnorm',exact=TRUE)
print(c(round(KS1s$p.value,4),' p_KS_std'))
print(' --------------------------------------------------- ')
print('Lilliefors test for original data set')
# install.packages('fBasics')  # install package
library(fBasics)
LF1 = lillieTest(y1)
# remark: output object LF1 is class S4 -> need applying @ for access
# (Why make it simple when it can be more complicated?)
print(c(round(LF1@test$p.value,4),' p_Lilliefors'))
LF1s = lillieTest(y1s)
print(c(round(LF1s@test$p.value,4),' p_Lilliefors_std'))
print(' --------------------------------------------------- ')
print('Shapiro-Wilk test for original data set')
SW1 = shapiro.test(y1)
print(c(round(as.numeric(SW1[2]),4),' p_ShapiroWilk'))
SW1s = shapiro.test(y1s)
print(c(round(as.numeric(SW1s[2]),4),' p_ShapiroWilk_std'))
print(' --------------------------------------------------- ')
print('Sample y1 shifted: y1 <- y1 + 1.3')
y1 = y1+1.3
KS1 = ks.test(y1,'pnorm',exact=TRUE)
print(c(KS1$p.value,' p_KS'))
y1s = (y1 - mean(y1))/sd(y1) # standardize
KS1s = ks.test(y1s,'pnorm',exact=TRUE)
print(c(round(KS1s$p.value,4),' p_KS_std'))
KS1sb = ks.test(y1,'pnorm',mean(y1),sd(y1),exact=TRUE)
print(c(round(KS1sb$p.value,4),' p_KS_std_b'))
print(' --------------------------------------------------- ')
LF1 = lillieTest(y1)
print(c(round(LF1@test$p.value,4),' p_Lilliefors'))
LF1s = lillieTest(y1s)
print(c(round(LF1s@test$p.value,4),' p_Lilliefors_std'))
print(' --------------------------------------------------- ')
SW1 = shapiro.test(y1)
print(c(round(as.numeric(SW1[2]),4),' p_ShapiroWilk'))
SW1s = shapiro.test(y1s)
print(c(round(as.numeric(SW1s[2]),4),' p_ShapiroWilk_std'))
print(' --------------------------------------------------- ')
print('Sample y2:')
KS2sb = ks.test(y2,'pnorm',mean(y2),sd(y2),exact=TRUE)
print(c(round(KS2sb$p.value,4),' p_KS_std_b'))
LF2 = lillieTest(y2)
print(c(LF2@test$p.value,' p_Lilliefors'))
SW2 = shapiro.test(y2)
print(c(as.numeric(SW2[2]),' p_ShapiroWilk'))
print(' --------------------------------------------------- ')
print('Sample y3:')
KS3sb = ks.test(y3,'pnorm',mean(y3),sd(y3),exact=TRUE)
print(c(round(KS3sb$p.value,4),' p_KS_std_b'))
LF3 = lillieTest(y3)
print(c(round(LF3@test$p.value,4),' p_Lilliefors'))
SW3 = shapiro.test(y3)
print(c(round(as.numeric(SW3[2]),4),' p_ShapiroWilk'))
print(' ---------------------------------------------------')