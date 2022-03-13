print('file: ANOVA-Bayesian.R')
# Bayesian ANOVA: Zar(2010) example 10.1
# (1) put data into data frame (optional)
# (2) Bayesian ANOVA
# (3) compare with frequentistic NHST-ANOVA (optional)
# ---------------------------------------------------------------------------------
# If the data are not already available in the form of a data frame, one can use 
# write.table() to generate an appropriate file and set column and row names (this 
# part requires more coding than the ANOVA:
sflag = 0   # 1 = put data into data frame; 0 = load data from file
if (sflag == 1) {
  # data: animal body weights (kg) Zar (2010, p.191)
  X1 = c(60.8,67.0,65.0,68.6,61.7)  # (kg) weight of animals
  X2 = c(68.7,67.7,75.0,73.3,71.8)
  X3 = c(69.6,77.1,75.2,71.5)
  X4 = c(61.9,64.2,63.1,66.7,60.3)
  library(plyr)
  out = raply(.n=length(X1),.exp='group1')
  write.table(X1,file='DummyFile.txt',row.names=out,col.names=NA)
  out = raply(.n=length(X2),.exp='group2')
  write.table(X2,file='DummyFile.txt',append=T,row.names=out,col.names=F)
  out = raply(.n=length(X3),.exp='group3')
  write.table(X3,file='DummyFile.txt',append=T,row.names=out,col.names=F)
  out = raply(.n=length(X4),.exp='group4')
  write.table(X4,file='DummyFile.txt',append=T,row.names=out,col.names=F)
  z = read.table(file='DummyFile.txt',header=T)
  write.table(z,file='Zar10Ex10d1data.txt',row.names=F,col.names=c('group','weight'))
}
# ---------------------------------------------------------------------------------
mydata=read.table('Zar10Ex10d1data.txt',header=TRUE)
# Bayesian ANOVA:
# install.packages('BayesFactor')
library(BayesFactor)
out = generalTestBF(weight ~ group,data=mydata)
B10 = extractBF(out,onlybf = TRUE); print(c(round(B10,4),'B10'))
# Jeffreys' scales of evidence (slightly modified):
BF = B10
if(BF > 10)                    print('strong evidence against H0/strong evidence for H1')
if((BF >= 3.16) && (BF <= 10)) print('substantial evidence against H0/substantial evidence for H1')
if((BF > 1) && (BF < 3.16))    print('slight evidence against H0/slight evidence for H1')
if((BF >= 0.316) && (BF <= 1)) print('slight evidence against H1/slight evidence for H0')
if((BF > 0.1) && (BF < 0.316)) print('substantial evidence against H1/substantial evidence for H0')
if(BF <= 0.1)                  print('strong evidence against H1/strong evidence for H0')
# ---------------------------------------------------------------------------------
sflag = 3
if (sflag == 3) {
# NHST-ANOVA (significance test):
q = summary(aov(weight ~ group,data=mydata))
p = q[[1]][, 5][1]  # yes, this is obvious!
Fvalue = q[[1]][, 4][1]
print(c(p,' p'))
print(c(Fvalue,' Fvalue'))
}