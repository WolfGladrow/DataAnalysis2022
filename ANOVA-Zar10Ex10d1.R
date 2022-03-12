print('file: ANOVA-Zar10Ex10d1.R')
# put data into data frame (optional) & ANOVA
# If the data are not already available in the form of a data frame, one can use 
# write.table() to generate an appropriate file and set column and row names (this 
# part requires more coding than the ANOVA:
sflag = 1   # 1 = put data into data frame 
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
# --------------------------------------------------------------------------------
# ANOVA Zar (2010, Example 10.1)
mydata=read.table('Zar10Ex10d1data.txt',header=T)
g1 = mydata$weight[mydata$group=='group1'] # single group for boxplot
g2 = mydata$weight[mydata$group=='group2']
g3 = mydata$weight[mydata$group=='group3']
g4 = mydata$weight[mydata$group=='group4']
# png('Zar10Ex10d1Boxplot160726.png',width=16,height=12,units='cm',res=300)
boxplot(g1,g2,g3,g4,col='blue',las=1,ylab='Weight (kg)',cex.lab=1.5)
# dev.off()
# (3) ANOVA:
summary(aov(weight~group,data=mydata))
#              Df Sum Sq Mean Sq F value   Pr(>F)    
#  group        3  338.9  112.98   12.04 0.000283 ***
#  Residuals   15  140.8    9.38    
q = summary(aov(weight~group,data=mydata))
p = q[[1]][, 5][1]  # yes, this is obvious!
Fvalue = q[[1]][, 4][1]
print(c(round(p,6),' p'))
print(c(round(Fvalue,2),' Fvalue'))
# ----------------------------------------------------------------
# Remarks:
# In- and output of data/results in R can be confusing. 'Data frame' is a popular
#    format for data. 
# p = q[[1]][, 5][1]   extracting the p-value from the ANOVA-output of aov():
#                      I don't understand why so many brackets are necessary here!
#                      Unfortunately, the explanations from R (here: ?aov) are
#                      most often not helpful. However, you can a lot of information
#                      from R users on the Web.
# ----------------------------------------------------------------