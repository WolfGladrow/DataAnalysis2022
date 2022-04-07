print('file: NHST-Mendel-Fisher.R')
# Example from Fisher (1925, Table 12) Mendelian class frequencies
fobs = c(328,122,77,33)       # observed frequencies m+x
total = sum(fobs)
H0 = c(9,3,3,1)
fexp = round(total*H0/sum(H0)) # expected frequencies m
x = fobs-fexp
q = round(x^2/fexp,3)
totalq = sum(q)         # test statistic chi-squared
nu = length(fexp) - 1; print(c(nu,' degrees of freedom, nu'))   
chisqc = qchisq(p=(1-0.05),df=nu); 
print(c(round(chisqc,2),' critical chi-square value for alpha=0.05'))
p = (1-pchisq(q=totalq,df=nu)); print(c(round(p,4),' p-value'))
xp = seq(1,4)
# png('MendelianExercise190415a.png',width=16,height=16,units='cm',res=300)
plot(xp,fobs,type='p',lwd=4,col='blue',xlab='Sample',ylab='# of individuals',
     las=1,cex=0.6,ylim=c(0,350),cex.lab=1.5)
points(xp,fexp,col='magenta',lwd=4,pch=24,cex=0.6)
# dev.off()