print('file: ChiSquaredTestZar22d1.R')
print(date())
# chisq.test Zar (2010, Example 22.1, modified)
fobs = c(84,16)        # observed frequencies
fthe = c(75,25)        # theoretical/hypothesized frequencies
pthe = fthe/sum(fthe)  # c(0.75,0.25)  # theoretical/hypothesized probabilities
out=chisq.test(fobs,p=pthe)
p = round(out$p.value,3)
chisqobs = round(out$statistic,2)
# ----------------------------------------------
# 	Chi-squared test for given probabilities
# data:  fobs
# X-squared = 4.32, df = 1, p-value = 0.03767
# ----------------------------------------------
alpha = 0.05
nu = 1   # degrees of freedom
x = seq(0.01,10,0.01)
y = dchisq(x,df=nu)
# png('Zar10Ex22d1R190109.png',width=16,height=16,units='cm',res=300)
plot(x,y,type='l',col='black',lwd=3,xlim=c(0,max(x)),ylim=c(0,0.3),ylab='',las=0,cex.lab=1.5)
title(ylab=expression(paste(chi^2,'(x, df=1)')),line=2.3,cex.lab=1.5)
xp0 = c(0,max(x)); yp0 = c(0,0)  # zero-line
lines(xp0,yp0,col='grey',lwd=1)
chsq0051 = round(qchisq(1-alpha,df=nu),2)
xp1 = c(chsq0051,chsq0051); yp1 = c(0,dchisq(chsq0051,nu))
lines(xp1,yp1,col='magenta',lwd=3)
chisqTS = out$statistic
xp2 = c(chisqTS,chisqTS); yp2 = c(0,0.1)
lines(xp2,yp2,col='blue',lwd=3)
text(chisqTS*1.01,0.1,bquote(~chi[obs]^2 == .(chisqobs)),col='blue',pos=4,cex=1.5)
text(chisqTS*1.01,0.25,bquote(~p == .(p)),col='blue',pos=4,cex=1.5)
text(0.5,0.02,bquote(~chi[0.05]^2 == .(chsq0051)),col='magenta',pos=4,cex=1.5)
text(1.5,0.25,bquote(~alpha == .(alpha)),col='magenta',pos=4,cex=1.5)
# dev.off()
print(c(round(out$p.value,4),' out$p.value'))
print(c(round(chsq0051,2),' chsq0051'))
print(c(as.double(chisqTS),' chisqTS'))
# -----------------------------------------------------------------------------
# Results:
# "file: ChiSquaredTestZar22d1.R"
# "Sat Dec 17 10:23:10 2022"
# "0.0377"       " out$p.value"
# "3.84"      " chsq0051"
# "4.32"     " chisqTS"
# -----------------------------------------------------------------------------
# Remarks:
# p = 0.0377 is smaller than alpha = 0.05 
# The null hypothesis H0 ’data follow partitioning of 3:1’ is rejected on the 
# (chosen) level of significance alpha = 0.05 
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------