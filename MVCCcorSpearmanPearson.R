print('file: MVCCcorSpearmanPearson.R')
# correlation coefficients: Spearman (montonic) versus Pearson (linear)
set.seed(1953)
Vmax = 5
K = 0.04
x = seq(0,30,0.01); L = length(x)
y = Vmax*x/(x+K) + 0.01*rnorm(L)
cp=cor(x,y,method='pearson'); print(c(round(cp,2),'Pearson correlation'))
cs=cor(x,y,method='spearman'); print(c(round(cs,2),'Spearman correlation'))
ck=cor(x,y,method='kendall'); print(c(round(ck,2),'Kendall correlation'))
# png('SpearmanCorrelation191029.png',width=16,height=12,units='cm',res=300)
plot(x,y,type='p',lwd=3,col='blue',xlab='x',ylab='y',las=1,cex=0.4)
library(latex2exp)
text(10,2,TeX('$r_{Pearson} = 0.29$'),col='blue',pos=4)
text(10,3,TeX('$\\tau_{Kendall} = 0.55$'),col='blue',pos=4)
text(10,4,TeX('$\\rho_{Spearman} = 0.72$'),col='blue',pos=4)
# dev.off()