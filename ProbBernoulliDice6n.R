print('file: ProbBernoulliDice6n.R')
print(date())
# rolling the dice
p = 1/6
n = 6
print(c(n,'n'))
k = seq(0,n)
B6 = dbinom(k,n,p)
print(c(round(B6,6),'B(k;n=6,p=1/6)'))
print(c(round(1/B6[7],1),'1/B(6;n=6,p=1/6)'))
# png('BernoulliDice6n200515.png',width=16,height=16,units='cm',res=300)
plot(k,B6,type='p',lwd=4,col='blue',xlab='k',ylab=NA,las=1,cex=0.6,cex.lab=1.5)
title(ylab='B(k;n=6,p=1/6)',line=2.5,cex.lab=1.5)
# dev.off()
# -----------------------------------------------------------------------------
# Results:
# "file: ProbBernoulliDice6n.R"
# "Sun Dec 18 13:27:35 2022"
# "6" "n"
# "0.334898"       "0.401878"       "0.200939"       "0.053584"      
# "0.008038"       "0.000643"       "2.1e-05"        "B(k;n=6,p=1/6)"
# "46656"            "1/B(6;n=6,p=1/6)"
# -----------------------------------------------------------------------------

