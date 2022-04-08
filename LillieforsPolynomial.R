print('LillieforsPolynomial.R')
# Lilliefors distribution: Molin & Abdi (1998), Abdi & Molin (2007)
n = 30; D = 0.0925906
b0 = 0.37872256037043; b1 = 1.30748185078790; b2 = 0.08861783849346
A = (-(b1+n)+sqrt((b1+n)^2-4*b2*(b0-1/D^2)))/(2*b2)
Pr = (-0.37782822932809+1.67819837908004*A
        -3.02959249450445*A^2 +2.80015798142101*A^3
        -1.39874347510845*A^4 +0.40466213484419*A^5
        -0.06353440854207*A^6 +0.00287462087623*A^7
        +0.00069650013110*A^8 -0.00011872227037*A^9
        +0.00000575586834*A^10)
# plot distribution for n = 30
Darr = seq(0.07,0.23,0.001); L = length(Darr)
Prarr = numeric(L)
for(k in 1:L) {D = Darr[k];
A = (-(b1+n)+sqrt((b1+n)^2-4*b2*(b0-1/D^2)))/(2*b2);
Prarr[k] = (-0.37782822932809+1.67819837908004*A
              -3.02959249450445*A^2 +2.80015798142101*A^3
              -1.39874347510845*A^4 +0.40466213484419*A^5
              -0.06353440854207*A^6 +0.00287462087623*A^7
              +0.00069650013110*A^8 -0.00011872227037*A^9
              +0.00000575586834*A^10)}
# png('LillieforsP160916.png',width=16,height=16,units='cm',res=300)
plot(Darr,Prarr,type='l',lwd=4,col='blue',ylab='f(D;n=30)',
       xlab='D',las=1,cex=0.4,ylim=c(0,1),cex.lab=1.5)
xp = c(min(Darr),max(Darr)); yp = c(0,0)
lines(xp,yp,col='magenta')
lines(xp,yp+1,col='magenta')
# dev.off()