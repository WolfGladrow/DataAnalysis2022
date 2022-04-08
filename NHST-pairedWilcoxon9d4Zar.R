print('file: NHST-pairedWilcoxon9d4Zar.R')
# Zar (2010) Example 9.4 p.185 Wilcoxon paired-sample test
#     applied to the data of Example 9.1.; simplified version')
# d = X1-X2       # (cm) difference
# 1 <- Zar (2010) Example 9.1 p.180 2-tailed paired-sample t test
x = c(142,140,144,144,142,146,149,150,142,148) # (cm) hind leg length
y = c(138,136,147,139,143,141,143,145,136,146) # (cm)  foreleg length
d = x-y
source('WilcoxonPaired.R')
WilcoxonPaired(d)
# --- Look at your data! -------------------------------------------
sflag = 1
if (sflag == 1) {
# png('Zar10Ex9d4a180612.png',width=16,height=16,units='cm',res=300)
xymin = min(c(x,y)); xymax = max(c(x,y)) 
plot(x,type='p',lwd=4,col='blue',xlab='Data #',ylab='Length (cm)',
     las=1,cex=0.6,ylim=c(xymin,xymax),cex.lab=1.5)
points(y,col='magenta',lwd=4,cex=0.6,pch=24)
# dev.off()
}
if (sflag == 2) {
  # png('Zar10Ex9d4d180612.png',width=16,height=16,units='cm',res=300)
  plot(d,type='p',lwd=4,col='black',xlab='Data #',ylab='Difference (cm)',las=1,
       cex=0.6,cex.lab=1.5)
  # dev.off()
}
# ---------- function WilcoxonPaired ----------'
# 'n = ' '10'  
# 'TminusObs = ' '4'           
# 'TplusObs = ' '51'         
# 'T_0.05(2)n = '       '8'                   ' (critical T value)'
# 'H0 rejected -> difference not equal 0'
# 'Estimate p-value by Monte Carlo simulation:'
# 'p-value = ' '0.0141'    
# 'z1, z2 = '      '-2.3953'        '2.3953'         ' (normal app.)'
# 'p-value = ' '0.0166'    
# 'H0 rejected -> difference not equal 0'