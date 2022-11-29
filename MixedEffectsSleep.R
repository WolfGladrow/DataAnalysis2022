print('file: MixedEffectsSleep.R')
print(date())
# created by: Dieter.Wolf-Gladrow@awi.de 11/2022 version 1.0
# ------------------------------------------------------------------------------library(lme4) # contains routine lmer() 
library(lme4) # contains routine lmer() & sleepstudy data
MEM = lmer(Reaction ~ Days + (Days | Subject), sleepstudy) # mixed effects model
MEMs = summary(MEM)
AICMEM = AIC(MEM); print(c(round(AICMEM,1),'AICMEM'))
# --------------
library(lattice)
sflag = 2
# ------ plot data:
if (sflag == 1) {
  # png('SleepDataPanel221110.png',width=16,height=16,units='cm',res=300)
  xyplot(Reaction ~ Days | Subject, data=sleepstudy,
         panel = function(x, y, ...) { 
           panel.xyplot(x, y, cex=0.5, ...) 
           panel.lmline(x, y, col = "red")
         })
  # dev.off()
}
# ------ plot density estimates:
if (sflag == 2) {
  qp = profile(MEM) # Profiled likelihood over various model parameters
  # png('SleepParameterDensities221128.png',width=16,height=16,units='cm',res=300)
  densityplot(qp)
  # dev.off()
}
# ------------------------------------------------------------------------------
# > summary(MEM)
# Linear mixed model fit by REML ['lmerMod']
# Formula: Reaction ~ Days + (Days | Subject)
# Data: sleepstudy
# 
# REML criterion at convergence: 1743.6
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.9536 -0.4634  0.0231  0.4634  5.1793 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev. Corr
# Subject  (Intercept) 612.10   24.741       
# Days                  35.07    5.922   0.07
# Residual             654.94   25.592       
# Number of obs: 180, groups:  Subject, 18
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)  251.405      6.825  36.838
# Days          10.467      1.546   6.771
# 
# Correlation of Fixed Effects:
#   (Intr)
# Days -0.138