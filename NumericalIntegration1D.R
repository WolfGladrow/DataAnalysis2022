print('file: NumericalIntegration1D.R')
print(date())
# purpose: 1D numerical integration with transfer of a parameter
print('1 = numerical integration 1D (9/2024)')
Integrand = function(x,a) {
    q = cos(a*x)
    return(q)
}
# Integrand = function(x,a) cos(a*x) # short form!!!
r = integrate(Integrand,lower=0,upper=1.2,a=2.1)$value
print(c(round(r,4),'r'))
# analytical solution: (1/a)*sin(a*x) at x=1.2 - at x=0
ra = (1/2.1)*sin(2.1*1.2)
print(c(round(ra,4),'ra'))
output = integrate(Integrand,lower=0,upper=1.2,a=2.1)
print(output)
rvalue = output$value
# -------------------------------------------------------------
# [1] "0.2773" "r"     
# [1] "0.2773" "ra"    
# 0.2773003 with absolute error < 7.5e-15
# -------------------------------------------------------------
# Remarks:
# (1) A simple function (for which an analytic solution is known)
#     was used on purpose in order to compare results from
#     numerical and analytical integration.
#
# (2) Call of integrate() yields not only the value of the
#     integral but also its uncertainty. This output can not
#     be used directly for further calculations. In order
#     obtain the value of the integral only, one can either
#     call integrate()$value or output$value
# -------------------------------------------------------------
