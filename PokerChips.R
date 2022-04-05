print('file: PokerChips.R')
# Poker chips (Lewis, 2017, p.142-143)')
pbag1 = 0.5; pbag2 = 0.5
p1red = 0.25; p2red = 0.75 
# p(bag 2 | data) = p(data | bag 2) * p(bag 2)/
#      (p(data | bag 1) * p(bag 1) + p(data | bag 2) * p(bag 2))
print('(1) red in first draw -> probability & odds for bag 2')
# p(bag 2 | r) = p(r | bag 2) * p(bag 2)/
#      (p(r | bag 1) * p(bag 1) + p(r | bag 2) * p(bag 2))
pbag2r = dbinom(1,1,p2red)*pbag2/(dbinom(1,1,p1red)*pbag1+dbinom(1,1,p2red)*pbag2)
obag2r = pbag2r/(1-pbag2r)
print('(2) 2 red -> probability & odds for bag 2')
# p(bag 2 | rr) = p(rr | bag 2) * p(bag 2)/
#      (p(rr | bag 1) * p(bag 1) + p(rr | bag 2) * p(bag 2))
pbag2rr = dbinom(2,2,p2red)*pbag2/(dbinom(2,2,p1red)*pbag1+dbinom(2,2,p2red)*pbag2)
obag2rr = pbag2rr/(1-pbag2rr)
print('(3) red in draws 1,2,3 -> probability & odds for bag 2')
# p(bag 2 | rrr) = p(rrr | bag 2) * p(bag 2)/
#      (p(rrr | bag 1) * p(bag 1) + p(rrr | bag 2) * p(bag 2))
pbag2rrr = dbinom(3,3,p2red)*pbag2/(dbinom(3,3,p1red)*pbag1+dbinom(3,3,p2red)*pbag2)
obag2rrr = pbag2rrr/(1-pbag2rrr)
print('(4) 2 red, 1 white -> probability & odds for bag 2')
# p(bag 2 | rrw) = p(rrw | bag 2) * p(bag 2)/
#      (p(rrw | bag 1) * p(bag 1) + p(rrw | bag 2) * p(bag 2))
pbag2rrw = dbinom(2,3,p2red)*pbag2/(dbinom(2,3,p1red)*pbag1+dbinom(2,3,p2red)*pbag2)
obag2rrw = pbag2rrw/(1-pbag2rrw)
# ---------------------------------------------------
# Results: 
print(c('pbag1 = ',pbag1))
print(c('pbag2 = ',pbag2))
print(c('p1red = ',p1red))
print(c('p2red = ',p2red))
print('(1) red in first draw -> probability & odds for bag 2')
print(c('pbag2r = ',round(pbag2r,4)))
print(c('obag2r = ',round(obag2r,4)))
print(c('dbinom(1,1,p1red) = ',round(dbinom(1,1,p1red),4)))
print(c('dbinom(1,1,p2red) = ',round(dbinom(1,1,p2red),4)))
print('(2) 2 red -> probability & odds for bag 2')
print(c('pbag2rr = ',round(pbag2rr,4)))
print(c('obag2rr = ',round(obag2rr,4)))
print(c('dbinom(2,2,p1red) = ',round(dbinom(2,2,p1red),4)))
print(c('dbinom(2,2,p2red) = ',round(dbinom(2,2,p2red),4)))
print('(3) red in draws 1,2,3 -> probability & odds for bag 2')
print(c('pbag2rrr = ',round(pbag2rrr,4)))
print(c('obag2rrr = ',round(obag2rrr,4)))
print(c('dbinom(3,3,p1red) = ',round(dbinom(3,3,p1red),4)))
print(c('dbinom(3,3,p2red) = ',round(dbinom(3,3,p2red),4)))
print('(4) 2 red, 1 white -> probability & odds for bag 2')
print(c('pbag2rrw = ',round(pbag2rrw,4)))
print(c('obag2rrw = ',round(obag2rrw,4)))
print(c('dbinom(2,3,p1red) = ',round(dbinom(2,3,p1red),4)))
print(c('dbinom(2,3,p2red) = ',round(dbinom(2,3,p2red),4)))
# 'Poker chips (Lewis, 2017, p.142-143)'
# 'pbag1 = ' '0.5'     
# 'pbag2 = ' '0.5'     
# 'p1red = ' '0.25'    
# 'p2red = ' '0.75'    
# '(1) red in first draw -> probability & odds for bag 2'
# 'pbag2r = ' '0.75'     
# 'obag2r = ' '3'        
# 'dbinom(1,1,p1red) = ' '0.25'                
# 'dbinom(1,1,p2red) = ' '0.75'                
# '(2) 2 red -> probability & odds for bag 2'
# 'pbag2rr = ' '0.9'       
# 'obag2rr = ' '9'    
# 'dbinom(2,2,p1red) = ' '0.0625'              
# 'dbinom(2,2,p2red) = ' '0.5625'
# '(3) red in draws 1,2,3 -> probability & odds for bag 2'
# 'pbag2rrr = ' '0.9643'     
# 'obag2rrr = ' '27'         
# 'dbinom(3,3,p1red) = ' '0.0156'              
# 'dbinom(3,3,p2red) = ' '0.4219'              
# '(4) 2 red, 1 white -> probability & odds for bag 2'
# 'pbag2rrw = ' '0.75'       
# 'obag2rrw = ' '3'          
# 'dbinom(2,3,p1red) = ' '0.1406'              
# 'dbinom(2,3,p2red) = ' '0.4219' 
