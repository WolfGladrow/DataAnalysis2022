print('file: Overview22.R')
  xapp = c(0,100,100,0,0); yapp = c(0,0,8,8,0) # basic box, appendix
  xda = xapp/2+25; yda = yapp+90 # data analysis
  xid = xapp/2.8; yid = yapp+80 # independent data
  xts = xapp/1.8+44; yts = yapp+80 # time series
  xpe = xapp/2.4; ype = yapp+60 # parameter estimation
  xht = xapp/2.4+55; yht = yapp+60 # hypothesis testing
  xbt = xapp/2.5+30; ybt = yapp+50 # Bayes theorem
  xbe = xapp/4+24; ybe = yapp+40 # Bayesian estimation
  xfe = xapp/5; yfe = yapp+40 # Frequentistic estimation
  xbht = xapp/4+54; ybht = yapp+40 # Bayesian hypothesis testing
  xst = xapp/5+82; yst = yapp+40 # significance testing
  xpo = xapp/4+10; ypo = yapp+20 # point estimation
  xmo = xapp/3+45; ymo = yapp+20 # models
  # png('Overview220220.png',width=16,height=20,units='cm',res=300)
  plot(xapp,yapp,type='l',lwd=1,col='black',las=1,cex=0.4,
       col.axis='white',xaxt='n',yaxt='n',xlab='',ylab='',bty='n',
       xlim=c(0,100),ylim=c(0,100)) #,asp=1) # NOTE: asp = 1
  lines(xda,yda,col='black'); polygon(xda,yda,col='yellow')
  lines(xid,yid,col='black'); polygon(xid,yid,col='yellow')
  lines(xts,yts,col='black'); polygon(xts,yts,col='grey')
  lines(xpe,ype,col='black'); polygon(xpe,ype,col='yellow')
  lines(xht,yht,col='black'); polygon(xht,yht,col='yellow')
  lines(xbt,ybt,col='black'); polygon(xbt,ybt,col='green')
  lines(xbe,ybe,col='black'); polygon(xbe,ybe,col='yellow')
  lines(xfe,yfe,col='black'); polygon(xfe,yfe,col='yellow') 
  lines(xbht,ybht,col='black'); polygon(xbht,ybht,col='yellow')
  lines(xst,yst,col='black'); polygon(xst,yst,col='orange')
  lines(xpo,ypo,col='black'); polygon(xpo,ypo,col='yellow') 
  lines(xmo,ymo,col='black'); polygon(xmo,ymo,col='yellow') 
  polygon(xapp,yapp,col='yellow')
  text(50,4,'Appendix',col='black')
  text(50,94,'Data Analysis',col='black')
  text(0,84,'Independent data',col='black',pos=4)
  text(43,84,'Time series (autocorrelation)',col='black',pos=4)
  text(0,64,'Parameter estimation',col='black',pos=4)
  text(57,64,'Hypothesis testing',col='black',pos=4)
  text(50,54,'Bayes Theorem',col='black')
  text(0,44,'Freq. est.',col='black',pos=4)
  text(25,44,'Bayes. est.',col='black',pos=4)
  text(54,44,'Bayes. test.',col='black',pos=4)
  text(86,44,'NHST',col='black',pos=4)
  # text(80,44,'Sign. test.',col='black',pos=4)
  text(11,24,'Point est.',col='black',pos=4)
  text(52,24,'Models',col='black',pos=4)
  arrows(76,95,80,89,length=0.1,angle=10,col='grey',lwd=3)
  arrows(24,95,20,89,length=0.1,angle=10,col='blue',lwd=3)
  arrows(14,79,10,69,length=0.1,angle=10,col='blue',lwd=3)
  arrows(16,79,80,69,length=0.1,angle=10,col='blue',lwd=3) # id -> ht
  arrows(16,59,25,49,length=0.1,angle=10,col='blue',lwd=3) # pe -> be
  arrows(15,59,5,49,length=0.1,angle=10,col='blue',lwd=3) # pe -> fe
  arrows(29,54,26,49,length=0.1,angle=10,col='green',lwd=3) # bt -> be
  arrows(80,59,75,49,length=0.1,angle=10,col='blue',lwd=3) # ht -> bt
  arrows(71,54,74,49,length=0.1,angle=10,col='green',lwd=3) # bt -> bht
  arrows(81,59,90,49,length=0.1,angle=10,col='orange',lwd=3) # ht -> st
  arrows(35,39,25,29,length=0.1,angle=10,col='blue',lwd=3) # be -> po
  arrows(36,39,60,29,length=0.1,angle=10,col='blue',lwd=3) # be -> mo
  arrows(0,11,0,18,length=0.05,angle=30,col='blue',lwd=3) # app
  arrows(0,18,0,11,length=0.05,angle=30,col='blue',lwd=3) # app
  arrows(25,11,25,18,length=0.05,angle=30,col='blue',lwd=3) # app
  arrows(25,18,25,11,length=0.05,angle=30,col='blue',lwd=3) # app
  arrows(50,11,50,18,length=0.05,angle=30,col='blue',lwd=3) # app
  arrows(50,18,50,11,length=0.05,angle=30,col='blue',lwd=3) # app
  arrows(75,11,75,18,length=0.05,angle=30,col='blue',lwd=3) # app
  arrows(75,18,75,11,length=0.05,angle=30,col='blue',lwd=3) # app
  arrows(100,11,100,18,length=0.05,angle=30,col='blue',lwd=3) # app
  arrows(100,18,100,11,length=0.05,angle=30,col='blue',lwd=3) # app
  # dev.off()