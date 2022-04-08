LilliePapprox = function(n,D){
  # created by: Dieter.Wolf-Gladrow@awi.de 
  #    9/2016 version 1.0
  # approximate p-values for Lilliefors test 
  # This software is provided 'as is' without warranty of
  # any kind. But it's mine, so you can't sell it.
  # Molin & Abdi (1998), Abdi & Molin (2007)
  b0 = 0.37872256037043; b1 = 1.30748185078790; b2 = 0.08861783849346
  A = (-(b1+n)+sqrt((b1+n)^2-4*b2*(b0-1/D^2)))/(2*b2)
  Pr = (-0.37782822932809+1.67819837908004*A
        -3.02959249450445*A^2 +2.80015798142101*A^3
        -1.39874347510845*A^4 +0.40466213484419*A^5
        -0.06353440854207*A^6 +0.00287462087623*A^7
        +0.00069650013110*A^8 -0.00011872227037*A^9
        +0.00000575586834*A^10)
  return(Pr)}