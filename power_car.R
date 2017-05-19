

m= 31752
V = c(seq(10, 1000, 5))
c_drag <- runif(10, 0.5, 1)


power_car = function( V, m, c_drag=2, A= 9, g = 9.8, p_air = 1.2, c_roll = 0.015) {
  
  Pb = c_roll * m * V + 0.5* A * p_air * c_drag * V^3
  mean = mean(Pb)
  maxPb = max(Pb)
  
  #plot = plot(V, Pb)
    
  return(list(Pb= Pb, mean = mean, max= maxPb))
  
}

power_car(V,m)

res = apply(as.matrix(c_drag), 1, power_car,  V  = V, m = m)
