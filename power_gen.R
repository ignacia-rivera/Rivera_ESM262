

power_gen = function(height, flow, rho= 1000, g= 9.8, K = 0.8) {
  
  #error checking
  if(height < 0) return (NA)
  
  #calculation
  result = rho* height* flow* g * K
  
  result(result)

  }