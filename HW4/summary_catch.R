
library(dplyr)

catch_summary <-  function(prices, catch){
  
  #Most frequent fish in each location
  
  most_frequent_index <- apply(Prices[,2:ncol(prices)],2,which.max)
  most_frequent <- Prices[most_frequent_index, 1]
  most_frequent <- data.frame(colnames(prices)[2:ncol(Table2)], most_frequent)
  colnames(most_frequent) <- c("site", "most frequent")
  
  #Total revenue for each location
  
  Revenue <- inner_join(prices, catch)
  
  return(list(most_frequent= most_frequent)) 
  
}

catch_summary(Table2)
