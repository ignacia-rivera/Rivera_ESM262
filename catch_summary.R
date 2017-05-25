
catch_summary <-  function(prices, catch, graph= 'Yes'){
  
  # Most frequent fish in each location
  
  most_frequent_index <- apply(catch[,2:ncol(catch)],2,which.max)
  most_frequent <- catch[most_frequent_index, 1]
  
  # Total revenue for each location
  
  revenue <- dplyr::inner_join(prices, catch, by = "fish")
  revenue <-  revenue[,3:ncol(revenue)]* revenue[,2] 
  revenue <- colSums(revenue, na.rm = FALSE)
  
  site_summary <- data.frame(site= colnames(catch[-1]),
                             most_frequent= most_frequent,
                             revenue)
  
  # Add total 
  
  total_revenue <- sum(site_summary$revenue, na.rm = T)
  
  # If the graph is required
  
  if (graph == 'Yes') {
    
    plot <- barplot(site_summary$revenue)
    #we  need to add total revenue 
    return(list(site_summary, total_revenue, plot))
    
  } 
  
  else {
    
    return(list(site_summary, total_revenue))
  }
  
}

catch_summary(prices, catch, graph= 'Yes')

 