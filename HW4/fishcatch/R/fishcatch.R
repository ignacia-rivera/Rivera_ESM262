#' Fish function
#'
#' @description
#'
#' @param prices a table that has prices for different fish
#' @param catch a table that has the number caught for each fish species for each location. Each location is in a different column, and each fish is in a different row.
#' @param plot a logical (default = FALSE) indicating if a graph of revenue by location and total revenue must be produced
#'
#' @return a list containing the following elements: 1) a tibble (frequent_fish) containing the most frequently caught fish in each location 2) a tibble (site_revenue) containing the total revenue for each location 3) an integer (total_catches) with the total fisheries sum, and 4) a plot (plot) of revenue by location
#' @export
#'
#' @author Lina Barbosa, Ignacia Rivera & Rucha Thakar
#'
#' @importFrom magrittr "%>%"

fishcatch <- function(prices = NULL, catch = NULL, plot = FALSE){

  if(is.null(prices)){stop("You did not supply a table for prices")}
  if(is.null(catch)){stop("You did not supply a table for catches")}

  ##### Most frequent fish per location #####
  #Get a tidy tibble
  tidy_catches <- catch %>%
    tidyr::gather(Site, Catch, -fish)

  #Obtain max by site and identify most frequent species
  max_by_site <- tidy_catches %>%
    dplyr::group_by(Site) %>%
    dplyr::summarize(Max = max(Catch, na.rm = T)) %>%
    dplyr::right_join(tidy_catches, by = "Site") %>%
    dplyr::filter(Catch == Max) %>%
    dplyr::select(-c(Max, Catch))

  #### Total revenue for each location ####
  revenue <- left_join(tidy_catches, prices, by = "fish") %>%
    mutate(revenue = Catch * price) %>%
    group_by(Site) %>%
    summarize(Revenue = sum(revenue, na.rm = T))

  ### Total fisheries sum ###
  total_catches <- sum(tidy_catches$Catch, na.rm = T)

  ### Plot
  plot_output <- NULL
  if(plot){
    plot_output <- ggplot2::ggplot(revenue, aes(x = Site, y = Revenue)) +
      ggplot2::geom_col(color = "black", alpha = 0.25) +
      ggplot2::geom_hline(yintercept = total_catches, linetype = "dashed") +
      ggplot2::theme_bw() +
      ggExtra::removeGrid() +
      ggplot2::scale_y_continuous(limits = c(0, 1.1*total_catches), expand = c(0,0)) +
      ggplot2::labs(title = "Total revenue by site",subtitle = "Dashed line represents total catch")
  }
  return(list(frequent_fish = max_by_site,
              site_revenue = revenue,
              total_catches = total_catches,
              plot = plot_output))
}
