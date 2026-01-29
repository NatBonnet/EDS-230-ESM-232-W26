#' Almond Profit
#'
#' @param clim: Climate data frame
#' @param year: Year of harvest
#' @param price: price of almonds in USD per ton
#' @param base_yield: average yearly yield of almonds (tons/acre)
#' @param cost: average yearly cost of almond production in USD per ton
#' 
#'
#' @returns Expected net profit (USD/acre) from almond yield in a given year
#' @export
#'
#' @examples

# Implement profit function with base cost and price
profit <- function(clim, year, price = 4000, cost = 3000){
  
  # Implement almond yield anomaly function 
  almond_yield <- function(clim, harvest_year){
    
    # Find the minimum temperature in Feb from a given year
    min_temps <- clim %>% 
      # Filter for February and the desired year
      filter(month == 2, year == harvest_year) %>%  
      # Find lowest Feb temp
      summarize(tmin = mean(tmin_c, na.rm = TRUE))
    
    # Find the total precipitation from Jan in a given year
    precip <- clim %>% 
      # Filter for January and desired year
      filter(month == 1, year == harvest_year) %>% 
      # Get total rainfall value (mm)
      summarize(precip_sum = sum(precip, na.rm = TRUE))
    
    # Apply equation from Lobell et. al 2006
    yield <- -0.015*min_temps$tmin -0.0046*(min_temps$tmin)**2 - 
      0.07*precip$precip_sum + 0.0043*(precip$precip_sum)**2 + 0.28
    # Return yield
    return(yield)
  }
  
  # Apply yield function to a given year
  yield <- almond_yield(clim = clim, harvest_year = year)
  
  # Return a value to estimate inflation costs
  inflation <- 1 + ((year - 1988) * 0.02)
  
  # Calculate raw profit from yield with a given base price factoring in inflation
  revenue <- yield * price * inflation
  
  # Calculate expected costs with inflation
  total_cost <- yield * cost * inflation
  
  # Calculate profit estimate for a given year
  profit <- revenue - total_cost
  
  # Return profit value
  return(profit)
}


#-------------------Implementation-----------------------------------

# Read in libraries for informal sensitivity analysis
library(tidyverse)
library(here)

# Read in data frame of interest
clim <- read.table(here("Assignments", "clim.txt"), header = TRUE)

# Test profit for one year
profit(clim = clim, year = 2000)

# Pull out entire period of interest
years <- seq(min(clim$year), max(clim$year), by = 1)

# Generate possible values for price variation by year
price_var <- rnorm(10, mean = 4000, sd = 50)

# Generate possible values for cost by year
cost_var <- rnorm(10, mean = 3000, sd = 50)

# Apply profit function across all years of interest in climate data
profit_profile <- expand_grid(year = years, price = price_var, cost = cost_var) %>% 
  mutate(profit = pmap_dbl(list(year = year, price = price, cost = cost), 
                           ~profit(clim = clim, ...)))

# Produce plot of sensitivity analysis
ggplot(data = profit_profile, aes(x = factor(year), y = profit)) +
  geom_boxplot(color = "darkblue", outliers = FALSE)+
  scale_y_continuous(labels = scales::label_dollar())+
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 70))+
  labs(title = "California Almond Profit Over Time", subtitle = "adjusted for base yield variance",
       y = "Profit Value (USD/acre)")

