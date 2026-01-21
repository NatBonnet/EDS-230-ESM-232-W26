#' Almond Yield Yearly Anomaly Model
#'
#' @param clim climate data in a .txt file format that can be read in as a table containing year, month, minimum temperature, and precipitation columns 
#' @param crop_name a text string of the crop being assessed to organize output summary table
#'
#' @returns a data frame containing the minimum, maximum, and mean yield anomalies across a time series
#' @export
#'
#' @examples
almond_yield <- function(clim, crop_name){
  
  clim <- read.table(here("Data", "clim.txt"), header = TRUE) 
  
  min_temps <- clim %>% 
    filter(month == 2) %>% 
    group_by(year) %>% 
    summarize(tmin = min(tmin_c))
  
  precip <- clim %>% 
    filter(month == 1) %>% 
    group_by(year) %>% 
    summarize(precip_sum = sum(precip))


  yield <- -0.015*min_temps$tmin -0.0046*(min_temps$tmin)**2 - 0.07*precip$precip_sum + 0.0043*(precip$precip_sum)**2 + 0.28

  min_yield <- min(yield)
  max_yield <- max(yield)
  mean_yield <- mean(yield)
  
  output <- data.frame(crop = crop_name, min_yield = min_yield, max_yield = max_yield, mean_yield = mean_yield)
    
  return(output)
}

library(here)
library(tidyverse)

# Test output renders properly
almond_yield(clim = clim, crop_name = "almond")
