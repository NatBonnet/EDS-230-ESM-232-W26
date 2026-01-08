# Writing a function to compute energy produced from a photovoltaic system given the average annual solar radiation

# energy produced = solar panel area (a)* panel yield (r) * annual avg solar radiation(given)(h) * performance ratio (accounting for losses) (pr)

#' Photovoltaic system output
#'
#' @param a 
#' @param h 
#'
#' @returns e = energy produced by photovoltaic system given average values for average energy loss (pr) and yield values (r)
#' @export
#'
#' @examples
#' energy <- e_prod_fun(a = 200, h = 5000)

e_prod_fun <- function(a, h){
  e = a * 0.2 * h * 0.75
  return(e)
}

energy <- e_prod_fun(a = 200, h = 5000)
