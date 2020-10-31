#' @title legend_sequential
#'
#' @description Create a qualitative legend
#'
#' @param variable_name string, name of the variable of this legend. 
#' @param legend_scale string, type of legend scale, must be the name of a d3 scale. Default: "scaleLinear".
#' @param color_low string, color for the lowest value of the legend.
#' @param color_high string, color for the highest value of the legend.
#' @param color_no_data string, color for no data entries.

legend_sequential <- function(variable_name, 
                              legend_scale='scaleLinear', 
                              color_low='white', 
                              color_high='green', 
                              color_no_data='lightgrey'){
  
  legend <- list('variable_name' = variable_name,
                 'legend_type' = 'sequential',
                 'legend_scale' = legend_scale,
                 'legend_values' = list('low'=color_low,
                                        'high'=color_high,
                                        'No Data'=color_no_data))
  
  return(legend)
  
}