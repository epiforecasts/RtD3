#' @title legend_qualitative
#'
#' @description Create a qualitative legend
#'
#' @param variable_name string, name of the variable of this legend. 
#' @param legend_values list, reference for legend colors in the format: {'value':'color', ...}.

legend_qualitative <- function(variable_name, legend_values){
  
  if (!'list' %in% class(legend_values)){
    stop("Legend values should be a list of colors in the format: {'value':'color', ...}.")
  }
  
  legend <- list('variable_name' = variable_name,
                 'legend_type' = 'qualitative',
                 'legend_values' = legend_values)
  return(legend)
  
}