#' @title default_map_legend_ref
#'
#' @description Create a default reference for a map legend. Also useful as a template when defining your own.

default_map_legend_ref <- function(){
  
  map_legend_ref = list(
    list('variable_name'='Expected change in daily cases',
      'legend_type'='qualitative',
      'legend_values'=list('Unsure'='red',
                           'Increasing'='blue',
                           'Likely increasing'='purple',
                           'Decreasing'='pink',
                           'Likely decreasing'='green',
                           'No Data'='grey')),
    list('variable_name'='Effective reproduction no.',
      'legend_type'='sequential',
      'legend_scale'='scaleLinear',
      'legend_values'=list('low'='white',
                           'high'='green',
                           'No Data'='grey')
    )
  )
  
  return(map_legend_ref)
  
}
