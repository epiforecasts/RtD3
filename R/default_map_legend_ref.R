#' @title default_map_legend_ref
#'
#' @description Create a default reference for a map legend. Also useful as a template when defining your own.

default_map_legend_ref <- function(){

  map_legend_ref = list(
    list('variable_name'='Expected change in daily cases',
      'legend_type'='qualitative',
      'legend_values'=list('Unsure'='#7b848f',
                           'Increasing'='#e75f00',
                           'Likely increasing'='#fd9e49',
                           'Decreasing'='#1170aa',
                           'Likely decreasing'='#5fa2ce',
                           'No Data'='lightgray')),
    list('variable_name'='Effective reproduction no.',
      'legend_type'='sequential',
      'legend_scale'='scaleLinear',
      'legend_values'=list('low'='white',
                           'high'='green',
                           'No Data'='lightgray')),
    list('variable_name'='New confirmed cases by infection date',
         'legend_type'='sequential',
         'legend_scale'='scaleLinear',
         'legend_values'=list('low'='white',
                              'high'='red',
                              'No Data'='lightgray')),
    list('variable_name'='Rate of growth',
         'legend_type'='sequential',
         'legend_scale'='scaleLinear',
         'legend_values'=list('low'='white',
                              'high'='blue',
                              'No Data'='lightgray')),
    list('variable_name'='Doubling/halving time (days)',
         'legend_type'='sequential',
         'legend_scale'='scaleLinear',
         'legend_values'=list('low'='white',
                              'high'='purple',
                              'No Data'='lightgray'))
  )

  return(map_legend_ref)

}
