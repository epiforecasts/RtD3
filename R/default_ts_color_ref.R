#' @title default_ts_color_ref
#'
#' @description Create a default reference for timeseries colors. Also useful as a template when defining your own.

default_ts_color_ref <- function(){
  
  color_ref = list(
    list('value'=90, 'type'='estimate', 'color'='#d1ebe3'),
    list('value'=90, 'type'='estimate based on partial data', 'color'='#f7dfcc'),
    list('value'=90, 'type'='forecast', 'color'='#e3e2ef'),
    list('value'=50, 'type'='estimate', 'color'='#9bd4c2'),
    list('value'=50, 'type'='estimate based on partial data', 'color'='#edb98f'),
    list('value'=50, 'type'='forecast', 'color'='#c2c0dd'),
    list('value'=20, 'type'='estimate', 'color'='#75c4ab'),
    list('value'=20, 'type'='estimate based on partial data', 'color'='#e69e65'),
    list('value'=20, 'type'='forecast', 'color'='#aba8d0')
  )
  
  return(color_ref)
  
}
