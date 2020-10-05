#' @title default_ts_colors
#'
#' @description Define default colors for time series plots
#'
#' Can be overridden with a list of the same format
#'
#' @export

default_ts_colors <- function(){
  
  default_colors <- list('poly_90_e' = '#d1ebe3',
                         'poly_90_eb' = '#f7dfcc',
                         'poly_90_f' = '#e3e2ef',
                         'poly_50_e' = '#9bd4c2',
                         'poly_50_eb' = '#edb98f',
                         'poly_50_f' = '#c2c0dd',
                         'poly_20_e' = '#75c4ab',
                         'poly_20_eb' = '#e69e65',
                         'poly_20_f' = '#aba8d0')
  
  return(default_colors)
  
}
