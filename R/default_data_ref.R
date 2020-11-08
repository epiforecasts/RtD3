#' @title default_data_ref
#'
#' @description Create a default reference for a input datasets. Also useful as a template when defining your own.

default_data_ref <- function(){

  data_ref = list(
      'summaryData'=list('geometry_name'='region'),
      'rtData'=list('geometry_name'='region'),
      'casesInfectionData'=list('geometry_name'='region'),
      'casesReportData'=list('geometry_name'='region'),
      'obsCasesData'=list('geometry_name'='region'),
      'geoData'=list('geometry_name'='sovereignt')
    )

  return(data_ref)

}


