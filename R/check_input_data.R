#' @title check_input_data
#'
#' @description Check input data
#'
#' @param arg_types list, data types of arguments
#' @param geoData sf object, map data
#' @param rtData data.frame, rt estimates in the format {'Source':{'rtData':x, 'casesInfectionData':x, 'casesReportData':x, 'obsCasesData':x}, ...}
#'
#' @export

check_input_data <- function(arg_types, geoData = NULL, rtData = NULL){

  if(!is.null(geoData)){
    if (!'sf' %in% unlist(arg_types['geoData'])){stop('geoData must be an sf object')}
  }

  if(!is.null(rtData)){
    if (!'list' %in% unlist(arg_types['rtData'])){stop('rtData must be a list object')}
  }

  #check rt data structure
  rt_expected_names <- c("summaryData", "rtData", "casesInfectionData", "casesReportData", "obsCasesData")

  if(!check_rtData_structure(rtData, rt_expected_names)){stop("Each level of rtData must include ", paste(rt_expected_names, collapse = ' '), ". Missing items should be NULL.")}

  #columns in this list must be present in the appropriate datasets
  expected_columns <- list(geoData = c('sovereignt', 'geometry'),
                           rtData = c('region','date','type','median','lower_90','upper_90','lower_50','upper_50'),
                           obsCasesData = c('region','date','confirm')
  )

  if (!is.null(geoData)){
    if (!check_geoData_columns(geoData, expected_columns[['geoData']])){stop("geoData missing required columns. geoData must contain: ", paste(expected_columns[['geoData']], collapse = ' '))}
  }

  if (!check_obsCasesData_columns(rtData, expected_columns[['obsCasesData']])){stop("obsCasesData missing required columns. obsCasesData must contain: ", paste(expected_columns[['obsCasesData']], collapse = ' '))}

  if (!check_rtData_columns(rtData, expected_columns[['rtData']])){stop("rtData missing required columns. rtData, casesInfectionData, casesReportData must contain: ", paste(expected_columns[['rtData']], collapse = ' '))}

  return(TRUE)

}

check_rtData_structure <- function(rtData, expected_names){

  agreement <- c()

  for (source in names(rtData)){
    agreement <- append(agreement, identical(names(rtData[[source]]), expected_names))
  }

  return(sum(agreement) == length(agreement))

}

check_geoData_columns <- function(geoData, expected_columns){
  return(length(setdiff(expected_columns, colnames(geoData))) == 0)
}

check_obsCasesData_columns <- function(rtData, expected_columns){

  agreement <- c()
  for (source in names(rtData)){
    if (!is.null(rtData[[source]][['obsCasesData']])){
      agreement <- append(agreement, length(setdiff(expected_columns, colnames(rtData[[source]][['obsCasesData']]))) == 0)
    }
  }

  return(sum(agreement) == length(agreement))
}

check_rtData_columns <- function(rtData, expected_columns){

  agreement <- c()

  for (source in names(rtData)){

    if (!is.null(rtData[[source]][['rtData']])){
      agreement <- append(agreement, length(setdiff(expected_columns, colnames(rtData[[source]][['rtData']]))) == 0)
    }

    if (!is.null(rtData[[source]][['casesInfectionData']])){
      agreement <- append(agreement, length(setdiff(expected_columns, colnames(rtData[[source]][['casesInfectionData']]))) == 0)
    }

    if (!is.null(rtData[[source]][['casesReportData']])){
      agreement <- append(agreement, length(setdiff(expected_columns, colnames(rtData[[source]][['casesReportData']]))) == 0)
    }

  }

  return(sum(agreement) == length(agreement))
}

check_geoData_names <- function(geoData, rtData){

  rtSample <- rtData[[1]][[which(unlist(sapply(rtData[[1]], function(x){return(!is.null(x))})[2:4]))[1] + 1]]

  name_diff <- setdiff(rtSample$region, geoData$sovereignt)

  name_warning_geoData(name_diff)

}

name_warning_geoData <- function(name_diff){

  if (length(name_diff) > 0 & length(name_diff) <= 5){
    warning('The following names are present in the estimates but not in the GeoData: ', paste(name_diff, collapse = ', '), '.')
  } else if (length(name_diff) > 5) {
    warning('The following names are present in the estimates but not in the GeoData: ', paste(name_diff[1:5], collapse = ', '), ' ... and ', length(name_diff) - 5, ' more.')
  }

}
