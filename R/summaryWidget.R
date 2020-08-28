#' @title summaryWidget
#'
#' @description Create an Rt visualisation using D3
#'
#' @param geoData sf object, map data
#' @param summaryData data.frame, summary data for mapping
#' @param rtData data.frame, rt estimates in the format {'Source':{'rtData':x, 'casesInfectionData':x, 'casesReportData':x, 'obsCasesData':x}, ...}
#' @param subregional_ref list, reference to subnational estimates in the format {'country_name':'url', ...}.
#' @param activeArea character, the default area to plot (defaults to United Kingdom)
#' @param activeData character, the default dataset to plot (defaults to 'R0')
#' @param activeTime character, the default time window (defaults to 'all')
#' @param runDate character, date of estimate run in the format ('YYYY-MM-DD')
#' @param width integer, width in pixels
#' @param elementId string, id of element
#'
#' @importFrom htmlwidgets createWidget
#'
#' @export

summaryWidget <- function(geoData = NULL,
                 summaryData = NULL,
                 rtData = NULL,
                 activeArea = 'United Kingdom',
                 activeData = 'R0',
                 activeTime = 'all',
                 runDate = NULL,
                 subregional_ref = NULL,
                 width = 1000,
                 elementId = NULL) {

  arg_types <- sapply(ls(), function(x){return(class(get(x)))})

  if(!is.null(geoData)){
    if (!'sf' %in% unlist(arg_types['geoData'])){stop('geoData must be an sf object')}
  }

  if(!is.null(summaryData)){
    if (!'data.frame' %in% unlist(arg_types['summaryData'])){stop('summaryData must be a data.frame object')}
  }

  if(!is.null(rtData)){
    if (!'list' %in% unlist(arg_types['rtData'])){stop('rtData must be a list object')}
  }

  #check rt data structure
  rt_expected_names <- c("rtData", "casesInfectionData", "casesReportData", "obsCasesData")

  if(!check_rtData_structure(rtData, rt_expected_names)){stop("Each level of rtData must include ", paste(rt_expected_names, collapse = ' '), ". Missing items should be NULL.")}

  #columns in this list must be present in the appropriate datasets
  expected_columns <- list(geoData = c('sovereignt', 'geometry'),
    rtData = c('country','date','type','median','lower_90','upper_90','lower_50','upper_50'),
    obsCasesData = c('region','date','confirm')
  )

  if (!is.null(geoData)){
    if (!check_geoData_columns(geoData, expected_columns[['geoData']])){stop("geoData missing required columns. geoData must contain: ", paste(expected_columns[['geoData']], collapse = ' '))}
  }

  if (!check_obsCasesData_columns(rtData, expected_columns[['obsCasesData']])){stop("obsCasesData missing required columns. obsCasesData must contain: ", paste(expected_columns[['obsCasesData']], collapse = ' '))}

  if (!check_rtData_columns(rtData, expected_columns[['rtData']])){stop("rtData missing required columns. rtData, casesInfectionData, casesReportData must contain: ", paste(expected_columns[['rtData']], collapse = ' '))}

  #warn for geoData name intersection issues
  if (!is.null(geoData)){

    check_geoData_names(geoData = geoData, rtData = rtData)

  }

  #define height, which is fixed based on dataset availability
  height <- define_height(geoData = geoData, summaryData = summaryData, rtData = rtData)

  # forward options using x
  x = list(
    activeArea = activeArea,
    activeData = activeData,
    activeTime = activeTime,
    runDate = runDate,
    geoData = geojsonNull(geoData),
    summaryData = jsonNull(summaryData),
    rtData = jsonNull(rtData),
    subregional_ref = subregional_ref
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'RtD3',
    x,
    width = width,
    height = height,
    package = 'RtD3',
    elementId = elementId
  )

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

  rtSample <- rtData[[1]][[which(unlist(sapply(rtData[[1]], function(x){return(!is.null(x))})[1:3]))[1]]]

  name_diff <- setdiff(rtSample$country, geoData$sovereignt)

  name_warning_geoData(name_diff)

}

name_warning_geoData <- function(name_diff){

  if (length(name_diff) > 0 & length(name_diff) <= 5){
    warning('The following names are present in the estimates but not in the GeoData: ', paste(name_diff, collapse = ', '), '.')
  } else if (length(name_diff) > 5) {
    warning('The following names are present in the estimates but not in the GeoData: ', paste(name_diff[1:5], collapse = ', '), ' ... and ', length(name_diff) - 5, ' more.')
  }

}

geojsonNull <- function(data){
  if (!is.null(data)){
    return(geojsonsf::sf_geojson(data))
  } else {
    return(data)
  }
}

jsonNull <- function(data){
  if (!is.null(data)){
    return(jsonlite::toJSON(data, null = "null"))
  } else {
    return(data)
  }
}

define_height <- function(geoData, summaryData, rtData){

  height = 0

  if(!is.null(geoData) & !is.null(summaryData)){
    height = height + 500
  }

  if(sum(sapply(rtData[[1]], function(x){return(!is.null(x))})[1:3]) < 3){
    height = height + (225 * sum(sapply(rtData[[1]], function(x){return(!is.null(x))})[1:3]))
  } else {
    height = height + (225 * 3)
  }

  height = height + 100

  return(height)

}

#' Shiny bindings for summaryWidget
#'
#' Output and render functions for using summaryWidget within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a RtD3
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name summaryWidget-shiny
#'
#' @export
summaryWidgetOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'RtD3', width, height, package = 'RtD3')
}

#' @rdname summaryWidget-shiny
#' @export
rendersummaryWidget <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, summaryWidgetOutput, env, quoted = TRUE)
}
