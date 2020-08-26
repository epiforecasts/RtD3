#' @title RtD3
#'
#' @description Create an Rt visualisation using D3
#'
#' @param geoData sf object, map data
#' @param summaryData data.frame, summary data for mapping
#' @param rtData data.frame, rt estimates
#' @param casesInfectionData data.frame, cases by date of infection estimates
#' @param casesReportData data.frame, cases by date of report estimates
#' @param obsCasesData data.frame, observed cases data
#' @param activeArea character, the default area to plot (defaults to United Kingdom)
#' @param activeData character, the default dataset to plot (defaults to 'R0')
#' @param activeTime character, the default time window (defaults to 'all')
#' @param runDate character, date of estimate run in the format ('YYYY-MM-DD')
#' @param width integer, width in pixels
#' @param height integer, height in pixels
#' @param elementId string, id of element
#'
#' @importFrom htmlwidgets createWidget
#'
#' @export

RtD3 <- function(geoData = NULL,
                 summaryData = NULL,
                 rtData = NULL,
                 casesInfectionData = NULL,
                 casesReportData = NULL,
                 obsCasesData = NULL,
                 activeArea = 'United Kingdom',
                 activeData = 'R0',
                 activeTime = 'all',
                 runDate = NULL,
                 width = 1000,
                 height = 1250,
                 elementId = NULL) {

  #check on subnational estimates
  #check map names against data names

  arg_types <- sapply(ls(), function(x){return(class(get(x)))})

  if(!is.null(geoData)){
    if (!'sf' %in% unlist(arg_types['geoData'])){stop('geoData must be an sf object')}
  }

  if(!is.null(summaryData)){
    if (!'data.frame' %in% unlist(arg_types['summaryData'])){stop('summaryData must be an data.frame object')}
  }

  if(!is.null(rtData)){
    if (!'data.frame' %in% unlist(arg_types['rtData'])){stop('rtData must be an data.frame object')}
  }

  if(!is.null(casesInfectionData)){
    if (!'data.frame' %in% unlist(arg_types['casesInfectionData'])){stop('casesInfectionData must be an data.frame object')}
  }

  if(!is.null(casesReportData)){
    if (!'data.frame' %in% unlist(arg_types['casesReportData'])){stop('casesReportData must be an data.frame object')}
  }

  if(!is.null(obsCasesData)){
    if (!'data.frame' %in% unlist(arg_types['obsCasesData'])){stop('obsCasesData must be an data.frame object')}
  }

  #need to check that columns are in format accepted by rt vis for global datasets
  expected_columns <- list(
    geoData = c('sovereignt', 'geometry'),
    rtData = c('country','date','type','median','lower_90','upper_90','lower_50','upper_50'),
    obsCasesData = c('region','date','confirm')
  )

  check_input_columns <- function(data){

    if (deparse(substitute(data)) == 'geoData'){
      return(length(setdiff(expected_columns[['geoData']], colnames(data))) == 0)
    } else if (deparse(substitute(data)) %in% c('rtData', 'casesInfectionData', 'casesReportData')){
      return(length(setdiff(expected_columns[['rtData']], colnames(data))) == 0)
    } else if (deparse(substitute(data)) == 'obsCasesData'){
      return(length(setdiff(expected_columns[['obsCasesData']], colnames(data))) == 0)
    } else {
      stop('Unknown dataset "', deparse(substitute(data)), '"  input for check_input_columns')
    }

  }

  if (!is.null(geoData)){
    if (!check_input_columns(geoData)){stop("geoData missing required columns. geoData must contain: ", paste(expected_columns[['geoData']], collapse = ' '))}
  }

  if (!is.null(rtData)){
    if (!check_input_columns(rtData)){stop("rtData missing required columns. rtData must contain: ", paste(expected_columns[['rtData']], collapse = ' '))}
  }

  if (!is.null(casesInfectionData)){
    if (!check_input_columns(casesInfectionData)){stop("casesInfectionData missing required columns. casesInfectionData must contain: ", paste(expected_columns[['rtData']], collapse = ' '))}
  }

  if (!is.null(casesReportData)){
    if (!check_input_columns(casesReportData)){stop("casesReportData missing required columns. casesReportData must contain: ", paste(expected_columns[['rtData']], collapse = ' '))}
  }

  if (!is.null(obsCasesData)){
    if (!check_input_columns(obsCasesData)){stop("obsCasesData missing required columns. obsCasesData must contain: ", paste(expected_columns[['obsCasesData']], collapse = ' '))}
  }

  if (!is.null(geoData)){
    geoData <- geojsonsf::sf_geojson(geoData)
  } else {
    geoData <- geoData
  }

  jsonNull <- function(data){
    if (!is.null(data)){
      return(jsonlite::toJSON(data))
    } else {
      return(data)
    }
  }

  # forward options using x
  x = list(
    activeArea = activeArea,
    activeData = activeData,
    activeTime = activeTime,
    runDate = runDate,
    geoData = geoData,
    summaryData = jsonNull(summaryData),
    rtData = jsonNull(rtData),
    casesInfectionData = jsonNull(casesInfectionData),
    casesReportData = jsonNull(casesReportData),
    obsCasesData = jsonNull(obsCasesData)
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

#' Shiny bindings for RtD3
#'
#' Output and render functions for using RtD3 within Shiny
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
#' @name RtD3-shiny
#'
#' @export
RtD3Output <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'RtD3', width, height, package = 'RtD3')
}

#' @rdname RtD3-shiny
#' @export
renderRtD3 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, RtD3Output, env, quoted = TRUE)
}
