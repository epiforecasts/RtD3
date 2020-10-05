#' @title summaryWidget
#'
#' @description Create an Rt visualisation using D3
#'
#' @param geoData sf object, map data
#' @param rtData data.frame, rt estimates in the format {'Source':{'rtData':x, 'casesInfectionData':x, 'casesReportData':x, 'obsCasesData':x}, ...}
#' @param subregional_ref list, reference to subnational estimates in the format {'country_name':'url', ...}.
#' @param activeArea character, the default area to plot.
#' @param activeTime character, the default time window (defaults to 'all')
#' @param runDate character, date of estimate run in the format ('YYYY-MM-DD')
#' @param width integer, width in pixels
#' @param elementId string, id of element
#' @param dryRun Logical, defaults to FALSE. Should the function be tested without the widget being created.
#' @param downloadUrl string, optional URL to download datasets
#' @param ts_color_ref list, default reference for time series plots. See default_ts_colors for format.
#' Useful for checking the integrity of input data.
#' @importFrom htmlwidgets createWidget
#'
#' @export

summaryWidget <- function(geoData = NULL,
                          rtData = NULL,
                          activeArea = NULL,
                          activeTime = 'all',
                          runDate = NULL,
                          subregional_ref = NULL,
                          width = 900,
                          elementId = NULL,
                          dryRun = FALSE,
                          downloadUrl = NULL,
                          ts_color_ref = NULL) {

  arg_types <- sapply(ls(), function(x){return(class(get(x)))})

  invisible(check_input_data(arg_types = arg_types, geoData = geoData, rtData = rtData))

  #warn for geoData name intersection issues
  if (!is.null(geoData)){
    check_geoData_names(geoData = geoData, rtData = rtData)
  }

  #define height, which is fixed based on dataset availability
  height <- define_height(geoData = geoData, rtData = rtData)

  #if ts color ref is null, use default colors
  if (is.null(ts_color_ref)){
    ts_color_ref <- default_ts_colors()
  }

  # forward options using x
  x = list(
    activeArea = activeArea,
    activeTime = activeTime,
    runDate = runDate,
    geoData = geojsonNull(geoData),
    rtData = jsonNull(rtData),
    subregional_ref = subregional_ref,
    fullWidth = width,
    downloadUrl = downloadUrl,
    ts_color_ref = ts_colors
  )

  if (!dryRun) {
    # create widget
    htmlwidgets::createWidget(
      name = 'RtD3',
      x,
      width = width,
      height = height,
      package = 'RtD3',
      elementId = elementId
    )
  }else{
    return(TRUE)
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
