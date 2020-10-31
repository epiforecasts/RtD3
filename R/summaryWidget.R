#' @title summaryWidget
#'
#' @description Create an Rt visualisation using D3. Need convenience functions to define defaults
#'
#' @param geoData sf object, map data
#' @param rtData data.frame, rt estimates in the format {'Source':{'rtData':x, 'casesInfectionData':x, 'casesReportData':x, 'obsCasesData':x}, ...}
#' @param subregional_ref list, reference to subnational estimates in the format {'country_name':'url', ...}.
#' @param ts_color_ref list, reference for colors for timeseries plots.
#' @param ts_bar_color string, color of observed cases bars in timeseries plots.
#' @param projection string, map projection, must be names in d3-projection.
#' @param map_legend_ref list, reference for map legend variables
#' @param credible_threshold integer, Threshold for credible intervals, maximum observed cases * this value will be removed.
#' @param dryRun Logical, defaults to FALSE. Should the function be tested without the widget being created.
#' Useful for checking the integrity of input data.
#' @importFrom htmlwidgets createWidget
#'
#' @export

summaryWidget <- function(geoData = NULL,
                          rtData = NULL,
                          subregional_ref = NULL,
                          ts_color_ref=NULL,
                          ts_bar_color='lightgrey',
                          projection='geoEquirectangular',
                          map_legend_ref=NULL,
                          credible_threshold=10,
                          dryRun = FALSE) {

  # forward options using x
  x = list(
    geoData = geojsonNull(geoData),
    rtData = jsonNull(rtData),
    subregional_ref = subregional_ref,
    ts_color_ref = ts_color_ref,
    ts_bar_color = ts_bar_color,
    projection = projection,
    map_legend_ref = map_legend_ref,
    credible_threshold = credible_threshold
  )

  if (!dryRun) {
    # create widget
    htmlwidgets::createWidget(
      name = 'RtD3',
      x,
      width = 1000,
      height = 1000,
      package = 'RtD3',
      elementId = NULL
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
