#' @title tsWidget
#'
#' @description Create a time series widget of Rt data
#'
#' @param rtData data.frame, rt estimates in the format {'Source':{'rtData':x, 'casesInfectionData':x, 'casesReportData':x, 'obsCasesData':x}, ...}
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

tsWidget <- function(rtData = NULL,
                          activeArea = NULL,
                          activeTime = 'all',
                          runDate = NULL,
                          width = 900,
                          elementId = NULL,
                          dryRun = FALSE,
                          downloadUrl = NULL,
                          ts_color_ref = NULL) {

  arg_types <- sapply(ls(), function(x){return(class(get(x)))})

  invisible(check_input_data(arg_types = arg_types, geoData = NULL, rtData = rtData))

  #define height, which is fixed based on dataset availability
  height <- define_height(geoData = NULL, rtData = rtData)

  #if ts color ref is null, use default colors
  if (is.null(ts_color_ref)){
    ts_color_ref <- default_ts_colors()
  }

  x = list(
    activeArea = activeArea,
    activeTime = activeTime,
    runDate = runDate,
    geoData = NULL,
    rtData = jsonNull(rtData),
    subregional_ref = NULL,
    fullWidth = width,
    downloadUrl = downloadUrl,
    ts_color_ref = ts_color_ref
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
