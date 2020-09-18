#' @title mapWidget
#'
#' @description Create a map of Rt summary data
#'
#' @param geoData sf object, map data
#' @param rtData data.frame, rt estimates in the format {'Source':{'rtData':x, 'casesInfectionData':x, 'casesReportData':x, 'obsCasesData':x}, ...}
#' @param width integer, width in pixels
#' @param elementId string, id of element
#' @param dryRun Logical, defaults to FALSE. Should the function be tested without the widget being created.
#' Useful for checking the integrity of input data.
#' @importFrom htmlwidgets createWidget
#'
#' @export

mapWidget <- function(geoData = NULL,
                     rtData = NULL,
                     width = 900,
                     elementId = NULL,
                     dryRun = FALSE) {

  arg_types <- sapply(ls(), function(x){return(class(get(x)))})

  invisible(check_input_data(arg_types = arg_types, geoData = geoData, rtData = rtData))

  #define height, which is fixed based on dataset availability
  height <- define_height(geoData = geoData, rtData = rtData, map_only = T)

  rtData_null <- list('summaryData' = NULL, 'rtData' = NULL, 'casesInfectionData' = NULL, 'casesReportData' = NULL, 'obsCasesData' = NULL)

  rtData_replacement <- list()

  for (source in names(rtData)){

    rtData_null$summaryData <- rtData[[source]]$summaryData

    rtData_replacement[[source]] <- rtData_null$summaryData
  }

  print(rtData_replacement)

  x = list(
    activeArea = NULL,
    activeTime = NULL,
    runDate = NULL,
    geoData = geojsonNull(geoData),
    rtData = jsonNull(rtData_replacement),
    subregional_ref = NULL
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
