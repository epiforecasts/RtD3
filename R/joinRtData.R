
#' Join RtData
#'
#' @description Joins two nested lists in the format required by `summaryWidget`. This may
#' be useful for merging estimates from disparate data sources or linking national level estimates
#' with subnational estimates
#' @param rtData A nested list as required by `summaryWidget`
#' @param rtData2  A nested list as required by `summaryWidget`
#'
#' @return A nested list as required by `summaryWidget`
#' @importFrom purrr map2
#' @importFrom data.table rbindlist
#' @export
#' @examples
#'
#' \donttest{
#' base_url <- "https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/"
#' subnational <-   national <- list("Cases" = readInEpiNow2(
#' path = paste0(base_url, "subnational/italy/cases/summary"),
#' region_var = "region"))
#'
#'
#' national <- list("Cases" = readInEpiNow2(
#' path = paste0(base_url, "national/cases/summary"),
#' region_var = "country"),
#' regions = "Italy")
#'
#' out <- list()
#' out$Cases <- joinRtData(subnational$Cases, national$Cases)
#' }

joinRtData <- function(rtData, rtData2) {
  rtData <- purrr::map2(rtData, rtData2,
                        ~ data.table::rbindlist(list(
                          .x[, lapply(.SD, as.character)],
                          .y[, lapply(.SD, as.character)]),
                          use.names = TRUE, fill = TRUE))
  return(rtData)
}
