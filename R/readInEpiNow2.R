#' Read in Results from EpiNow2
#'
#' @description Reads in results from `EpiNow2` and converts them into the `RtD3` format. Supports
#' either input via a list object or from a file path/url.
#' @param input_list A list of results as returned by `EpiNow2::regional_summary`
#' @param path A character string indicating the path (either file or URL) to the summary results
#'
#' @return A named list in the format required by `summaryWidget` along with a summary table.
#' @export
#' @importFrom data.table fread
#' @examples
#' # Define the base URL/file path for the estimates
#' base_url <- 'https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/'
#'
#' # Read in each summary folder
#' rtData <- readInEpiNow2(path = paste0(base_url, "cases/summary"))
#'
#' rtData
readInEpiNow2 <- function(input_list, path) {

  if (missing(input_list) & missing(path)) {
    stop("Either a list or a file path/url must be supplied")
  }

  if (!missing(input_list) & !missing(path)) {
    message("Both a list and a filepath has been supplied. Defaulting to the list.")
  }


  if (!missing(input_list)) {
    out <- list('summary' = input_list$summarised_results$table,
                'rtData' = input_list$summarised_measures$rt,
                'casesInfectionData' = input_list$summarised_measures$cases_by_infection,
                'casesReportData' =  input_list$summarised_measures$cases_by_report,
                'obsCasesData' =  input_list$reported_cases)

  }
  if (!missing(path)) {

    path <- paste0(path, "/")

    out <- list('summary' = data.table::fread(paste0(path, 'summary_table.csv')),
                'rtData' = data.table::fread(paste0(path, 'rt.csv')),
                'casesInfectionData' = data.table::fread(paste0(path, 'cases_by_infection.csv')),
                'casesReportData' = data.table::fread(paste0(path, 'cases_by_report.csv')),
                'obsCasesData' = data.table::fread(paste0(path, 'reported_cases.csv')))
  }

  return(out)
}
