#' Read in Results from EpiNow2
#'
#' @description Reads in results from `EpiNow2` and converts them into the `RtD3` format. Supports
#' either input via a list object or from a file path/url.
#' @param input_list A list of results as returned by `EpiNow2::regional_summary`
#' @param path A character string indicating the path (either file or URL) to the summary results
#' @param region_var A character string that identifies the region name used.
#' @return A named list in the format required by `summaryWidget` along with a summary table.
#' @export
#' @importFrom data.table fread
#' @examples
#' # Read in each summary folder
#' rtData <- readInEpiNow2(path = "https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary",
#'                         region_var = "country")
#'
#' rtData
readInEpiNow2 <- function(input_list, path, region_var = "region") {

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

  ## Capitalise words using base r
  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                             {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }

  out$summary <- data.table::setnames(out$summary, capwords(region_var), "Country")

  rename_col <- function(df) {
    df <- data.table::setnames(df, region_var, "country")
  }

  out$rtData <- rename_col(out$rtData)
  out$casesInfectionData <- rename_col(out$casesInfectionData)
  out$casesReportData <- rename_col(out$casesReportData)

  return(out)
}
