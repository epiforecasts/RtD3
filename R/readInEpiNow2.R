#' Read in Results from EpiNow2
#'
#' @description Reads in results from `EpiNow2` and converts them into the `RtD3` format. Supports
#' either input via a list object or from a file path/url.
#' @param input_list A list of results as returned by `EpiNow2::regional_summary`
#' @param path A character string indicating the path (either file or URL) to the summary results
#' @param region_var A character string that identifies the region name used.
#' @param regions A character string indicating the regions of interest to returns. Defaults to all regions.
#' @return A named list in the format required by `summaryWidget` along with a summary table.
#' @export
#' @importFrom data.table fread setnames
#' @examples
#' # Read in each summary folder
#'
#' base_path <- "https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/"
#' rtData <- readInEpiNow2(
#'   path = paste0(base_path, "master/national/cases/summary"),
#'   region_var = "country")
#'
#' rtData
#'
#'
#'
#' france <- readInEpiNow2(
#'   path = paste0(base_path, "master/national/cases/summary"),
#'   region_var = "country",
#'   regions = "France")
#'
#' france
readInEpiNow2 <- function(input_list, path, region_var = "region", regions) {

  if (missing(regions)) {
    regions <- NULL
  }

  if (missing(input_list) & missing(path)) {
    stop("Either a list or a file path/url must be supplied")
  }

  if (!missing(input_list) & !missing(path)) {
    message("Both a list and a filepath has been supplied. Defaulting to the list.")
  }

  if (!missing(input_list)) {
    out <- list('summaryData' = input_list$summarised_results$table,
                'rtData' = input_list$summarised_measures$rt,
                'casesInfectionData' = input_list$summarised_measures$cases_by_infection,
                'casesReportData' =  input_list$summarised_measures$cases_by_report,
                'obsCasesData' =  input_list$reported_cases)

  }
  if (!missing(path)) {

    path <- paste0(path, "/")

    out <- list('summaryData' = data.table::fread(paste0(path, 'summary_table.csv'), integer64 = 'numeric'),
                'rtData' = data.table::fread(paste0(path, 'rt.csv'), integer64 = 'numeric'),
                'casesInfectionData' = data.table::fread(paste0(path, 'cases_by_infection.csv'), integer64 = 'numeric'),
                'casesReportData' = data.table::fread(paste0(path, 'cases_by_report.csv'), integer64 = 'numeric'),
                'obsCasesData' = data.table::fread(paste0(path, 'reported_cases.csv'), integer64 = 'numeric'))
  }

  ## Capitalise words using base r
  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                             {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }

  out$summaryData <- data.table::setnames(out$summaryData, capwords(region_var), "region")

  rename_col <- function(df) {
    df <- data.table::setnames(df, region_var, "region")
  }

  out$rtData <- rename_col(out$rtData)
  out$casesInfectionData <- rename_col(out$casesInfectionData)
  out$casesReportData <- rename_col(out$casesReportData)

  if (!is.null(regions)) {
    out <- lapply(out, function(df) {
      df <- df[region %in% regions]
      return(df)
    })
  }

  return(out)
}
