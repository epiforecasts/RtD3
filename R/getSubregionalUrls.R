#' Get Subregional Estimate Urls
#'
#' @param path A character string container the overall path to subnational estimates
#' @param areas A character vector listing the subregional estimates (assuming that listed in the
#' geoData with capitalisation and without capitalisation in the path).
#'
#' @return A named list of subnational urls.
#' @export
#'
#' @examples
#'
#'getSubregionalUrls(path = "https://epiforecasts.io/covid/posts/national/",
#'                   areas = c('Afghanistan', 'Brazil', 'Colombia', 'India'))
#'
getSubregionalUrls <- function(path, areas) {

  subregional_ref <-  paste0(path, tolower(areas), "/")
  names(subregional_ref) <- areas

  return(subregional_ref)
}
