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
#'                   areas = c('Afghanistan', 'Brazil', 'Colombia', 'United States'))
#'
getSubregionalUrls <- function(path, areas) {

  tweaked_areas <- sub(" ", "-", areas)
  tweaked_areas <- tolower(tweaked_areas)
  subregional_ref <-  paste0(path, tweaked_areas, "/")
  names(subregional_ref) <- areas

  return(subregional_ref)
}
