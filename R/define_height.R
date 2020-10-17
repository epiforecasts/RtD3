

define_height <- function(geoData, rtData, map_only = F){

  if(map_only){
    return(100)
  }

  height = 0

  if(!is.null(geoData) & !is.null(unlist(rtData[[1]]['summaryData']))){
    height = height + 500
  }

  if(sum(sapply(rtData[[1]], function(x){return(!is.null(x))})[2:4]) < 3){
    height = height + (225 * sum(sapply(rtData[[1]], function(x){return(!is.null(x))})[2:4]))
  } else {
    height = height + (225 * 3)
  }

  height = height + 150

  return(height)

}
