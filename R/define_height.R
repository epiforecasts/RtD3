define_height <- function(geoData, rtData){
  
  height = 0
  
  if(!is.null(geoData) & !is.null(unlist(rtData[[1]]['summaryData']))){
    height = height + 500
  }
  
  if(sum(sapply(rtData[[1]], function(x){return(!is.null(x))})[2:4]) < 3){
    height = height + (225 * sum(sapply(rtData[[1]], function(x){return(!is.null(x))})[2:4]))
  } else {
    height = height + (225 * 3)
  }
  
  height = height + 100
  
  return(height)
  
}