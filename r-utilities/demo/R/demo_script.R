
#' A Demo Function
#'
#' This function is a sample demo of Package Creation.
#' @param flag : Defaults to TRUE.
#' @keywords demo
#' @export
#' @examples
#' demo_fn()

demo_fn <- function(flag=TRUE){
  
  if(flag){
    return("Returned TRUE")
  }else{
    return("Returned FALSE")
  }
  
}