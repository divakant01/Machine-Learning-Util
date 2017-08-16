# web_service.R

#* @get /mean
normalMean <- function(samples=10){
  data <- rnorm(samples)
  mean(data)
}

#* @post /sum
addTwo <- function(a, b){
  as.numeric(a) + as.numeric(b)
}



normalMean1 <- function(samples=10){
  data <- rnorm(samples)
  mean(data)
}

addTwo1 <- function(a, b){
  as.numeric(a) + as.numeric(b)
}


