#'@author Divakant Pandey
#'
#'@Description Convert NCI160$data to DF
#'
library(ggplot2)
library(ISLR)

#' 6830*64 DF
df <- data.frame(matrix(unlist(NCI60$data), nrow=6830, byrow=T))

#' 64*6830 DF
dfByCol<-do.call(rbind.data.frame, NCI60["data"])

