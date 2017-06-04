#'@author Divakant Pandey
#'
#'@title Year vs Wage with trendline
#'
#'@Description ISLR data
#'
library(ggplot2)
library(ISLR)

ggplot(Wage, aes(year, wage)) + geom_point(aes(color = year)) + geom_smooth(method = lm)
