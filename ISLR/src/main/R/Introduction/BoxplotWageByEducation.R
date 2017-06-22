#'@author Divakant Pandey
#'
#'@title Boxplot Wage by education
#'
#'@Description Source ISLR data
#'
library(ggplot2)
library(ISLR)

ggplot(Wage, aes(education, wage)) + geom_boxplot(aes(color = education),
                                                  outlier.colour = "red",
                                                  outlier.shape = 1) + scale_x_discrete(labels = abbreviate)