#'@author Divakant Pandey
#'
#'@title Boxplot Wage by education per race
#'
#'@Description Source ISLR data
#'
library(ggplot2)
library(ISLR)


ggplot(Wage, aes(education, wage)) + geom_boxplot(aes(color = race),
                                                  outlier.colour = "red",
                                                  outlier.shape = 1) + scale_x_discrete(labels = abbreviate)