#'@author Divakant Pandey
#'
#'@title Boxplot today by Direction
#'
#'@Description Source ISLR data
#'
library(ggplot2)
library(ISLR)

ggplot(Smarket, aes(Direction, (Lag2 - Lag1))) + geom_boxplot(aes(color =
                                                                    Direction),
                                                              outlier.color = "red",
                                                              outlier.shape = 1) + labs(y = "Today")