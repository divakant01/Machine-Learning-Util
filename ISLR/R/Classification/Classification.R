#'@author Divakant Pandey
#'
#'@title Classsification
#'
#'@Description Theory
library(ISLR)
library(ggplot2)

attach(Default)

# Scatter Plot for Scatter Plot (Balance vs Income By Defaults), Orange defaults and Blue not
ggplot(Default, aes(x = balance, y = income, group = default)) + geom_point(aes(shape =
                                                                                  default, color = default)) + xlab("Balance") + ylab("Income") + theme(legend.position = "bottom") +
  scale_color_manual(values = c('Blue', 'Orange')) + scale_shape_manual(values = c(1, 3))

#Box Plot for Default /Balance
ggplot(Default, aes(default, balance, fill = default)) + geom_boxplot() +
  scale_fill_manual(values = c('blue', 'orange'))

#Boxplot for Default by Income
ggplot(Default, aes(default, income, fill = default)) + geom_boxplot() +
  scale_fill_manual(values = c('blue', 'orange'))



