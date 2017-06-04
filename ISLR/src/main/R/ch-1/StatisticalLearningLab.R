#'@author Divakant Pandey
#'
#'@title Lab Exercise ch 2
#'
#'@Description ISLR data
#'
x <- c (1 , 3 , 2 , 5)
x
length(x)
sqrt (x)

ls()
rm(list = ls())


#' Default matrix population by column
matrix (data = c (1 , 2 , 3 , 4) ,
        nrow = 2 ,
        ncol = 2)

#' Default matrix population by row
matrix (
  data = c (1 , 2 , 3 , 4) ,
  nrow = 2 ,
  ncol = 2,
  byrow = TRUE
)

#' rnorm generates n random normal distribution variable
x = rnorm(50)

#'By default, rnorm() creates standard normal random variables with a mean of 0 and a standard deviation of 1
y = x + rnorm(50, mean = 50, sd = 0.1)

#' cor fn is used for comparing corelation between variable
cor(x, y)

#' similar output for random nos
set.seed (3)

y = rnorm(100)

mean(y)

var(y)

sqrt(var(y))

sd(y)

x = rnorm (100)
y = rnorm (100)
plot (x , y)

plot (x ,
      y ,
      xlab = " this is the x - axis " ,
      ylab = " this is the y - axis " ,
      main = " Plot of X vs Y ")

#' pdf func is used to save the plot, 
#' dev.off is used to indiacate that we are done with plot
pdf("ISLR/src/main/resources/Figure.pdf")
plot (x ,y , col =" green ")
dev.off ()

#'seq() can be used to create a sequence of numbers
seq (1 ,10)
#'short form of seq
1:10

#' contour() function produces a contour plot in order to represent three-dimensional data; contour plot
#' it is like a topographical map
set.seed(3)
x = 1:100
y=x
f = outer (x ,y , function (x , y ) cos ( y ) /(1+ x ^2) )
contour (x ,y , f )
contour (x ,y ,f , nlevels =45 , add = T )

#' t gives transpose 
fa =( f - t ( f ) ) /2
contour (x ,y , fa , nlevels =15)

#'image() function produces a color-coded plot whose colors depend on the z value,
#'also known as heatmap, and is sometimes used to plot temperature in weather heatmap forecasts.
image (x ,y , fa )

#'persp() can be used to produce a three-dimensional plot. 
#'The arguments theta and phi control the angles at which the plot is viewed.
persp (x ,y , fa )
persp (x ,y , fa , theta =30)
persp (x ,y , fa , theta =30 , phi =40)

#' Indexing Data - examine part of a set of data
A = matrix (1:16 ,4 ,4)
A
#' 2nd Row, 3rd column
A [2 ,3]

#' 1st row and 3rd row - (2,4) column
A [ c (1 ,3) , c (2 ,4) ]

#' 1st row , 2nd and 3rd row - (2,3,4) column
A [1:3 ,2:4]

#' All columns from 1st and 2nd row
A [1:2 ,]

#' All rows from 1st and 2nd column
A [,1:2]

#' All except 1st and 3rd row
A [ - c (1 ,3) ,]

#' dim outputs the number of rows followed by the number of columns of a given matrix
dim(A)
 
#'read.table() function load data from a text file
#'fix() function can be used to view it in a spreadsheet like window
#'na.strings tells R that any time it sees a particular character or set of characters 
#'(such as a question mark), it should be treated as a missing element of the data matrix
a=read.table("ISLR/src/main/resources/input.csv", header =T , na.strings ="?") 
fix(a)

a=read.csv("ISLR/src/main/resources/input.csv", header =T , na.strings ="?") 
fix(a)
dim(a)

#' na.omit removes missing data
a=na.omit(a)
fix(a)
dim(a)

#'Once the data are loaded correctly, we can use names() to check the variable names
names(a)

#' attach func make the variables in this data frame available by name
library(ISLR)
attach(Auto)

#'as.factor() function converts quantitative variables into qualitative variables
#'If the variable plotted on the x-axis is categorial, then boxplots will
#'automatically be produced by the plot() function
cylinders=as.factor(cylinders)
plot(cylinders,mpg,col="red",varwidth=T,horizontal=T,xlab="Cylinders",ylab="MPG")

#' Histogram
hist(mpg,col = 2,breaks = 15)

#'The pairs() function creates a scatterplot matrix i.e. a scatterplot for every
#'pair of variables for any given data set. We can also produce scatterplots
#'for just a subset of the variables
pairs ( Auto )

#'Subset of columns
pairs (~mpg + displacement + horsepower + weight +acceleration , Auto )

#'identify() provides a useful interactive method for identifying the value 
#'for a particular variable for points on a plot.
plot ( horsepower , mpg )
#identify ( horsepower , mpg , name )

#' Save command history
savehistory()

#' Load Command history
loadhistory()
