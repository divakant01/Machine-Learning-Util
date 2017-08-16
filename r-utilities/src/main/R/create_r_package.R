
library(devtools)
library(roxygen2)

create("demo")

# Add R Code
# Add Documentation

setwd("demo")
roxygen2::document()

setwd("..")

install("demo")


library(demo)

demo_fn(FALSE)
