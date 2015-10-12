# This is the R script used to do the analysis for PS4 prob 4

#Start analysis ----

require(ggplot2)
set.seed(0)
# also a good place to set global chunk options
library(knitr) # need this for opts_chunk command
opts_chunk$set(fig.width = 5, fig.height = 5)
# if we wanted chunks by default not to be evaluated
# opts_chunk$set(eval = FALSE) 
set.seed(0)
setwd("~/src/stat243/ps4")
library(stringr)
library(curl)
library(methods)
library(XML)
library(RCurl)
library(devtools)
library(gsubfn)
library(microbenchmark)
require(rbenchmark)
library(pryr)
options(stringsAsFactors = FALSE)


# Start ----

gc(reset=TRUE)
source("my_lm_4a.R")
source("my_lm_4b.R")


n=10^6

y<-rnorm(n)
x1<-rnorm(n)
x2<-rnorm(n)
x3<-rnorm(n)

# Analysis for Question 4 a) ----



startMem<-mem_used()
startMem # Print memory use at start
m1<-my_lm_4a(y~x1+x2+x3)
rm(m1)



# Analysis for Question b) -----


startMem<-mem_used()
startMem # Print memory use at start

m2<-my_lm_4b(y~x1+x2+x3)
rm(m2)



