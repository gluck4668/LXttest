\name{LXttest}
\alias{LXttest}
\title{LXttest}
\usage{
LXttest(data)
}
\description{
It can be used for t test.
}
\examples{

if(!requireNamespace("devtools"))
  install.packages("devtools")

library(devtools)

install_github("gluck4668/LXttest")

library(LXttest)

??LXttest

#------------------------------------
data(ttest_data_example)

#------------------------------------


setwd("D:/Desktop/R_example/LXttest_exampl")

data <- "ttest_example.xlsx"

LXttest(data)

}
