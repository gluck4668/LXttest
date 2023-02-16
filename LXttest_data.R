
setwd("D:/Desktop/LXttest")
library(openxlsx)

ttest_data_example <- read.xlsx("ttest_example.xlsx")

usethis::use_data(ttest_data_example,overwrite = T)

rm(list=ls())

data(ttest_data_example)


