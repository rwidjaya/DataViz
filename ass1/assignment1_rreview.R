# Set working folder for Assignment 1
setwd("./Documents/DataViz/ass1")
getwd()


# Download & Load necessary packages
install.packages(readr)
install.packages(haven)
install.packages(dplyr)
install.packages(tidyr)
install.packages(stringr)
install.packages(ggplot2)

library(readr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
## install.packages('tidyverse')
## library(tidyverse)


# Loading data to tibbles 
acc_2015 <- read_csv('accident.csv')
  # read_csv will turn char data into vector, but since we're using readr this will turn csv into TIBBLE 
acc_2014 <- read_sas('accident.sas7bdat')


# Fill "" with NA
acc2014 <- acc2014 %>% mutate(TWAY_ID2 = na_if(TWAY_ID2, ''))
table(is.na(acc2014$TWAY_ID2))


# Identify column discrepancy between 2 data frames 
dim(acc2014)
dim(acc2015)
## setdiff(colnames(acc2015), colnames(acc2014))

colnames(notcol <- acc2015[!(colnames(acc2015) %in% colnames(acc2014))])
'''
Comment:
Three columns that are missing:
  - RUR_URB:
  - FUNC_SYS:
  - RD_OWNER:
'''


# Merge 2 data frames 
acc <- bind_rows(acc2014, acc2015)
count(acc, 'RUR_URB')
'''
Comment:
  Why is there 30k NAs in the acc RUR_URB column?
    Because acc2014 does not have a RUR_URB column, 
    the bind_rows() function automatically do an outer join 
    on both tibbles and filled missing datas from acc2014 with NA.
    The amount of NA is checks out with the amount of rows in 
    acc2014.
'''







