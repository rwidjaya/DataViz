# Set working folder for Assignment 1
setwd("./Documents/DataViz/ass1")
getwd()


# Download & Load necessary packages
install.packages('readr')
install.packages('haven')
install.packages('dplyr')
install.packages('tidyr')
install.packages('stringr')
install.packages('ggplot2')

library(readr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Alternative: 
#   install.packages('tidyverse')
#   library(tidyverse)


# Loading data to tibbles 
acc2015 <- read_csv('accident.csv')
acc2014 <- read_sas('accident.sas7bdat')


# Fill "" with NA
acc2014 <- acc2014 %>% mutate(TWAY_ID2 = na_if(TWAY_ID2, ''))
table(is.na(acc2014$TWAY_ID2))


# Identify column discrepancy between 2 data frames 
dim(acc2014)
dim(acc2015)

# Alternative:
##  setdiff(colnames(acc2015), colnames(acc2014))
##  setdiff(colnames(acc2014), colnames(acc2015))

colnames(notcol <- acc2015[!(colnames(acc2015) %in% colnames(acc2014))])
colnames(notcol2 <- acc2014[!(colnames(acc2014) %in% colnames(acc2015))])

# Comment:
# Four (4) missing variables: 
#  - 2015 NOT in 2014: RUR_URB, FUNC_SYS, RD_OWNER
#  - 2014 NOT in 2015: ROAD_FNC


# Merge 2 data frames 
acc <- bind_rows(acc2014, acc2015)
count(acc, 'RUR_URB')


# Comment:
#   Why is there 30k NAs in RUR_URB variables?
#     Because acc2014 does not have a RUR_URB variable, 
#     the bind_rows() function automatically do an outer join 
#     on both tibbles and filled missing datas from acc2014 with NA.
#     The amount of NA is checks out with the amount of rows in 
#     acc2014.

# Merging fips dataset
fips <- read_csv('fips.csv')


# Transforming int to char 
acc$STATE <- as.character(acc$STATE)
acc$COUNTY <- as.character(acc$COUNTY)


# Adding zeros to State and County Codes
acc$COUNTY <- str_pad(acc$COUNTY, 3, side = 'left', pad = '0')
acc$STATE <- str_pad(acc$STATE, 2, side = 'left', pad = '0')


# Renaming variables and merging Accident and FIPS dataset
acc <- plyr::rename(acc, c('STATE' = 'StateFIPSCode', 'COUNTY' = 'CountyFIPSCode'))
acc <- left_join(acc, fips, by = c("StateFIPSCode", "CountyFIPSCode"))


# DATA EXPLORATION
## 1
agg <- summarise(group_by(acc, StateName, YEAR), TOTAL = sum(FATALS))

## 2
agg_wide <- spread(agg, YEAR, TOTAL)

## 3
agg_wide <- plyr::rename(agg_wide, c("2014" = 'Year2014', "2015" = 'Year2015'))
agg_wide <- mutate(agg_wide, Diff_Percent = ((Year2015-Year2014)/Year2014)*100)

## 4
agg_wide <- arrange(agg_wide, desc(agg_wide$Diff_Percent))

## 5
agg_wide <- filter(agg_wide, Diff_Percent > 15, !is.na(StateName))

## Chain Function 
agg <- acc %>% 
  group_by(StateName, YEAR) %>%
  summarise(TOTAL = sum(FATALS)) %>%
  spread(YEAR, TOTAL) %>%
  plyr:: rename(c("2014" = 'Year2014', "2015" = 'Year2015')) %>%
  mutate(Diff_Percent = ((Year2015-Year2014)/Year2014)*100) %>%
  arrange(desc(Diff_Percent)) %>%
  filter(Diff_Percent > 15, !is.na(StateName))
 
