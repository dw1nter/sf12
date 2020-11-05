# Title     : SF-12 Reader
# Objective : Read table SF-12 from Highway Statistics website and save as a machine readable .CSV file.
# Created by: David.Winter
# Created on: 11/5/2020

library (tidyverse)
library(readxl)

# Working directory where output file is saved.
setwd("S:/HPPI/DataHub Files/sf12/")

area <- as.character
year <- as.numeric
sf_combined <- data.frame(matrix(ncol=6, nrow=0))

#year <- 2015:1995

for (year in 2000:1995){

  startRow <- 12

  # Reading input Excel file from OHPI website
short_year <- substr(year,3,4)
if (year > 2006){
  sf12_url <- paste0('https://www.fhwa.dot.gov/policyinformation/statistics/',year,'/xls/sf12.xls')
}
else if (year>2001){
  sf12_url <- paste0('https://www.fhwa.dot.gov/policy/ohim/hs',short_year,'/xls/sf12.xls')
}
else if (year>1999){
  sf12_url <- paste0('https://www.fhwa.dot.gov/ohim/hs',short_year,'/xls/sf12.xls')
}
else {
  sf12_url <- paste0('https://www.fhwa.dot.gov/ohim/hs',short_year,'/excel/sf12.xls')
}
  destination_file <- paste0(year,"sf12-download-fhwa.xls")
 # https://www.fhwa.dot.gov/policy/ohim/hs06/xls/sf12.xls

  if (!file.exists(destination_file)){
    download.file(sf12_url, destfile = destination_file, mode="wb")
  }
  # Read data from rural tab (SF12P1)
  sf_rural_raw <- read_excel(destination_file, sheet="SF12P1", col_types = "text", skip=startRow)


  #as.character(area)
  area <- "Rural"

  colnames (sf_rural_raw) <- c("state_name",
                              "Capital_Interstate",
                             # "Capital_FreewayExpress",
                              "Capital_OtherPrinArt",
                              "Capital_MinArt",
                              "Capital_MajCol",
                              "Capital_MinCol",
                              "Capital_Total",
                              "Maintenance_Interstate",
                            #  "Maintenance_FreewayExpress",
                              "Maintenance_OtherPrinArt",
                              "Maintenance_MinArt",
                              "Maintenance_MajCol",
                              "Maintenance_MinCol",
                              "Maintenance_Total"
  )
  # Deleting blank row at top, total columns, and rows.
  sf_rural_raw <- sf_rural_raw[-1,]
  sf_rural_raw <- sf_rural_raw %>% select(-c('Capital_Total','Maintenance_Total'))

  #remove total
  sf_rural_raw <- sf_rural_raw %>% filter(state_name != 'Total')
  sf_rural_raw <- sf_rural_raw %>% filter(state_name != 'Percent')

  # Taking cleaned up data table and pivoting to machine readable format.
 sf_rural_tidy <- sf_rural_raw %>%
    pivot_longer('Capital_Interstate':'Maintenance_MinCol', names_to="COLUMN_KEY", values_to='Outlays') %>%
    separate(COLUMN_KEY, into = c("Category", "Functional System"), sep='_')

  sf_rural_tidy <- sf_rural_tidy %>% add_column(area,.after = 'state_name')
  sf_rural_tidy <- sf_rural_tidy %>% add_column(year,.before = 'state_name')

  # Read data from small urban tab (SF12P2)
  sf_small_raw <- read_excel(destination_file, sheet="SF12P2", col_types = "text", skip=startRow)

    area <- "Small Urban"
  
  colnames (sf_small_raw) <- c("state_name",
                                "Capital_Interstate",
                            #    "Capital_FreewayExpress",
                                "Capital_OtherPrinArt",
                                "Capital_MinArt",
                                "Capital_MajCol",
                                "Capital_MinCol",
                                "Capital_Total",
                                "Maintenance_Interstate",
                            #    "Maintenance_FreewayExpress",
                                "Maintenance_OtherPrinArt",
                                "Maintenance_MinArt",
                                "Maintenance_MajCol",
                                "Maintenance_MinCol",
                                "Maintenance_Total"
  )
  # Deleting blank row at top, total columns, and rows.
  sf_small_raw <- sf_small_raw[-1,]
  sf_small_raw <- sf_small_raw %>% select(-c('Capital_Total','Maintenance_Total'))

  #remove total
  sf_small_raw <- sf_small_raw %>% filter(state_name != 'Total')
  sf_small_raw <- sf_small_raw %>% filter(state_name != 'Percent')

  # Taking cleaned up data table and pivoting to machine readable format.
  sf_small_tidy <- sf_small_raw %>%
    pivot_longer('Capital_Interstate':'Maintenance_MinCol', names_to="COLUMN_KEY", values_to='Outlays') %>%
    separate(COLUMN_KEY, into = c("Category", "Functional System"), sep='_')

  sf_small_tidy <- sf_small_tidy %>% add_column(area,.after = 'state_name')
  sf_small_tidy <- sf_small_tidy %>% add_column(year,.before = 'state_name')

  # Read data from urbanized area tab (SF12P3)
  sf_urban_raw <- read_excel(destination_file, sheet="SF12P3", col_types = "text", skip=startRow)

  area <- "Urbanized"

  colnames (sf_urban_raw) <- c("state_name",
                               "Capital_Interstate",
                            #   "Capital_FreewayExpress",
                               "Capital_OtherPrinArt",
                               "Capital_MinArt",
                               "Capital_MajCol",
                               "Capital_MinCol",
                               "Capital_Total",
                               "Maintenance_Interstate",
                           #    "Maintenance_FreewayExpress",
                               "Maintenance_OtherPrinArt",
                               "Maintenance_MinArt",
                               "Maintenance_MajCol",
                               "Maintenance_MinCol",
                               "Maintenance_Total"
  )
  # Deleting blank row at top, total columns, and rows.
  sf_urban_raw <- sf_urban_raw[-1,]
  sf_urban_raw <- sf_urban_raw %>% select(-c('Capital_Total','Maintenance_Total'))

  #remove total
  sf_urban_raw <- sf_urban_raw %>% filter(state_name != 'Total')
  sf_urban_raw <- sf_urban_raw %>% filter(state_name != 'Percent')

  # Taking cleaned up data table and pivoting to machine readable format.
  sf_urban_tidy <- sf_urban_raw %>%
    pivot_longer('Capital_Interstate':'Maintenance_MinCol', names_to="COLUMN_KEY", values_to='Outlays') %>%
    separate(COLUMN_KEY, into = c("Category", "Functional System"), sep='_')

  sf_urban_tidy <- sf_urban_tidy %>% add_column(area,.after = 'state_name')
  sf_urban_tidy <- sf_urban_tidy %>% add_column(year,.before = 'state_name')

  sf_tidy <- rbind(sf_rural_tidy, sf_small_tidy, sf_urban_tidy)

  # Saving machine readable file.
  write.csv(sf_tidy,file = paste0(year,'_sf12.csv'))

sf_combined <- rbind(sf_combined,sf_tidy)

 # year <- year-1
}
# Saving machine readable file.
write.csv(sf_combined,file = 'combined_sf12.csv')


