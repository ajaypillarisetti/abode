setwd('~/Documents/HAPIT4')

library(parallel)
library(doMC)
library(tools)
library(readxl)
registerDoMC(8)


############################################
###        EXTRACT UN HH SIZE DATA       ###
############################################
# https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=2ahUKEwivv6_PkK7iAhWWpp4KHRAlBUYQFjAAegQIABAC&url=https%3A%2F%2Fpopulation.un.org%2FHousehold%2Fexceldata%2Fpopulation_division_UN_Houseshold_Size_and_Composition_2018.xlsx
# https://population.un.org/Household/index.html

un <- as.data.table(read_excel('raw_data/population_division_UN_Houseshold_Size_and_Composition_2018.xlsx', sheet = 4, skip=4))
un <- un[, c('Country or area', 'ISO Code', 'Data source category', 'Reference date (dd/mm/yyyy)', 'Average household size (number of members)')]
setnames(un, c('country', 'iso_code', 'source', 'date', 'hh_size'))

#create a dummy variable to identify duplicates
un[, dummy := paste(country, date, sep="")]
#get most recent estimate by country and create matching varibale to dummy
keepers <- un[, max(date), by='country'][, paste(country, V1, sep="")]
#select based on dummy
un <- un[dummy %in% keepers]
#drop dummy
un[, dummy := NULL]
#convert to year
un[, date:=year(date)]
#convert hh_size to numeric
un[, hh_size:=as.numeric(hh_size)]
#identify countries with multiple estimates
multiple_estimates <- un[, which(length(iso_code)>1), by='country'][, country]
#identify countries with single estimates
single_estimates <- un[, which(length(iso_code)==1), by='country'][, country]
#choose max hhsize (not much heterogeneity)
fixed_dups <- un[country %in% multiple_estimates, list(hh_size=max(hh_size, na.rm=T), source=paste(source, collapse="/")), by='country,iso_code,date' ]
hh_size_estimates <- rbind(un[country %in% single_estimates], fixed_dups)
hh_size_estimates[, hh_size:=round(hh_size, 2)]
setnames(hh_size_estimates, 'country', 'location')
saveRDS(hh_size_estimates, 'data/unpd_hhsize_2018.rds')
