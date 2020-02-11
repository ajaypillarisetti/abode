###Ajay Pillarisetti, 2013 - 2019
###HAPIT "Chupacabra"

hapitVersionNumber <- '4.0.0'
gbd_data_year <- "2017"
soga_data_year <- "2019"
name <- "ABODE"
versionNumber <- '1.0.0'

library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(lubridate)
library(shinydashboard)
library(scales)
library(devtools)
library(xtable)
library(doMC)
library(DT)
library(rmarkdown)
library(tufte)
library(EnvStats)
library(tools)
library(plotly)
library(highcharter)
library(data.table)

options(xtable.comment = FALSE)
options(xtable.booktabs = TRUE)

logMoments <- function(mean, sd){
	v <- sd*sd
	phi <- sqrt(v+(mean^2))
	mu <- log((mean^2)/phi)
	sigma <- sqrt(log((phi^2)/(mean^2)))
	list(mu=mu, sigma=sigma)
}

import_data <- function(file){
	name <- basename(file_path_sans_ext(file))
	assign(name, readRDS(file), envir = .GlobalEnv)
}

l_ply(list.files('data', full.names = TRUE, pattern = '.rds'), import_data)

#clean up GBD data
gbd_pop_data_2017[measure_name %like% "DALYs", measure_name := 'DALYs']
gbd_pop_data_2017[cause_name %like% "Lower respiratory", cause_name := 'LRI']
gbd_pop_data_2017[cause_name %like% "Ischemic heart", cause_name := 'IHD']
gbd_pop_data_2017[cause_name %like% "Chronic obstructive", cause_name := 'COPD']
gbd_pop_data_2017[cause_name %like% "Tracheal", cause_name := 'LC']
gbd_pop_data_2017[cause_name %like% "Diabetes", cause_name := 'T2DM']

return_cols_like <- function(dataframe, like, invert = FALSE){
  if(invert){colnames(dataframe)[!colnames(dataframe) %like% like]}else{
  colnames(dataframe)[colnames(dataframe) %like% like]}
}

countryListRef <- sort(gbd_pop_data_2017[, unique(location_name)])
numberOfCountries <- length(unique(countryListRef))

bly <- c("#E69F00", "#0072B2", "#000000", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#CC79A7")

Sys.setenv("plotly_username"="ajay")
Sys.setenv("plotly_api_key"="y4sztiASwGnECUli72tu")