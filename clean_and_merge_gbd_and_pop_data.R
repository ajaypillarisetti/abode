# clean_and_merge_gbd_and_pop_data
setwd('~/Documents/HAPIT4')

library(data.table)

#BoD Data
gbd1 <- fread('raw_data/IHME-GBD_2017_DATA-5c62ae74-1.csv')
gbd2 <- fread('raw_data/IHME-GBD_2017_DATA-5c62ae74-2.csv')
gbd <- rbind(gbd1, gbd2)
gbd <- gbd[metric_name=='Number' & sex_name!='Both']

#drop unnecessary columns
gbd[, measure_id := NULL]
gbd[, location_id := NULL]
gbd[, sex_id := NULL]
gbd[, age_id := NULL]
gbd[, cause_id := NULL]
gbd[, metric_id := NULL]
setnames(gbd, c('val', 'upper', 'lower'), c('gbd_val', 'gbd_upper', 'gbd_lower'))

#Pop Data
pop <- fread('raw_data/IHME_GBD_2017_POP_2015_2017_Y2018M11D08.CSV')
pop2017 <- pop[year_id==2017 & sex_name != 'Both']
#match colnames to other IHME datasets
setnames(pop2017, 'age_group_name', 'age_name')
country_total_pop <- pop2017[age_name == "All Ages", list(pop_total = sum(val)), by = c('location_name', 'age_name', 'year_id')]

#drop unnecessary columns
pop2017[, measure_id := NULL]
pop2017[, measure_name := NULL]
pop2017[, metric_name := NULL]
pop2017[, location_id := NULL]
pop2017[, sex_id := NULL]
pop2017[, age_group_id := NULL]
setnames(pop2017, c('val', 'upper', 'lower'), c('pop_val', 'pop_upper', 'pop_lower'))
pop2017[, pop_upper := NULL]
pop2017[, pop_lower := NULL]

pop_gbd <- merge(gbd, pop2017, by = c('location_name', 'age_name','sex_name'))
pop_gbd <- merge(pop_gbd, country_total_pop[, c('location_name', 'pop_total'), with = F], by='location_name')
pop_gbd[, pop_frac := round(pop_val / pop_total, 4)]

# pop_gbd[measure_name %like% "DALYs", measure_name := 'DALYs']
# pop_gbd[cause_name %like% "Lower respiratory", cause_name := 'LRI']
# pop_gbd[cause_name %like% "Ischemic heart", cause_name := 'IHD']
# pop_gbd[cause_name %like% "Chronic obstructive", cause_name := 'COPD']
# pop_gbd[cause_name %like% "Tracheal", cause_name := 'Lung Cancer']
# pop_gbd[cause_name %like% "Diabetes", cause_name := 'T2DM']

pop_gbd[, metric_name := NULL]
pop_gbd[, year := NULL]
pop_gbd[, year_id := NULL]

pop_gbd[, age_name:=factor(age_name, ordered = TRUE, levels = c('All Ages', 'Under 5', '<1 year', '1 to 4', '5 to 9', '10 to 14', '15 to 19', '20 to 24', "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54","55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",  "85 to 89", "90 to 94", "95 plus"))]

saveRDS(pop_gbd, file='data/gbd_pop_data_2017.rds')
