# clean_and_merge_gbd_and_pop_data
setwd('~/Documents/HAPIT4')

library(data.table)

#read in HAP data
hap <- fread('~/Documents/HAPIT4/raw_data/hap_pm_map.csv')
hap <- hap[year_id == 2017, c('location_name', 'year_id', 'predict_pm'), with = F]
setnames(hap, c('location_name', 'year_id', 'hap_val'))
saveRDS(hap, file='data/hap_exp_conc_2017.rds')

aap <- fread('~/Documents/HAPIT4/raw_data/oap_summary_all_locations.csv')
aap <- aap[year_id == 2017, c('location_name', 'year_id', 'mean_value')]
setnames(aap, c('location_name', 'year_id', 'aap_val'))
saveRDS(aap, file='data/aap_conc_2017.rds')

hap_ratios <- fread('~/Documents/HAPIT4/raw_data/hap_pm_ratios.csv')
hap_ratios <- melt(hap_ratios, id.var = c('grouping', 'mean_pm2.5'))
hap_ratios <- hap_ratios[variable %like% 'draw', list(mean_ratio = mean(value)), by = 'grouping']
saveRDS(hap_ratios, file='data/hap_exp_ratios_2017.rds')

sfu_frac <- fread('~/Documents/HAPIT4/raw_data/hap_prop_exposure.csv')
sfu_frac <- sfu_frac[year_id==2017, c('location_name', 'mean', 'lower', 'upper', 'population')]
saveRDS(sfu_frac, file='data/sfu_2017.rds')
