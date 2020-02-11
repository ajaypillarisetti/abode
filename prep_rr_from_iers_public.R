library(parallel)
library(doMC)
registerDoMC(8)
library(tools)
library(data.table)

############################################
###  CREATE AGE + CAUSE SPECIFIC TABLES  ###
############################################
computeRR <- function(conc, file){
	dfr <- fread(file)
	risk_name <- gsub("params_", "", basename(file_path_sans_ext(file)))
	dfr[, list(
		RR = 1+alpha*(1-exp(-beta*((conc-tmrel)^gamma)))), by=V1
	][, list(
		conc = conc, 
		lower = quantile(RR, 0.25, na.rm=T), 
		mean = mean(RR, na.rm=T), 
		upper = quantile(RR, 0.975, na.rm=T),
		risk = risk_name)
	 ]
}

wrapper_RR <- function(file){
	risk_output <- as.data.table(ldply(seq(1,1000, by = 0.1), computeRR, file, .parallel = TRUE))
	saveRDS(risk_output, paste('data/iers/', file_path_sans_ext(basename(file)), '.01.rds', sep=""))
	return(NULL)
}

#each file contains 1000 runs of the models at different TMRELs
#for each of these thousand runs, we need to get estimates for concentrations from 1:1000
param_files <- list.files('raw_data/iers', full.names = T, pattern='.csv')

#run for all files in folder
mclapply(param_files, wrapper_RR, mc.cores=12, mc.silent = TRUE)

## TO DO
## create tests using summary tables from IHME

all_iers.1 <- list.files('data/iers', pattern = '.01.rds', full.names = T)
ihd <- rbindlist(lapply(grep("ihd", all_iers.1, value = T), readRDS))
stroke <- rbindlist(lapply(grep("stroke", all_iers.1, value = T), readRDS))

ihd[, age:=sapply(strsplit(risk, "_"), "[[", 3)]
ihd[, risk:=sapply(strsplit(risk, "_"), "[[", 2)]
saveRDS(ihd, 'data/iers/ihd.01_99.rds')

stroke[, age:=sapply(strsplit(risk, "_"), "[[", 3)]
stroke[, risk:=sapply(strsplit(risk, "_"), "[[", 2)]
saveRDS(stroke, 'data/iers/stroke.01_99.rds')

iers <- list.files('data/iers', pattern = '.01.', full.names = T)
iers <- rbindlist(lapply(iers, readRDS), fill=TRUE)
iers[is.na(age), unique(risk)]
setnames(iers, 'risk', 'cause')
iers[cause %like% "t2_dm", cause:='T2DM']
iers[cause %like% "ihd", cause:='IHD']
iers[cause %like% "lri", cause:='LRI']
iers[cause %like% "neo_lung", cause:='LC']
iers[cause %like% "resp_copd", cause:='COPD']
iers[cause %like% "stroke", cause:='Stroke']
iers[is.na(age), age:='All Ages']
iers <- iers[!is.na(mean)]
iers[age != "All Ages", age_end := as.numeric(age) + 4]
iers[age != "All Ages", age_name := paste(age, age_end, sep=' to ')]
iers[age == "All Ages", age_name := 'All Ages']
iers <- iers[, -c('age', 'age_end')]
setnames(iers, 'cause', 'cause_name')

iers[, age_name:=factor(age_name, ordered = TRUE, levels = c('All Ages', "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54","55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",  "85 to 89", "90 to 94", "95 to 99"))]

saveRDS(iers, 'data/iers_2017.rds')