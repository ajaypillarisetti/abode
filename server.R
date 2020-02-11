###Ajay Pillarisetti, 2013 - 2019
###HAPIT "Chupacabra"

Sys.setenv(TZ="GMT")
defaultWD <- getwd()

shinyServer(function(input, output, session) {

	#########################
	######### HAPIT ######### 
	#########################
	setwd(defaultWD)

	#crumb for making sure inputs are loaded prior to trying to grab outputs
	#allows interactive warnings
	crumb <- reactiveValues()
	crumb$proceed <- 0

	#core detection for multicore processing
	numCores <- round(detectCores()*0.90,0)
	registerDoMC(numCores)

	#set counterfactiual PM (integer)
	cfPM <- 3


	######### IERS  #########
	rr.lookup <- readRDS('data/iers_2017.rds')
	setnames(rr.lookup, 'mean', 'meanRR')


	######## INPUTS ######### 
	#capture inputs
	country 			<- reactive({input$countrySelect})

	#exposure inputs
	femalePre  			<-  reactive({as.numeric(input$femalePre)})
	malePre  			<-  reactive({as.numeric(input$malePre)})
	childPre  			<-  reactive({as.numeric(input$childPre)})
	ambientPre  		<-  reactive({as.numeric(input$ambientPre)})
	femalePost  		<-  reactive({as.numeric(input$femalePost)})
	malePost  			<-  reactive({as.numeric(input$malePost)})
	childPost  			<-  reactive({as.numeric(input$childPost)})
	ambientPost  		<-  reactive({as.numeric(input$ambientPost)})
	femaleToMaleRatio  	<-  reactive({as.numeric(input$femaleToMaleRatio)})
	femaleToChildRatio  <-  reactive({as.numeric(input$femaleToChildRatio)})

	#population-related inputs
	userPeoplePerHH		<- reactive({input$peoplePerHH}) 
	numTarHH 			<- reactive({input$numTarHH}) #totalPopSFU/avgHHSize

	#intervention-related inputs
	useFrac 			<- reactive({input$useFrac/100})
	usefulLife			<- reactive({input$usefulLife})


	######## GBD DATA ######### 
	gbd_data <- reactive({
		country_gbd_pop <- gbd_pop_data_2017[location_name == country()]
		country_gbd_pop[age_name %in% c('1 to 4', '<1 year', 'Under 5'), age_group := 'Child']
		country_gbd_pop[is.na(age_group), age_group := "Adult"]
		unique(country_gbd_pop)
	})		
   
 	age_dist <- reactive({
 		age_dist <- dcast.data.table(unique(gbd_pop_data_2017[location_name == country(), pop_val, by ='age_name,sex_name']), age_name~sex_name, value.var = 'pop_val')
 		setnames(age_dist, 'age_name', "Age Group")
 		age_dist[, Female := round(Female, 0)]
 		age_dist[, Male := round(Male, 0)]
 		age_dist[, Total := round(Male + Female, 0)]
 		age_dist <- as.data.table(arrange(age_dist, `Age Group`))
 		age_dist 		
 	})

	#country background socioeconomic and demographic data
	population 			<- reactive({gbd_pop_data_2017[location_name==country(), unique(pop_total)]})
	u5 					<- reactive({gbd_pop_data_2017[location_name==country() & age_name =="Under 5", sum(unique(pop_val))]})
	fracSFU 			<- reactive({sfu_2017[location_name==country(), round(mean, 2)]})
	numSFUHH 			<- reactive({crumb$proceed <- 1; (population()/avgHHSize()) * fracSFU()})

	#calculated, does not req if-else
	totalPopSFU 		<- reactive({population()*fracSFU()})
	
	#calculated, does not req if-else
	totalPopu5SFU 		<- reactive({u5()*fracSFU()})
	avgHHSize 			<- reactive({if(length(unpd_hhsize_2018[location==country(), hh_size]) == 0 | is.na(unpd_hhsize_2018[location==country(), hh_size])){4}else{unpd_hhsize_2018[location==country(), hh_size]}})

	#air pollution parameters from databases
	hap.exposure		<- reactive({hap_exp_conc_2017[location_name==country(), hap_val]})
	aap.exposure 		<- reactive({aap_conc_2017[location_name==country(), aap_val]})
	hap.ratios 			<- hap_exp_ratios_2017

	#combined exposure objects
	exposure.object <- reactive({
		rbind(
		  data.table(type = 'amb.all.rr', conc = round(ambientPre(),0), period = "pre"),
		  data.table(type = 'hap.female.rr', conc = round(femalePre(),0), period = "pre"),
		  data.table(type = 'hap.male.rr', conc = round(malePre(),0), period = "pre"),
		  data.table(type = 'hap.child.rr', conc = round(childPre(),0), period = "pre"),
		  data.table(type = 'tot.female.rr', conc = round(ambientPre() + femalePre(), 0), period = "pre"),
		  data.table(type = 'tot.male.rr', conc = round(ambientPre() + malePre(),0), period = "pre"),
		  data.table(type = 'tot.child.rr', conc = round(ambientPre() + childPre(),0), period = "pre"),

		  data.table(type = 'amb.all.rr', conc = round(ambientPost(),0), period = "post"),
		  data.table(type = 'hap.female.rr', conc = round(femalePost(),0), period = "post"),
		  data.table(type = 'hap.male.rr', conc = round(malePost(),0), period = "post"),
		  data.table(type = 'hap.child.rr', conc = round(childPost(),0), period = "post"),
		  data.table(type = 'tot.female.rr', conc = round(ambientPost() + femalePost(),0), period = "post"),
		  data.table(type = 'tot.male.rr', conc = round(ambientPost() + malePost(),0), period = "post"),
		  data.table(type = 'tot.child.rr', conc = round(ambientPost() + childPost(),0), period = "post")
		)
	})

	pop_scaler <- reactive({
		int_pop <- input$numTarHH * avgHHSize()
		pop_scaler <- gbd_data()[measure_name == 'Deaths' & cause_name == "COPD"][, c('location_name', 'age_name', 'sex_name', 'pop_val', 'pop_total', 'pop_frac')]
		pop_scaler[, pop_in_cat:=round(pop_frac*int_pop,0)]
		pop_scaler[, gbd_scaler:=pop_in_cat/pop_val]
		pop_scaler
	})	

	#Prep relative risk tables and perform PAF math
	rrs <- reactive({
		rrs <- merge(exposure.object(), rr.lookup, by = 'conc')
		rrs[, risk:=sapply(strsplit(type, '.', fixed = TRUE), '[[', 1)]
		rrs[, group:=sapply(strsplit(type, '.', fixed = TRUE), '[[', 2)]
		rrs[, type:=NULL]

		amb_risks <- rrs[risk == 'amb', c('period','age_name', 'meanRR','conc', 'group', 'cause_name')]
		setnames(amb_risks, 'meanRR', 'amb.rr')
		setnames(amb_risks, 'conc', 'amb.conc')

		rrs <- merge(rrs[risk == 'tot'], amb_risks[, -c('group')], by=c('period','age_name', 'cause_name'))

		rrs[, rr := ((1 - useFrac()) * amb.rr) + (useFrac() * meanRR)]
		rrs[, paf := (rr - 1)/rr]

		rrs[group == 'male', group := "Male"]
		rrs[group == 'female', group := "Female"]
		rrs[group == 'child', group := "Child"]

		gbd_data <- copy(gbd_data())
		gbd_data[age_name %in% c("1", "<1 year", "Under 5"), group := "Child"]
		gbd_data[is.na(group), group := sex_name]

		stroke_ihd_rrs <- rbind(merge(gbd_data[cause_name %in% c('Stroke', 'IHD')], rrs[period=='pre'], by = c('age_name', 'cause_name', 'group'), all.x = T), merge(gbd_data[cause_name %in% c('Stroke', 'IHD')], rrs[period=='post'], by = c('age_name', 'cause_name', 'group')))
		other_outcomes_rrs <- rbind(merge(gbd_data[!cause_name %in% c('Stroke', 'IHD')], rrs[period=='pre', -c('age_name')], by = c('cause_name', 'group'), all.x = T), merge(gbd_data[!cause_name %in% c('Stroke', 'IHD')], rrs[period=='post', -c('age_name')], by = c('cause_name', 'group')))
		gbd_data_rrs <- rbind(stroke_ihd_rrs, other_outcomes_rrs)
		gbd_data_rrs <- gbd_data_rrs[!is.na(amb.rr)]

		gbd_data_rrs <- merge(gbd_data_rrs, pop_scaler()[,-c('pop_val','pop_total','pop_frac','pop_in_cat')], by = c('location_name', 'age_name', 'sex_name'))
		setnames(gbd_data_rrs, 'conc', 'total.conc')
		gbd_data_rrs[, hap.conc := total.conc - amb.conc]
		gbd_data_rrs
	})

	all_outputs <- reactive({
		outputs <- rrs()[, list(
			tot.burden = gbd_val * paf * gbd_scaler,
			tot.burden.lower = gbd_lower * paf * gbd_scaler,
			tot.burden.upper = gbd_upper * paf * gbd_scaler
		), by='location_name,group,cause_name,sex_name,age_name,measure_name,period']

		outputs[cause_name != "LRI" & age_name<25 & !age_name %in% c("All Ages", "<5", "<1"), colnames(outputs)[colnames(outputs) %like% "burden"] := 0]
		outputs
	})

	fractions <- reactive({
		for_fractions <- dcast(unique(rrs()[!is.na(total.conc), c('group', 'period', 'total.conc', 'amb.conc', 'hap.conc')]), group ~ period, value.var = c('total.conc', 'amb.conc', 'hap.conc'))
		fractions <- for_fractions[, 
			list(
				dTotal = total.conc_pre - total.conc_post,
				dAmb = amb.conc_pre - amb.conc_post,
				dHap = hap.conc_pre - hap.conc_post
			), by = c('group')
		]
		fractions
	})

	aBurden <- reactive({
		aburden_builder <- function(colname){
 			step1 <- dcast.data.table(all_outputs(), location_name + group + cause_name + measure_name + sex_name + age_name ~ period, value.var = colname)
			step1[, item := colname]
  			step1[, averted := pre-post, by = 'location_name,group,cause_name,measure_name,sex_name,age_name,item']
  			step1
		}	
		columns <- return_cols_like(all_outputs(), 'burden', invert = F)
		aburden <- as.data.table(ldply(columns, aburden_builder))
		aburden[, averted := round(averted, 0)]

		aburden_wide <- dcast.data.table(aburden, location_name+group+cause_name+measure_name+sex_name+age_name~item, value.var = 'averted')
  		aburden_wide[, measure_name := as.factor(measure_name)]
  		aburden_wide[, cause_name := as.factor(cause_name)]
  		aburden_wide[, sex_name := as.factor(sex_name)]
  		aburden_wide[, age_name := as.factor(age_name)]
  		aburden_wide <- merge(aburden_wide, fractions()[, list(
			amb_fraction= dAmb/dTotal,
			hap_fraction= dHap/dTotal
			), by = 'group'], by = 'group')

		aburden_wide[, amb.burden := round(tot.burden * amb_fraction, 0)]
		aburden_wide[, amb.burden.lower := round(tot.burden.lower * amb_fraction, 0)]
		aburden_wide[, amb.burden.upper := round(tot.burden.upper * amb_fraction, 0)]
		aburden_wide[, hap.burden := round(tot.burden * hap_fraction, 0)]
		aburden_wide[, hap.burden.lower := round(tot.burden.lower * hap_fraction, 0)]
		aburden_wide[, hap.burden.upper := round(tot.burden.upper * hap_fraction, 0)]
  		setnames(aburden_wide, c("Group", 'Country', 'Cause', 'Measure', 'Sex', 'Age Group', 'Total', 'Total (lower)', 'Total (upper)', 'Amb. Burden (fraction)', 'HAP Burden (fraction)', 'Ambient', 'Ambient (lower)', 'Ambient (upper)', 'HAP', 'HAP (lower)', 'HAP (upper)'))
		aburden_wide
	})

	aBurden_overview <- reactive({
		all_ages <- aBurden()[Measure %in% c('Deaths', 'DALYs') & `Age Group` == 'All Ages']
		all_ages[, return_cols_like(all_ages, "fraction") := NULL]
		all_ages[, Group := NULL]
		non_all_ages <- aBurden()[Measure %in% c('Deaths', 'DALYs') & !(`Cause`  %in%  c('COPD', 'LC', 'LRI', 'T2DM'))]
		non_all_ages <- non_all_ages[, list(
			Ambient = sum(Ambient),
			`Ambient (lower)` = sum(`Ambient (lower)`),
			`Ambient (upper)` = sum(`Ambient (upper)`),
			HAP = sum(HAP),
			`HAP (lower)` = sum(`HAP (lower)`),
			`HAP (upper)` = sum(`HAP (upper)`),
			Total = sum(Total),
			`Total (lower)` = sum(`Total (lower)`),
			`Total (upper)` = sum(`Total (upper)`),
			`Age Group` = "All Ages"
		), by='Country,Cause,Measure,Sex']
		rbind(all_ages, non_all_ages)
	})


	##########################
	##### disease tables ##### 
	##### ses tables     ##### 
	##########################
 	output$ses_table <- DT::renderDataTable({
 		data.table(
 			`Population (millions)` = round(population()/1e6, 2),
 			`U5 Population (millions)` = round(u5()/1e6,2),
 			`HH Size` = if(length(avgHHSize()) == 0){NA}else{avgHHSize()},
 			`Dirty Fuel Use (%)` = 100*fracSFU(),
 			`Avg Kitchen PM2.5 Concentration (µg/m3)` = round(hap.exposure(),0),
 			`Avg Amb PM2.5 Exposure (µg/m3)` = round(aap.exposure(),0)
 		)
 	}, extensions = 'Buttons', options = list(dom = 'frtipB', buttons = c('csv', 'excel', 'pdf'), paging = FALSE, searching = FALSE, bInfo = FALSE, sortable = FALSE, ordering = FALSE), rownames = FALSE)

 	output$pop_age_sex <- DT::renderDataTable({
 		age_dist()
 	}, extensions = 'Buttons', options = list(dom = 'frtipB', buttons = c('csv', 'excel', 'pdf'), paging = TRUE, searching = FALSE, bInfo = TRUE, sortable = TRUE, ordering = TRUE), rownames = FALSE)

  	output$daly_table <- DT::renderDataTable({
		dalys <- dcast.data.table(gbd_pop_data_2017[location_name == country() & measure_name %like% 'DALYs' & age_name == "All Ages"], cause_name ~ sex_name, value.var = "gbd_val")
		dalys[, Female := round(Female, 0)]
		dalys[, Male := round(Male, 0)]
		dalys[, Total := Male + Female]
		setnames(dalys, 'cause_name', 'Cause')
  	}, extensions = 'Buttons', options = list(dom = 'frtipB', buttons = c('csv', 'excel', 'pdf'), paging = FALSE, searching = FALSE, bInfo = FALSE, sortable = FALSE, ordering = FALSE), rownames = FALSE)

  	output$death_table <- DT::renderDataTable({
 		deaths <- dcast.data.table(gbd_pop_data_2017[location_name == country() & measure_name %like% 'Deaths' & age_name == "All Ages"], cause_name ~ sex_name, value.var = "gbd_val")
		deaths[, Female := round(Female, 0)]
		deaths[, Male := round(Male, 0)]
		deaths[, Total := Male + Female]
		setnames(deaths, 'cause_name', 'Cause')		  		
  	}, extensions = 'Buttons', options = list(dom = 'frtipB', buttons = c('csv', 'excel', 'pdf'), paging = FALSE, searching = FALSE, bInfo = FALSE, sortable = FALSE, ordering = FALSE), rownames = FALSE)

  	output$yll_table <- DT::renderDataTable({
 		ylls <- dcast.data.table(gbd_pop_data_2017[location_name == country() & measure_name %like% 'YLL' & age_name == "All Ages"], cause_name ~ sex_name, value.var = "gbd_val")
		ylls[, Female := round(Female, 0)]
		ylls[, Male := round(Male, 0)]
		ylls[, Total := Male + Female]
		setnames(ylls, 'cause_name', 'Cause')		  		
  	}, extensions = 'Buttons', options = list(dom = 'frtipB', buttons = c('csv', 'excel', 'pdf'), paging = FALSE, searching = FALSE, bInfo = FALSE, sortable = FALSE, ordering = FALSE), rownames = FALSE)

  	output$yld_table <- DT::renderDataTable({
 		ylds <- dcast.data.table(gbd_pop_data_2017[location_name == country() & measure_name %like% 'YLD' & age_name == "All Ages"], cause_name ~ sex_name, value.var = "gbd_val")
		ylds[, Female := round(Female, 0)]
		ylds[, Male := round(Male, 0)]
		ylds[, Total := Male + Female]
		setnames(ylds, 'cause_name', 'Cause')		  		
  	}, extensions = 'Buttons', options = list(dom = 'frtipB', buttons = c('csv', 'excel', 'pdf'), paging = FALSE, searching = FALSE, bInfo = FALSE, sortable = FALSE, ordering = FALSE), rownames = FALSE)

  	output$exposures_table <- DT::renderDataTable({
  		exposure.object()
  	}, extensions = 'Buttons', options = list(dom = 'frtipB', buttons = c('csv', 'excel', 'pdf'), paging = FALSE, searching = FALSE, bInfo = FALSE, sortable = FALSE, ordering = FALSE), rownames = FALSE)

  	output$atotals_table <- DT::renderDataTable({
  		averted <- aBurden_overview()[Measure %in% c('Deaths', 'DALYs'), -c("Country", "Age Group", return_cols_like(aBurden_overview(), "lower|upper"))]
  		averted
  	}, filter = "top", extensions = 'Buttons', options = list(dom = 'frtipB', buttons = c('csv', 'excel', 'pdf'), paging = FALSE, searching = TRUE, scrollX = TRUE), rownames = FALSE)

  	output$avertedOverview <- DT::renderDataTable({
  		all_ages_long <- melt(aBurden_overview(), id.var = c('Country', 'Cause', 'Measure', 'Sex', 'Age Group'))
  		dcast(all_ages_long[, sum(value), by = c('Country', 'Measure', 'variable')], Country + Measure ~ variable)
  	}, extensions = 'Buttons', options = list(dom = 'frtipB', buttons = c('csv', 'excel', 'pdf'), paging = FALSE, searching = TRUE, scrollX = TRUE), rownames = FALSE)

	##########################
	####### UI OUTPUTS ####### 
	##########################

	####### EXPOSURE ####### 
 	output$femalePre <- renderUI({
	    numericInput('femalePre', NULL, value = round(hap.exposure() * hap.ratios[grouping == 'female', mean_ratio],0), min = 0)
 	})

 	output$malePre <- renderUI({
	    numericInput('malePre', NULL, value = round(input$femalePre * input$femaleToMaleRatio,0), min = 0)
 	})

 	output$childPre <- renderUI({
	    numericInput('childPre', NULL, value = round(input$femalePre * input$femaleToChildRatio,0), min = 0)
 	}) 	

 	output$ambientPre <- renderUI({
	    numericInput('ambientPre', NULL, value = round(aap.exposure(),0), min = 0)
 	}) 	

 	output$femalePost <- renderUI({
	    numericInput('femalePost', NULL, value = round(0.50 * hap.exposure() * hap.ratios[grouping == 'female', mean_ratio],0), min = 0)
 	})

 	output$malePost <- renderUI({
	    numericInput('malePost', NULL, value = round(input$femalePost * input$femaleToMaleRatio,0), min = 0)
 	})

 	output$childPost <- renderUI({
	    numericInput('childPost', NULL, value = round(input$femalePost * input$femaleToChildRatio,0), min = 0)
 	}) 	

 	output$ambientPost <- renderUI({
	    numericInput('ambientPost', NULL, value = round(input$ambientPre,0), min = 0)
 	}) 	

 	output$femaleToMaleRatio <- renderUI({
        sliderInput( "femaleToMaleRatio",  HTML("Female to Male Adult Exposure Ratio<sup><small>3</small></sup>"),  min = 0.4,  max = 1,  value = hap.ratios[grouping == 'male', mean_ratio]/hap.ratios[grouping == 'female', mean_ratio])
    })

 	output$femaleToChildRatio <- renderUI({
       sliderInput("femaleToChildRatio", HTML("Female to Child (< 5) Exposure Ratio<sup><small>4</small></sup>"), min = 0.4, max = 1, value = hap.ratios[grouping == 'child', mean_ratio]/hap.ratios[grouping == 'female', mean_ratio])
 	})

	####### INTERVENTION ####### 
	output$interventionDetails <- renderUI({
		numericInput("numTarHH", HTML("Number of Targeted HH<sup><small>6</small></sup>"), min = 0, max = if(numSFUHH()<=1e6){round_any(numSFUHH(),25000,ceiling)}else{numSFUHH()}, value = 25000,step = 25000)
	})

	output$peoplePerHH <- renderUI({
		sliderInput("peoplePerHH", HTML("People Per HH<sup><small>7</small></sup>"), min = 1, max = 10, value = round(avgHHSize(),1),step = 1)
	})

	output$childPerHH <- renderUI({
		sliderInput("childPerHH", HTML("Kids <5 Per HH<sup><small>8</small></sup>"), min = 0, max = userPeoplePerHH(), value = u5()/(population()/avgHHSize()),step = 0.5)
	})

	output$adultPerHH <- renderUI({
		sliderInput("adultPerHH", HTML("Adults Per HH<sup><small>9</small></sup>"), min = 0, max = userPeoplePerHH(), value = userPeoplePerHH() - input$childPerHH,step = 0.5)
	})

	####### TABLE TITLES ####### 
	output$sesTableTitle <- renderText({paste(gbd_data_year, country(), "Background SES & Demographic Statistics", sep=" ")})
	output$burdenTableTitle <- renderText({paste(gbd_data_year, country(), "Annual Disease Data", sep=" ")})

	####### Bar Chart Output Filters ####### 
	output$causeBarChartPlotFilter <- renderUI({
		selectizeInput("causeBarChartPlotFilter", NULL, choices = aBurden()[, unique(Cause)], multiple = TRUE, selected = c("COPD", "IHD", "LC", "LRI", "Stroke", "T2DM"))
	})

	output$sexBarChartPlotFilter <- renderUI({
		selectizeInput("sexBarChartPlotFilter", NULL, choices = aBurden()[, c(unique(as.character(Sex)), "Both")], multiple = FALSE, selected = c("Male", "Female"))
	})

	output$measureBarChartPlotFilter <- renderUI({
		selectizeInput("measureBarChartPlotFilter", NULL, choices = aBurden()[!Measure %in% c('YLLs (Years of Life Lost)', 'YLDs (Years Lived with Disability)'), unique(Measure)], multiple = FALSE, selected = "DALYs")
	})

	output$riskBarChartPlotFilter <- renderUI({
		selectizeInput("riskBarChartPlotFilter", NULL, choices = c('Ambient', 'HAP', 'Total'), multiple = FALSE, selected = c("HAP"))
	})

	output$ageBarChartPlotFilter <- renderUI({
		selectizeInput("ageBarChartPlotFilter", NULL, choices = aBurden()[, unique(`Age Group`)], multiple = FALSE, selected = 'All Ages')
	})

	####### Age-specific Output Filters ####### 
	output$causeAgeChartPlotFilter <- renderUI({
		selectizeInput("causeAgeChartPlotFilter", NULL, choices = aBurden()[, unique(Cause)], multiple = FALSE, selected = c("LRI"))
	})

	output$sexAgeChartPlotFilter <- renderUI({
		selectizeInput("sexAgeChartPlotFilter", NULL, choices = aBurden()[, c(unique(as.character(Sex)), "Both")], multiple = FALSE, selected = c("Male", "Female"))
	})

	output$measureAgeChartPlotFilter <- renderUI({
		selectizeInput("measureAgeChartPlotFilter", NULL, choices = aBurden()[!Measure %in% c('YLLs (Years of Life Lost)', 'YLDs (Years Lived with Disability)'), unique(Measure)], multiple = FALSE, selected = "DALYs")
	})

	output$riskAgeChartPlotFilter <- renderUI({
		selectizeInput("riskAgeChartPlotFilter", NULL, choices = c('Ambient', 'HAP', 'Total'), multiple = FALSE, selected = c("HAP"))
	})

	output$ageAgeChartPlotFilter <- renderUI({
		selectizeInput("ageAgeChartPlotFilter", NULL, choices = aBurden()[, unique(`Age Group`)], multiple = FALSE, selected = 'All Ages')
	})


 	##########################
	######## GRAPHICS ######## 
	##########################
	output$population_pyramid <- renderPlotly({
		pop <- copy(age_dist())[, `Country Total`:=round(population(),0)]
		pop <- melt(pop, id.var=c('Age Group', 'Country Total'), measure.var = c("Female", "Male"))
		pop[, Percent := round(((100*value)/`Country Total`),2)]
		pop[variable == 'Male', Percent:=-Percent]

		p <- ggplot(data = pop[!`Age Group` %in% c('All Ages', "Under 5")], aes(x = `Age Group`, y = Percent, fill = variable))

		p2 <- p + geom_bar(stat='identity', width=1, alpha=0.5) + 
			coord_flip() +     
			labs(x = "Age", y = "Percent of Population", fill = "Group") +
		    theme_bw(12) +    
		    scale_y_continuous(labels = abs) +
		    scale_fill_manual(values = bly) +
		    theme(legend.position = "bottom",
		          axis.title.x = element_text(vjust = -2),
		    	  axis.title.y = element_text(vjust = 2),
		          legend.title=element_blank()
			)
		p2
	})

	selected_data_for_barchart <- reactive({
		melted_data <- melt(aBurden_overview(), id.var = c('Cause', 'Measure', 'Sex', 'Age Group', 'Country'))
		if(input$riskBarChartPlotFilter == 'Total'){
			riskBarChartPlotFilter <- c('HAP', 'Ambient')
		}else{
			riskBarChartPlotFilter <- input$riskBarChartPlotFilter
		}
		if(input$sexBarChartPlotFilter == "Both"){
			melted_data <- melted_data[(!variable %like% "lower|upper") & (Measure %in% input$measureBarChartPlotFilter) & (Cause %in% input$causeBarChartPlotFilter) & (variable %in% riskBarChartPlotFilter) , list(value = sum(value)), by = c('Cause', 'Measure', 'Age Group', 'Country', 'variable')]
		}else{
			melted_data <- melted_data[!variable %like% "lower|upper" & (Measure %in% input$measureBarChartPlotFilter) & (Cause %in% input$causeBarChartPlotFilter) & (Sex %in% input$sexBarChartPlotFilter) & (variable %in% riskBarChartPlotFilter)]
		}
		melted_data
	})

	output$barchart <- renderHighchart({
		dd <- dcast.data.table(selected_data_for_barchart(), Cause ~ variable, value.var = 'value')
		highchart() %>%
			hc_chart(type = "column") %>%
			hc_plotOptions(column = list(stacking = "normal", animation = FALSE, groupPadding = 0)) %>%
			hc_xAxis(categories = as.character(dd$Cause)) %>%
			hc_add_series(name = "Ambient Burden",
		        data = dd$Ambient,
		        stack = "Averted Burden") %>%
			hc_add_series(name = "HAP Burden",
				data = dd$HAP,
				stack = "Averted Burden") %>%
			hc_add_theme(hc_theme_smpl()) %>%   
			hc_exporting(enabled = TRUE) %>%
			hc_xAxis(title = list(text = "Cause")) %>% 
			hc_yAxis(title = list(text = "Number Averted"))
	})

	output$impacts_by_age <- renderHighchart({
		print(str(aBurden()))
		if(input$sexAgeChartPlotFilter == "Both"){
			all_ages_long <- melt(aBurden(), id.var = c('Country', 'Group','Cause', 'Measure', 'Sex', 'Age Group'))
			dd <- dcast(all_ages_long[, sum(value), by = c('Country', 'Cause', 'Measure', 'Age Group', 'variable')], Country + Cause + Measure + `Age Group` ~ variable)
			dd <- dd[Cause == input$causeAgeChartPlotFilter & Measure == input$measureAgeChartPlotFilter]

		}else{
			dd <- aBurden()[Cause == input$causeAgeChartPlotFilter & Measure == input$measureAgeChartPlotFilter & Sex == input$sexAgeChartPlotFilter]
		}
		setnames(dd, "Age Group", 'age_group')
		if(input$riskAgeChartPlotFilter == "Total"){
			setnames(dd, "Total", "risk")
			setnames(dd, "Total (lower)", 'risk_lower')
			setnames(dd, "Total (upper)", 'risk_upper')
		}
		if(input$riskAgeChartPlotFilter == "Ambient"){
			setnames(dd, "Ambient", "risk")
			setnames(dd, "Ambient (lower)", 'risk_lower')
			setnames(dd, "Ambient (upper)", 'risk_upper')
		}
		if(input$riskAgeChartPlotFilter == "HAP"){
			setnames(dd, "HAP", "risk")
			setnames(dd, "HAP (lower)", 'risk_lower')
			setnames(dd, "HAP (upper)", 'risk_upper')
		}	

		dd[, age_group := as.character(age_group)]
		dd[, age_group:=factor(age_group, ordered = TRUE, levels = c('All Ages', 'Under 5', '<1 year', '1 to 4', '5 to 9', '10 to 14', '15 to 19', '20 to 24', "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54","55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",  "85 to 89", "90 to 94", "95 plus"))]
		dd[Cause == 'COPD', color_of_causes := "#3A405A"]
		dd[Cause == 'T2DM', color_of_causes := "#F9DEC9"]
		dd[Cause == 'LRI', color_of_causes := "#99B2DD"]
		dd[Cause == 'IHD', color_of_causes := "#E9AFA3"]
		dd[Cause == 'LC', color_of_causes := "#685044"]
		dd[Cause == 'Stroke', color_of_causes := "#439A86"]

		pp <- 
			hchart(
				dd, 
				"errorbar", 
				hcaes(
					x = age_group, 
					low = risk_lower, 
					high = risk_upper,
					color = color_of_causes,
					group = age_group
				), 
				whiskerLength = 1,
				stemWidth = 1, name = "") %>% 
			hc_add_series(
				dd, 
				"scatter", 
				hcaes(
					x = age_group, 
					y = risk,
					color = color_of_causes,
					group = age_group
				), 
				marker = list(radius = 6), 
				name = "") %>% 
			hc_tooltip(
				pointFormat = "{point.Cause} - {point.age_group} <br> {point.Total} <br> <small>range: {point.total_lower} to {point.total_upper}</small>",
				shared = TRUE
				) %>% 
			hc_add_theme(hc_theme_smpl()) %>%   
			hc_exporting(enabled = TRUE) %>%   
			# hc_title(text = "Something") %>%
			hc_xAxis(title = list(text = "Age Group")) %>% 
			hc_yAxis(title = list(text = "Number Averted")) %>%
			hc_plotOptions(grouping = TRUE) %>%
			hc_legend(enabled = FALSE)
		pp
	})


	##########################
	##### INPUT OBSERVER #####
	##########################
	observe({
		if(crumb$proceed<=0 & input$tabs %in% c("health", "costs", "downloads")){
			warning_string <- "Please load the 'Inputs' tab before trying to view HAPIT output."
			js_string <- 'alert("SOMETHING");'
			js_string <- sub("SOMETHING",warning_string,js_string)
			session$sendCustomMessage(type='jsCode', list(value = js_string))
			updateTabItems(session, "tabs", 'inputs')
		}
		if(crumb$proceed<=0 & input$tabs %in% c("inputs")){
			session$sendCustomMessage(type='jsCode', list(value = "$('#runScenario').click()"))
		}

		# if( (femalePost() > femalePre()) |  (malePost() > malePre()) |  (childPost() > childPre()) ){
		# 	warning_string <- "Post-intervention exposure greater than pre-intervention exposure."
		# 	js_string <- 'alert("SOMETHING");'
		# 	js_string <- sub("SOMETHING",warning_string,js_string)
		# 	session$sendCustomMessage(type='jsCode', list(value = js_string))
		# 	# if(adultPrePM()-50<cfPM){
		# 	# 	updateNumericInput(session, "adultPrePM", value=285)
		# 	# 	updateNumericInput(session, "adultPostPM", value=140)}else{
		# 	# 	updateNumericInput(session, "adultPostPM", value=adultPrePM()-50)}
	 # 	   }
	 	   
		# if(femalePost() < cfPM | malePost() < cfPM | childPost() < cfPM){
		# 	warning_string <- "Exposures cannot be less than the counterfactual exposure of 3 ug/m3."
		# 	js_string <- 'alert("SOMETHING");'
		# 	js_string <- sub("SOMETHING",warning_string,js_string)
		# 	session$sendCustomMessage(type='jsCode', list(value = js_string))
		# 	updateNumericInput(session, "femalePost", value=cfPM)
		# 	updateNumericInput(session, "malePost", value=cfPM)
		# 	updateNumericInput(session, "childPost", value=cfPM)
	 #    }
	  })
})