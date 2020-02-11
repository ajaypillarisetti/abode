###Ajay Pillarisetti, 2013 - 2019
###ABODE 1.0
###HAPIT "Chupacabra"

dashboardPage(
  dashboardHeader(title = paste(name, versionNumber)),
  dashboardSidebar(
   		sidebarMenu(id = "tabs", selectInput(inputId = "countrySelect", label = "Select a Country", choices = countryListRef, selected = 'Nepal', selectize = T),
	    menuItem("Overview", tabName = "overview",  icon = icon("info-circle")),
	    menuItem("Inputs", tabName = "inputs",  icon = icon("edit")),
    	menuItem("Health Impacts", tabName = "health",  icon = icon("chart-bar")),
        menuItem("Documentation", tabName = "docs",  icon = icon("book"))#,
    )
  ),
	dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(type='text/javascript', src='scripties.js'),
            tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'
                ))
            ),

    	tabItems(
			tabItem("overview",
                fluidRow(
                    box(width=12, collapsible = TRUE, status="info", solidHeader=FALSE, title=paste("Welcome to ABODE, the Air Pollution Burden of Disease Explorer"),
                        p("ABODE estimates health changes due to interventions designed to lower exposures to household air pollution (HAP) of household members currently using unclean fuels (wood, dung, coal, kerosene, and others). These interventions could be due to cleaner burning stoves, cleaner fuels, providing chimneys or other ventilation changes, movement of the traditional hearth to a different location, motivating changes in behavior, or a combination of the above. ABODE can also estimate changes in health due to changes in ambient air pollution from household interventions that may not be measured in normal household exposure measurements. With some care in entering input parameters, it can be used for evaluating other interventions to reduce HAP, including those for lighting and spaceheating."),
                        p("ABODE can also estimate health changes due to altered ambient air pollution resulting from transitions to clean fuels. Currently, this estimation only applies to the households in which your program intervenes; that is, it does not apply to the country as a whole."),
                        HTML("<p>ABODE uses background disease rates and relationships between exposure to PM<sub>2.5</sub> and health outcomes described as part of the Institute for Health Metrics and Evaluation's", gbd_data_year,"Global Burden of Disease and Comparative Risk Assessment efforts. It uses country-level estimates of ambient air pollution from IHME and HEI's",soga_data_year," State of Global Air report. </p>"), 
                        HTML(paste("<p><em>Meaningful use of ABODE requires field work at the intervention dissemination site to demonstrate pollution exposures before and after the intervention in a representative sample of households.</em> As each country's health and HAP situation is different, ABODE currently contains the background data necessary to conduct analysis in <strong>", numberOfCountries, " countries. </strong></p>", sep=""))
                    )
                ),
                fluidRow(
                    tabBox(
                        width = 12,
                        title = textOutput("sesTableTitle"),
                        tabPanel("Overview", DT::dataTableOutput('ses_table')),
                        tabPanel("Pop. by Age & Sex", DT::dataTableOutput('pop_age_sex')),
                        tabPanel("Population Pyramid", plotlyOutput("population_pyramid"))
                    )
                ),
                fluidRow(
                    tabBox(
                        width = 12,
                        title = textOutput("burdenTableTitle"),
                        tabPanel("Deaths", DT::dataTableOutput('death_table')),
                        tabPanel("DALYs", DT::dataTableOutput('daly_table')),
                        tabPanel("YLLs", DT::dataTableOutput('yll_table')),
                        tabPanel("YLDs", DT::dataTableOutput('yld_table'))
                    )
                )
			),      
    		tabItem("inputs",
                fluidRow(
                    box(title = "Exposure-related Inputs",
                        width=7,
                        status="primary", solidHeader = TRUE,
                        HTML(""),
                        HTML("<p>A number of user-defined inputs are required. By default, ABODE includes exposure values estimated by IHME at the country level for women, men, and children, as well as an estimate of ambient air pollution concentrations. <em> When you update the 'Females' value, the 'Males' and 'Children' values are automatically adjusted by a ratio of female-to-male or female-to-child exposures derived from the literature.</em> These ratios and adjustments can be overridden by manual input of context-specific values.</p> "),
                        hr(),                        
                        HTML('<p><strong>Pre-Intervention Exposures to PM<sub>2.5</sub><sup><small>1</small></sup></strong></p>'),
                        column(3,
                            HTML('<p>Females<p>'),
                            uiOutput("femalePre")
                        ),
                        column(3,
                            HTML('<p>Males<p>'),
                            uiOutput("malePre")
                        ),
                        column(3,
                            HTML('<p>Children<p>'),
                            uiOutput("childPre")
                        ),
                        column(3,
                            HTML('<p>Ambient<p>'),
                            uiOutput("ambientPre")
                        ),
                        HTML("<hr style='clear:both;'/>"),
                        HTML('<p><strong>Post-Intervention Exposures to PM<sub>2.5</sub><sup><small>2</small></sup></strong></p>'),
                        column(3,
                            HTML('<p>Females<p>'),
                            uiOutput("femalePost")
                        ),
                        column(3,
                            HTML('<p>Males<p>'),
                            uiOutput("malePost")
                        ),
                        column(3,
                            HTML('<p>Children<p>'),
                            uiOutput("childPost")
                        ),
                        column(3,
                            HTML('<p>Ambient<p>'),
                            uiOutput("ambientPost")
                        ),
                        HTML("<hr style='clear:both;'/>"),
                        column(6,
                            uiOutput('femaleToMaleRatio')
                        ),
                        column(6,
                            uiOutput('femaleToChildRatio')
                        )
                    ),
                box(title = 'Population-related Inputs',
                    width=5, status = 'primary',
                    solidHeader = TRUE,
                        uiOutput('interventionDetails'),
                        uiOutput('peoplePerHH'),
                        fluidRow(
                            column(width=6,
                                uiOutput('childPerHH')
                            ),
                            column(width=6,
                                uiOutput('adultPerHH')
                            )
                        )
                    ),
                box(title = 'Intervention-related Inputs',
                    width=5,
                    status='primary', solidHeader = TRUE,
                        sliderInput(
                            "useFrac", 
                            HTML("Percent of Participating Households using Intervention<sup><small>10</small></sup>"), 
                            min = 0, 
                            max = 100, 
                            value = 50,
                            step =  1
                        )
                    )                
                )
            ),
            tabItem("health",
                fluidRow(
                    tabBox(
                    	title = "Averted Ill Health Plots",
                        width=12,
                        tabPanel(title = HTML('<img src = "bar_chart.png" width = "25" /><span class="tabPanelTitle">Overview</span>'),
                            highchartOutput("barchart"),
                            hr(),
                            HTML('<p><strong>Filter Results</strong></p>'),
                            column(4,
                                HTML('<p>Cause<p>'),
                                uiOutput("causeBarChartPlotFilter")
                            ),
                            column(3,
                                HTML('<p>Measure<p>'),
                                uiOutput("measureBarChartPlotFilter")                               
                            ),
                            column(2,
                                HTML('<p>Sex<p>'),
                                uiOutput("sexBarChartPlotFilter")                               
                            ),
                            column(3,
                                HTML('<p>Risk<p>'),
                                uiOutput("riskBarChartPlotFilter")                                
                            ),
	                        HTML('&nbsp;')
	                    ),
                        tabPanel(title = HTML('<img src = "point_with_error_bars.png" width = "25" /><span class="tabPanelTitle">Impacts by Age</span>'),
                            highchartOutput("impacts_by_age"),
                            hr(),
                            hr(),
                            HTML('<p><strong>Filter Results</strong></p>'),
                            column(4,
                                HTML('<p>Cause<p>'),
                                uiOutput("causeAgeChartPlotFilter")
                            ),
                            column(3,
                                HTML('<p>Measure<p>'),
                                uiOutput("measureAgeChartPlotFilter")                               
                            ),
                            column(2,
                                HTML('<p>Sex<p>'),
                                uiOutput("sexAgeChartPlotFilter")                               
                            ),
                            column(3,
                                HTML('<p>Risk<p>'),
                                uiOutput("riskAgeChartPlotFilter")                                
                            ),
                            HTML('&nbsp;')                            
                        ),
                        tabPanel(title = HTML('<img src = "table.png" width = "25" /><span class="tabPanelTitle">Detailed Results Table</span>'),
                            DT::dataTableOutput('atotals_table'),
                            HTML('&nbsp;')                            
                        )                        
                    ),
                    box(
                        title = "Ill-health Averted Summary",
                        width = 12,
                        DT::dataTableOutput('avertedOverview')
                    )   
            	)           
            ),
            tabItem("docs",
                fluidRow(
                    tabBox(
                        width = 12,
                        title = 'ABODE Documentation',
                        tabPanel("Overview",
                            HTML('<p>The burden of disease attributable to household PM<sub>2.5</sub> pollution is estimated for Lower Respiratory Infection in children and adults and for six diseases in adults:  Chronic Obstructive Pulmonary Disease (COPD), Ischemic Heart Disease (IHD), Lower Respiratory Infection (LRI), Lung Cancer,  Stroke, and Type 2 Diabetes Mellitus (T2DM).</p>'),

                            HTML("<p>The risk of each disease at a given pollution exposure is estimated using an <strong><a href='http://ehp.niehs.nih.gov/1307049/'>integrated exposure response (IER) function</a></strong>. More details on the IER modeling process can be found at <strong><a href='http://ghdx.healthmetricsandevaluation.org/record/global-burden-disease-study-2010-gbd-2010-ambient-air-pollution-risk-model-1990-2010'>IHME's website</a></strong>. In previous versions of HAPIT/ABODE, a lookup table was produced following the example of Burnett et al using <strong><a href='http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_CRCurve_values.csv'>data provided by IHME.</a></strong> The current version of ABODE uses updated curve parameters provided by IHME, which can be downloaded <a href='https://www.dropbox.com/s/kmw97rsp0wptciy/iers_2017.zip?dl=1' target='_blank'>here</a>."),

                            HTML('<p>The relative risk is used to find the population attributable fraction for each disease, which is the fraction of the background disease rate that is attributable to PM<sub>2.5</sub> pollution (rather than, say, high cholesterol intake).  The difference between the disease attributable to PM<sub>2.5</sub> at the pre- and post- exposure levels is the benefit of the intervention.</p>'),

                            HTML("<p>Burden of disease estimates and health benefits estimated by ABODE require definition of an 'ideal' counterfactual exposure, below which there is no risk to health. In the 2017 Burden of Disease, this value was set at ~3 µg/m<sup>3</sup> for annual average PM<sub>2.5</sub> exposure.</p>")
                            ),
                        tabPanel("Data Sources",
                            HTML("<p><strong>All country-level background disease -- including cause-specific deaths and DALYS -- and population data are from the 2017 Global Burden of Disease</strong>. Data were downloaded from <strong><a href='http://ghdx.healthdata.org/gbd-results-tool'>IHME's GBD Data Tool</a></strong>. The version of background disease and population data used in ABODE 1.0.0 can be downloaded <a href='https://www.dropbox.com/s/c1wlrga4fxzlzvi/health_and_pop_data.zip?dl=1' target='_blank'>here</a>.</p> "),

                            HTML("<p><strong>Data on household size and composition were downloaded from the UN Population division</strong>. For countries with multiple entries, the most recent entry was used; for countries with multiple entries from the most recent year, the highest estimate was used. Data used in ABODE 1.0.0 can be downloaded <a href='https://www.dropbox.com/s/ms7ufjjw1en6s8v/UNPD_Houseshold_Size_and_Composition_2018.zip?dl=1' target='_blank'> here.</a></p>"),

                            HTML("<p><strong>Data related to solid fuel use and exposure to ambient and household air pollution were provided by IHME.</strong></p>
                                <p style='margin-left: 40px;''>Solid fuel use data are based on models and methods described in <a href='https://www.sciencedirect.com/science/article/pii/S0160412018309565' target='_blank'>Shupler et al, 2018.</a> and on the associated <a href='https://www.sciencedirect.com/science/article/pii/S2352340918313374' target='_blank'>Global household air pollution database</a>. IHME-provided data contains modeled estimates for years 1999-2017 for most countries (and sub-nationally, in some instances). The most recent available estimate was utilized for each country. The raw data utilized in ABODE 1.0.0 can be downloaded <a href='https://www.dropbox.com/s/3c90q8xcizjel1g/hap_prop_exposure.zip?dl=1' target='_blank'>here</a>.<br>
                                </p>
                                <p style='margin-left: 40px;''>Average ambient air pollution data at the national level were provided for years 1990, 1995, 2000, 2005, and 2010 through 2017. Methods used to model exposure are described in <a href='https://pubs.acs.org/doi/10.1021/acs.est.8b02864' target='_blank'>Shaddick et al 2018</a>. Data used in ABODE 1.0.0 are available <a href='https://www.dropbox.com/s/ehyhfrnxu21i34v/oap_summary_all_locations.zip?dl=1' target='_blank'>here</a>.<br>
                                </p>
                                <p style='margin-left: 40px;''>Country-level household air pollution exposure estimates were derived from data provided by IHME. Two datasets were utilized; the first contained modeled estimates of kitchen concentrations of PM<sub>2.5</sub> for years 1999-2017 at the national and, in some cases, subnational level. The second database contained modeled ratios of the relationship between HAP kitchen concentrations and male, female, and child exposures. To determine average exposures for men, women, and children, kitchen levels in a given country were multiplied by the appropriate ratio. The database of kitchen concentrations is available <a href='https://www.dropbox.com/s/zf9lv216mx29zw8/hap_pm_map.zip?dl=1' target='_blank'>here</a> (WARNING: 200MB file); the modeled ratios are available <a href='https://www.dropbox.com/s/otyxrqmh473ugiw/hap_pm_ratios.zip?dl=1' target='_blank'>here</a>.<br>
                                </p>                                
                                ")
                            ),

                        tabPanel("Methods",
                            HTML("ABODE relies on methods (described <a href='https://www.dropbox.com/s/z0d3oeyvefqtqax/Lancet%20-%20Balakrishnan%20GBD%20Appendix.pdf?dl=1' target='_blank'> here</a>) utilized in the Comparative Risk Assessment (CRA) of GBD 2017 for estimates of avertable burden attributable to air pollution interventions.</p> 
                                <p>Unlike previous versions of the CRA, the 2017 version estimated the burden of ill-health associated with combined ambient and household PM exposure, rather than the burden associated with HAP and ambient pollution separately. In doing so, it sought to avoid double counting that may have occurred and is described in detail in <a href='https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2017GH000115' target='_blank'>Kodros et al, 2017</a>. To apportion the burden attributable to ambient air pollution, it multipled the total burden by the fraction of exposure arising from ambient air pollution. The same process was performed for HAP. As a simplified example, assume annual exposure to PM<sub>2.5</sub> in country X was 100 µg/m<sup>3</sup> and resulted in 1000 deaths per year. If 10 µg/m<sup>3</sup> of that total exposure arose from household sources, 100 deaths per year would be attributable to HAP exposure, while the remaining 900 would be attributable to ambient particulate pollution.</p>
                                <p>Thus, in ABODE, a total exposure -- composed of the sum of average HAP and ambient exposures -- is calculated for each country. This total exposure is used to estimate relative risks, PAFs, and burdens for each disease outcome. The default values from IHME are used to estimate the pre-intervention burden. The same approach is used for post-intervention exposures, albeit relying on user-input, presumably reduced ambient and HAP exposures resulting from the intervention.</p> 
                                <p>The difference between the total pre-intervention and total post-intervention burden is the total attributable burden. To determine the fraction of this burden attributable to HAP, the total attributable burden is multipled by the fraction of the change in exposure attributable to HAP. For example, if total exposure is reduced by 50 µg/m<sup>3</sup> (45 from the HAP intervention and 5 from subsequent changes in ambient air quality), the total attributable burden would be multiplied by 45/50 to estimate the HAP attributable burden. The remaining burden (5/50) would be attributable to reductions in ambient particulate polluion.</p>
                                ")
                        ),

                        tabPanel("Citation and Literature",
                            HTML("<p> <strong>Use the following to cite ABODE</strong>:</p>
                                <p>Air Pollutiom Burden of Disease Explorer. Atlanta, GA: Emory University and Berkeley, CA: UC Berkeley. 2013-2019. Available from http://householdenergy.shinyapps.io/abode. (Accessed [INSERT DATE])</p>

                                <p><strong>More details on the exact methodology used in ABODE can be found in </strong><br></p>
                                <p>Pillarisetti, A; Mehta, S; Smith, KR. <em><a href='http://www.kirkrsmith.org/publications/2016/3/9/re9eii99f9ijhqfjnkkvkscm869zgr'>HAPIT, the Household Air Pollution Intervention Tool, to evaluate the health benefits and cost-effectiveness of clean cooking interventions.</a></em> Ch 10 in Thomas, E., Ed, <em>Broken Pumps and Promises: Incentivizing Impact in Environmental Health</em>, Springer International Press, 2016, pp. 147-169.</p>
                                <p><strong>A number of publications use ABODE-like estimation to determine the impact of air pollution reducing interventions:</strong></p>
                                <p>Steenland K, Pillarisetti A, et al. (2018) Modeling the potential health benefits of lower household air pollution after a hypothetical liquified petroleum gas (LPG) cookstove intervention. Environment International 111:71–79.</p>
                                <p>Etchie TO et al. (2018) The gains in life expectancy by ambient PM<sub>2.5</sub> pollution reductions in localities in Nigeria. Environmental Pollution 236:146–157. doi: 10.1016/j.envpol.2018.01.034</p>
                                <p>Rosenthal J, Quinn A, Grieshop AP, Pillarisetti A, Glass RI (2018) Clean cooking and the SDGs: Integrated analytical approaches to guide energy interventions for health and environment goals. Energy for Sustainable Development 42:152–159. doi: 10.1016/j.esd.2017.11.003</p>
                                <p>Etchie TO et al. (2017) The health burden and economic costs averted by ambient PM<sub>2.5</sub> pollution reductions in Nagpur, India. Environment International 102:145–156.</p>
                                <p>Pillarisetti A* et al. (2017) Household Energy Interventions and Health and Finances in Haryana, India: An Extended Cost-Effectiveness Analysis. In: Disease Control Priorities, Third Edition (Volume 7): Injury Prevention and Environmental Health. The World Bank, pp 223–237</p>
                                <p>Anenberg SC, Henze DK, Lacey F, Irfan A, Kinney P, Kleiman G, Pillarisetti A. (2017) Air pollution-related health and climate benefits of clean cookstove programs in Mozambique. Environ Res Lett 12:025006–13. doi: 10.1088/1748-9326/aa5557</p>
                            ")
                        )
                        
                    )
                )
            ),
            tabItem("downloads",
                fluidRow(
                    box(title = "Download Report",
                        width=6,
                        status="info", solidHeader = TRUE,
                        p('ABODE allows users to download a summary report of their current run containing graphs, tables, and a short summary of their run parameters.'),
                        downloadButton("downloadPDF", 'Download Report')
                    ),
                    box(title = "Download Tables & Graphs",
                        width=6,
                        status="info", solidHeader = TRUE,
                        p('ABODE allows users to download a zip file containing graphs, tables, and a short summary of their run parameters. Future versions of ABODE will allow users to re-upload launch parameters so that repeated runs of ABODE can be replicated.'),
                        downloadButton("downloadArchive", 'Download Archive')
                    )               
               
                )
            )
    	)	
	)
)