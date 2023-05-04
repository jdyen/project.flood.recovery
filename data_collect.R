#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Flood Analysis Project 2023

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# remotes::install_github("aae-stats/aae.db")


##==========================================================================================================================================
##==========================================================================================================================================
##=================================== MAKE SURE GOCONNECT IS CONNECTED =====================================================================
##==========================================================================================================================================
##==========================================================================================================================================


library(aae.db)
library(dplyr)
library(tidyr)

##==========================================================================================================================================
##==========================================================================================================================================
##==========================================================================================================================================
##==========================================================================================================================================

# find sites fished this year (2023) and their previous fished year (only VEFMAP for now)
sites.ba_years <- fetch_query(
  "WITH base_data AS (
    SELECT id_site, site_name, waterbody, id_project, yr
    	FROM aquatic_data.v_site_year
    	WHERE waterbody IN ('Broken River', 'Little Murray River', 'Campaspe River') AND id_project = 2
	UNION ALL
    SELECT id_site, site_name, waterbody, id_project, yr
    	FROM aquatic_data.v_site_year
    	WHERE id_project = 19
    )
	, rank_site_years as (
	
SELECT id_site, site_name, waterbody, id_project, yr, rank() over(partition by waterbody, id_site order by waterbody, id_site, yr DESC) rank
    	FROM base_data
    	ORDER BY waterbody, id_site, yr DESC	
	
	)
    , sites_to_use as (
    SELECT id_site, max(yr), count(*)
    FROM rank_site_years
    WHERE rank in (1,2) 
    GROUP BY id_site
    HAVING max(yr) = 2023 AND  count(*)  = 2
    )
    SELECT a.id_site, site_name, waterbody, id_project, yr, rank
    FROM rank_site_years a INNER JOIN sites_to_use b ON a.id_site = b.id_site
    WHERE rank in (1,2)
    ORDER BY waterbody, site_name, a.id_site, id_project, rank",
  collect = FALSE
)

#collect the data
sites.ba_years <- sites.ba_years %>% collect()

# Get the waterbodies from the current dataset to use in species filter
waterbodies <- as.data.frame(distinct(sites.ba_years, waterbody))
# colnames(waterbodies) = 'waterbody'

str(sites.ba_years)
# get the minimum year of previous surveys to truncate the VEFMAP cpue intial data download
min_yr = min(sites.ba_years$yr)

##==========================================================================================================================================

projects_to_use <- c(2, 19)

catch.cpue <- fetch_cpue(projects_to_use)
#filter data to min year and over (split filter to make sure collect works)
catch.cpue <- catch.cpue %>% filter(survey_year >= min_yr & waterbody %in% !!waterbodies$waterbody)
catch.cpue <- catch.cpue %>% collect()

#inner join the analysis sites df and the cpue df to only retain relevant data
catch.cpue <- inner_join(catch.cpue, sites.ba_years[,c('id_site', 'yr', 'id_project', 'rank')], by = c('id_site', "survey_year" = 'yr', 'id_project')) 

#factor rank (1 = after, 2 = before)
catch.cpue$rank<- as.factor(catch.cpue$rank)

str(catch.cpue)

##==========================================================================================================================================
#get list of species per site over site recent history
catch.sp <- fetch_cpue(projects_to_use)
catch.sp <- catch.sp %>% filter(waterbody %in% !!waterbodies$waterbody)
catch.sp <- catch.sp %>% select(c('id_site', 'scientific_name', 'catch')) %>% group_by(id_site, scientific_name) %>% summarise(catch_total = sum(catch), .groups = "keep")
catch.sp <- catch.sp %>% collect()
catch.sp <- catch.sp[catch.sp$catch_total > 0,]

##==========================================================================================================================================

#filter current dataset for sp available to the site
catch.cpue_filtered <- inner_join(catch.cpue, catch.sp, by = c('id_site', 'scientific_name'))

#list project focal species
sp = c('Cyprinus carpio' , 'Maccullochella peelii' , 'Maccullochella macquariensis', 'Macquaria ambigua' , 'Melanotaenia fluviatilis' , 'Perca fluviatilis' , 'Retropinna semoni' , 'Bidyanus bidyanus', 'Macquaria australasica', 'Salmo trutta', 'Gadopsis marmoratus', 'Gadopsis bispinosus')

#filter current dataset to focal species
catch.cpue_filtered <- catch.cpue_filtered[catch.cpue_filtered$scientific_name %in% sp,]

#transpose the before and after cpue values
flood_data_ba = pivot_wider(catch.cpue_filtered, names_from = rank, values_from = cpue)
colnames(flood_data_ba)[colnames(flood_data_ba) == '2'] <- 'before_cpue'
colnames(flood_data_ba)[colnames(flood_data_ba) == '1'] <- 'after_cpue'

#remove na values in cpue columns
flood_data_ba$before_cpue <- ifelse(is.na(flood_data_ba$before_cpue), 0, flood_data_ba$before_cpue)
flood_data_ba$after_cpue <- ifelse(is.na(flood_data_ba$after_cpue), 0, flood_data_ba$after_cpue)

#Compact the data to line up before and after cpue
flood_data_ba <- flood_data_ba %>% select('id_site', 'waterbody', 'site_name', 'scientific_name', 'before_cpue', 'after_cpue' ) %>% group_by(id_site, waterbody, site_name, scientific_name) %>% summarise(before_cpue = max(before_cpue), after_cpue = max(after_cpue))

#remove species not caught in both before and after surveys
flood_data_ba <- flood_data_ba[flood_data_ba$before_cpue > 0 | flood_data_ba$after_cpue > 0,]

##==========================================================================================================================================
# Merge flood impact categories
site.flood_impact <- read.csv("site_flood_impact.csv", header = TRUE)
flood_data_ba <- inner_join(flood_data_ba, site.flood_impact, by = c('id_site'))

##==========================================================================================================================================
##==========================================================================================================================================
##==========================================================================================================================================

### Run code in water_data_collect.R to generate before/after discharge values (all_site.water_data) for sites

# Merge before/after discharge data
flood_data_ba <- inner_join(flood_data_ba, all_site.water_data, by = c('id_site'))

##==========================================================================================================================================
##==========================================================================================================================================
##==========================================================================================================================================



