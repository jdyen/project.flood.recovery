# remotes::install_github("aae-stats/aae.db")


##==========================================================================================================================================
##==========================================================================================================================================
##=================================== MAKE SURE GOCONNECT IS CONNECTED =====================================================================
##==========================================================================================================================================
##==========================================================================================================================================


library(aae.db)
library(dplyr)

##==========================================================================================================================================
##==========================================================================================================================================
##==========================================================================================================================================
##==========================================================================================================================================

# find sites fished this year (2023) and their previous fished year (only VEFMAP for now)
survey_sites_ba_years <- fetch_query(
  "WITH base_data AS (
    SELECT id_site, site_name, waterbody, id_project, yr, rank() over(partition by waterbody, id_site order by waterbody, id_site, yr DESC) rank
    	FROM aquatic_data.v_site_year
    	WHERE waterbody IN ('Broken River', 'Little Murray River') AND id_project = 2
    	ORDER BY waterbody, id_site, yr DESC
    )
    , sites_to_use as (
    SELECT id_site, max(yr), count(*)
    FROM base_data
    WHERE rank in (1,2) 
    GROUP BY id_site
    HAVING max(yr) = 2023 AND  count(*)  = 2
    )
    SELECT a.id_site, site_name, waterbody, id_project, yr, rank
    FROM base_data a INNER JOIN sites_to_use b ON a.id_site = b.id_site
    WHERE rank in (1,2)
    ORDER BY site_name, a.id_site, waterbody, id_project, rank",
  collect = FALSE
)

#collect the data
survey_sites_ba_years <- survey_sites_ba_years %>% collect()

str(survey_sites_ba_years)
# get the minimum year of previous surveys to truncate the VEFMAP cpue intial data download
min_yr = min(survey_sites_ba_years$yr)


vefmap_cpue <- fetch_cpue(2)
#filter data to min year and over
vefmap_cpue <- vefmap_cpue %>% filter(survey_year >= min_yr )

vefmap_cpue <- vefmap_cpue %>% collect()

#inner join the analysis sites df and the cpue df to only retain relevant data
vefmap_cpue <- inner_join(vefmap_cpue, survey_sites_ba_years, by = c('id_site', "survey_year" = 'yr')) 

#factor rank (1 = after, 2 = before)
vefmap_cpue$rank<- as.factor(vefmap_cpue$rank)

# Get the waterbodies from the current dataset to use in species filter
waterbodies <- distinct(vefmap_cpue, waterbody.x)
colnames(waterbodies) = 'waterbody'

#=======================================================================================================
#get list of species per site over site recent history
vefmap_sp <- fetch_cpue(2)

#!!!! Actually want this filtered by the waterbodies df
vefmap_sp <- vefmap_sp %>%
  filter(
    waterbody %in% c('Broken River', 'Little Murray River')
  )

# vefmap_sp <- vefmap_sp %>% filter(survey_year >= 2017)
vefmap_sp <- vefmap_sp %>% collect()

# vefmap_sp <- vefmap_sp[vefmap_sp$waterbody %in% waterbodies$waterbody.x, ]
vefmap_sp <- vefmap_sp %>% select(c('id_site', 'scientific_name', 'catch')) %>% group_by(id_site, scientific_name) %>% summarise(catch_total = sum(catch))
vefmap_sp <- vefmap_sp[vefmap_sp$catch_total > 0,]

#=======================================================================================================

#filter current dataset for sp available to the site
vefmap_cpue.filtered <- inner_join(vefmap_cpue, vefmap_sp, by = c('id_site', 'scientific_name'))

#list project focal species
sp = c('Cyprinus carpio' , 'Maccullochella peelii' , 'Macquaria ambigua' , 'Melanotaenia fluviatilis' , 'Perca fluviatilis' , 'Retropinna semoni' , 'Bidyanus bidyanus', 'Macquaria australasica', 'Salmo trutta', 'Gadopsis marmoratus', 'Gadopsis bispinosus')

#filter current dataset to focal species
vefmap_cpue.filtered <- vefmap_cpue.filtered[vefmap_cpue.filtered$scientific_name %in% sp,]






