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

