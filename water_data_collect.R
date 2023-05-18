#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Flood Analysis Project 2023

# WATER DATA COLLECT

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# remotes::install_github("aae-stats/aae.hydro")

library(aae.hydro)
library(lubridate)
##==========================================================================================================================================
##==========================================================================================================================================
##=================================== MAKE SURE GOCONNECT IS CONNECTED =====================================================================
##==========================================================================================================================================
##==========================================================================================================================================

site.station_date_range <- fetch_query(
  "WITH base as (
	SELECT id_site, station, min_distance_m, dist_rank
	FROM stream_network.v_aae_site_closest_gauges
		WHERE dist_rank = 1
	)
, date_groups as (
	SELECT CONCAT(EXTRACT(year from sdate), '_', EXTRACT(month from sdate)) yr_m_group,
	min(sdate) min_sdate,
	max(sdate) max_sdate
	FROM projects.v_floods_site_survey_dates
	GROUP BY CONCAT(EXTRACT(year from sdate), '_', EXTRACT(month from sdate))
)
, site_date_groups as (
	SELECT b.id_site, yr_m_group, min_sdate, max_sdate
	FROM date_groups a, projects.v_floods_site_survey_dates b
	WHERE b.sdate BETWEEN min_sdate AND max_sdate
)
SELECT a.id_site, station, yr_m_group, min_sdate, max_sdate
FROM base a INNER JOIN site_date_groups b
ON a.id_site = b.id_site
GROUP BY a.id_site, station, yr_m_group, min_sdate, max_sdate
ORDER BY yr_m_group, station",
collect = FALSE)

#collect the data
site.station_date_range <- site.station_date_range %>% collect()
site.station_date_range$station <- as.character(site.station_date_range$station)

##==========================================================================================================================================

##==========================================================================================================================================
##==========================================================================================================================================
##================================== SITE STATION FIXES ====================================================================================
##==========================================================================================================================================
##==========================================================================================================================================

site.station_date_range[site.station_date_range$id_site == 1285,]$station = "406201"
site.station_date_range[site.station_date_range$station == "406218",]$station = "406202"
site.station_date_range[site.station_date_range$station == "406264",]$station = "406202"
site.station_date_range[site.station_date_range$id_site == 1521,]$station = "409399"
site.station_date_range[site.station_date_range$id_site == 4065,]$station = "404227"
# site.station_date_range[site.station_date_range$id_site == c(4281, 4282, 4283, 4286),] = "409202"
site.station_date_range[site.station_date_range$id_site == 4281,]$station = "409202"
site.station_date_range[site.station_date_range$id_site == 4282,]$station = "409202"
site.station_date_range[site.station_date_range$id_site == 4283,]$station = "409202"
site.station_date_range[site.station_date_range$id_site == 4286,]$station = "409202"
site.station_date_range[site.station_date_range$id_site == 4145,]$station = "404214"

site.station_date_range[site.station_date_range$id_site == 1334,]$station = "405200"
site.station_date_range[site.station_date_range$id_site == 1535,]$station = "405204"
site.station_date_range[site.station_date_range$id_site == 1536,]$station = "405270"
site.station_date_range[site.station_date_range$id_site == 3157,]$station = "403200"
site.station_date_range[site.station_date_range$id_site == 3164,]$station = "403205"
site.station_date_range[site.station_date_range$id_site == 3142,]$station = "414211"
site.station_date_range[site.station_date_range$id_site == 3144,]$station = "414211"
site.station_date_range[site.station_date_range$id_site == 3146,]$station = "414211"
site.station_date_range[site.station_date_range$id_site == 3147,]$station = "414211"
site.station_date_range[site.station_date_range$id_site == 3149,]$station = "414211"
site.station_date_range[site.station_date_range$id_site == 3152,]$station = "414211"

site.station_date_range[site.station_date_range$id_site == 3311,]$station = "405234"
site.station_date_range[site.station_date_range$id_site == 3283,]$station = "405234"
site.station_date_range[site.station_date_range$id_site == 3284,]$station = "405234"
site.station_date_range[site.station_date_range$id_site == 3285,]$station = "405234"
site.station_date_range[site.station_date_range$id_site == 3287,]$station = "405234"
site.station_date_range[site.station_date_range$id_site == 3268,]$station = "405228"
site.station_date_range[site.station_date_range$id_site == 3213,]$station = "405228"
site.station_date_range[site.station_date_range$id_site == 3142,]$station = "414214"
site.station_date_range[site.station_date_range$id_site == 3144,]$station = "414214"
site.station_date_range[site.station_date_range$id_site == 3146,]$station = "414214"
site.station_date_range[site.station_date_range$id_site == 3147,]$station = "414214"
site.station_date_range[site.station_date_range$id_site == 3149,]$station = "414214"
site.station_date_range[site.station_date_range$id_site == 3152,]$station = "414214"
site.station_date_range[site.station_date_range$id_site == 3125,]$station = "414229"
site.station_date_range[site.station_date_range$id_site == 3126,]$station = "414229"
site.station_date_range[site.station_date_range$id_site == 3127,]$station = "414229"
site.station_date_range[site.station_date_range$id_site == 3129,]$station = "414229"
site.station_date_range[site.station_date_range$id_site == 3130,]$station = "414229"





##==========================================================================================================================================

# Find the common date ranges for the stations
station.date_range <- select(site.station_date_range, station, yr_m_group, min_sdate,  max_sdate) %>% group_by(station, yr_m_group, min_sdate,  max_sdate) %>% summarise()

#List the common date ranges to collect
date_groups <- station.date_range %>% select(yr_m_group) %>% group_by(yr_m_group) %>% summarise()

#create an empty flow data df to fill iteratively
all_flow_data <- data.frame()
i = 1

# collect the water data for the different gauges in the date groups
while (i <= nrow(date_groups))
{
  station_set <- station.date_range[station.date_range$yr_m_group == date_groups$yr_m_group[i],]
  
  flow_data <- fetch_hydro(
    sites = station_set$station,
    start = min(station_set$min_sdate),
    end = min(station_set$max_sdate),
    variables = c("discharge"), #, "level"),
    options = list(varfrom = c("100.00"),  varto = c("141.00")),
    data_source = "TELEM",
    # include_missing = TRUE,
    state = "vic"
  )

  all_flow_data <- rbind(all_flow_data, flow_data)
  i = i + 1
}

remove(station_set)
remove(flow_data)

#==============================================================================================================================
#link the collected water data to the sites
site.water_data <- left_join(site.station_date_range, all_flow_data, by = c('station' = 'site_code'), relationship = "many-to-many") %>% filter(between(date_formatted, min_sdate, max_sdate))

# create summary stats of the water data per site (mean is used only)
site.water_data <- site.water_data[site.water_data$variable_code ==  "141.00",] %>% select(id_site, variable_name, value, min_sdate) %>% mutate(yr= as.integer(format(min_sdate, format="%Y"))) %>% group_by(id_site, variable_name, yr) %>% summarise(min=min(value), max = max(value), mn= mean(value), range = max(value)-min(value), .groups = "keep") %>% add_count( name = "gauge_count") 

# rank the sites and year to determine before and after water data
site.water_data <- site.water_data %>% group_by(id_site) %>% mutate(rank = rank(yr))
site.water_data$dis_ba = ifelse(site.water_data$rank == 1,"before_discharge","after_discharge")

#pivot the water data for seperate before/after discharge columns
all_site.water_data = pivot_wider(site.water_data[,c("id_site", "mn", "dis_ba")], names_from = dis_ba, values_from = mn)

#==============================================================================================================================