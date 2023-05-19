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
##======================================= FUNCTIONS ========================================================================================
##==========================================================================================================================================

check_row_counts <- function(count1, count2, error_message){
  ifelse(count1 != count2, error_message, "All GOOD")
}

populate_recruit_data <- function(flood_data_table, recruit_table){
  
  recruit_table <- inner_join(flood_data_table[,c('id_site', 'scientific_name')], recruit_table, by = c('id_site', 'scientific_name'))
  
  flood_data_table[flood_data_table$id_site %in% recruit_table$id_site & flood_data_table$scientific_name %in% recruit_table$scientific_name,]$before_1plus = recruit_table[['before_1plus']]
  flood_data_table[flood_data_table$id_site %in% recruit_table$id_site & flood_data_table$scientific_name %in% recruit_table$scientific_name,]$after_1plus = recruit_table[['after_1plus']]
  flood_data_table[flood_data_table$id_site %in% recruit_table$id_site & flood_data_table$scientific_name %in% recruit_table$scientific_name,]$before_yoy = recruit_table[['before_yoy']]
  flood_data_table[flood_data_table$id_site %in% recruit_table$id_site & flood_data_table$scientific_name %in% recruit_table$scientific_name,]$after_yoy = recruit_table[['after_yoy']]
  pop_recruit_data <- flood_data_table
  
}

##==========================================================================================================================================
##==========================================================================================================================================
##==========================================================================================================================================
##==========================================================================================================================================

# find sites fished this year (2023) and their previous fished year (only VEFMAP for now)
site.ba_years <- fetch_query(
  "SELECT id_site, site_name, waterbody, id_project, yr, sdate, rank
	 FROM projects.v_floods_site_survey_dates
   ORDER BY waterbody, site_name, id_site, id_project, rank",
  collect = FALSE
)

#collect the data
site.ba_years <- site.ba_years %>% collect()

# Get the waterbodies from the current dataset to use in species filter
waterbodies <- as.data.frame(distinct(site.ba_years, waterbody))
# colnames(waterbodies) = 'waterbody'

str(site.ba_years)
# get the minimum year of previous surveys to truncate the VEFMAP cpue intial data download
min_yr = min(site.ba_years$yr)

projects_to_use <- array(unlist(distinct(site.ba_years, id_project)))

##==========================================================================================================================================

#list project focal species
sp = c('Cyprinus carpio' , 'Maccullochella peelii' , 'Maccullochella macquariensis', 'Macquaria ambigua' , 'Melanotaenia fluviatilis' , 'Perca fluviatilis' , 'Retropinna semoni' , 'Bidyanus bidyanus', 'Macquaria australasica', 'Salmo trutta', 'Gadopsis marmoratus', 'Gadopsis bispinosus')

##==========================================================================================================================================

#get list of species per site over site recent history
catch.sp <- fetch_cpue(projects_to_use)
catch.sp <- catch.sp %>% filter(waterbody %in% !!waterbodies$waterbody)
catch.sp <- catch.sp %>% select(c('id_site', 'scientific_name', 'catch')) %>% group_by(id_site, scientific_name) %>% summarise(catch_total = sum(catch), .groups = "keep")
catch.sp <- catch.sp %>% collect()
catch.sp <- catch.sp[catch.sp$catch_total > 0,]

#need to add wetmap data
catch.sp_wetmap <- fetch_query( "SELECT id_site, scientific_name, catch_total FROM projects.v_floods_wetmap_sp_catch", collect = TRUE)

catch.sp <- rbind(catch.sp, catch.sp_wetmap)

remove(catch.sp_wetmap)

##==========================================================================================================================================

#filter current dataset for sp available to the site
flood_data_ba <- inner_join(site.ba_years, catch.sp[,c('id_site', 'scientific_name')], by = c('id_site'), relationship = "many-to-many")

#filter current dataset to focal species
flood_data_ba <- flood_data_ba[flood_data_ba$scientific_name %in% sp,]

flood_data_ba <- select(flood_data_ba, c('id_site', 'site_name', 'waterbody', 'id_project','scientific_name')) %>% group_by(id_site, site_name, waterbody, id_project, scientific_name) %>% summarise()

##==========================================================================================================================================

# core list of sites to be used in the analysis (used for checking that none are lost in various stages)
site.list <- flood_data_ba %>% group_by(id_site, waterbody) %>% summarise()

##==========================================================================================================================================
##========================================== CPUE DATA =====================================================================================
##==========================================================================================================================================

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
### Run code in cpue_data_collect.R to generate before/after cpue values (catch.cpue_ba) for sites
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#Check that site count is equal
print(check_row_counts(nrow(site.list), nrow(catch.cpue_ba %>% group_by(id_site) %>% summarise()), "MISSING SITEs catch.cpue_ba" ))

# Merge before/after discharge data (can make this a LEFT join to include existing site species absent from BA surveys)
flood_data_ba <- inner_join(flood_data_ba, catch.cpue_ba[,c('id_site', 'scientific_name', 'before_cpue', 'after_cpue')], by = c('id_site', 'scientific_name'))

##==========================================================================================================================================
##=========================================== DISCHARGE DATA ===============================================================================
##==========================================================================================================================================

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
### Run code in water_data_collect.R to generate before/after discharge values (all_site.water_data) for sites
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#Check that site count is equal
print(check_row_counts(nrow(site.list), nrow(all_site.water_data %>% group_by(id_site) %>% summarise()), "MISSING SITEs in all_site.water_data" ))

# Merge before/after discharge data
flood_data_ba <- inner_join(flood_data_ba, all_site.water_data, by = c('id_site'))

##==========================================================================================================================================
##=========================================== RECRUIT DATA ===============================================================================
##==========================================================================================================================================

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
### Run code in recruit_data_collect.R to generate before/after yoy counts for sites
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

flood_data_ba$before_1plus = as.integer(0)
flood_data_ba$after_1plus = as.integer(0)
flood_data_ba$before_yoy = as.integer(0)
flood_data_ba$after_yoy = as.integer(0)

# Merge before/after recruit data
flood_data_ba <- populate_recruit_data(flood_data_ba, catch.lw_mc)
flood_data_ba <- populate_recruit_data(flood_data_ba, catch.lw_tc)
flood_data_ba <- populate_recruit_data(flood_data_ba, catch.lw_mp)
flood_data_ba <- populate_recruit_data(flood_data_ba, catch.lw_bf)
flood_data_ba <- populate_recruit_data(flood_data_ba, catch.lw_cc)

##==========================================================================================================================================
##=========================================== FLOOD IMPACT CATEGORIES ======================================================================
##==========================================================================================================================================

# Merge flood impact categories
site.flood_impact <- read.csv("site_flood_impact.csv", header = TRUE)
flood_data_ba <- inner_join(flood_data_ba, site.flood_impact, by = c('id_site'))

#Check that sites haven't been lost
print(check_row_counts(nrow(site.list), nrow(flood_data_ba %>% group_by(id_site) %>% summarise()), "MISSING SITES IN THE IMPACT LIST" ))

##==========================================================================================================================================


aaedb_disconnect()
