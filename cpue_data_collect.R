#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Flood Analysis Project 2023

# CPUE DATA COLLECT

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


catch.cpue <- fetch_cpue(projects_to_use)
#filter data to min year and over (split filter to make sure collect works)
catch.cpue <- catch.cpue %>% filter(survey_year >= min_yr & waterbody %in% !!waterbodies$waterbody & id_site %in% !!site.list$id_site)
catch.cpue <- catch.cpue %>% collect()

#.......... include wetmap cpue data ...........................................................................................................
catch.cpue_wetmap <- fetch_query( "SELECT id_site::integer, waterbody, site_name, id_project, survey_year, survey_date, scientific_name, cpue FROM projects.v_floods_wetmap_cpue", collect = TRUE)

catch.cpue <- catch.cpue[,c("id_site", "waterbody", "site_name", "id_project", "survey_year", "survey_date", "scientific_name", "cpue")]
catch.cpue <- rbind(catch.cpue, catch.cpue_wetmap)
remove(catch.cpue_wetmap)
#.................................................................................................................................................

#inner join the analysis sites df and the cpue df to only retain relevant data
catch.cpue <- inner_join(catch.cpue, site.ba_years[,c('id_site', 'yr', 'id_project', 'rank')], by = c('id_site', "survey_year" = 'yr', 'id_project')) 

#factor rank (1 = after, 2 = before)
catch.cpue$rank<- as.factor(catch.cpue$rank)

str(catch.cpue)


##==========================================================================================================================================

#filter current dataset for sp available to the site
catch.cpue_filtered <- inner_join(catch.cpue, catch.sp, by = c('id_site', 'scientific_name'))

#filter current dataset to focal species
catch.cpue_filtered <- catch.cpue_filtered[catch.cpue_filtered$scientific_name %in% sp,]

#transpose the before and after cpue values
catch.cpue_ba = pivot_wider(catch.cpue_filtered, names_from = rank, values_from = cpue)
colnames(catch.cpue_ba)[colnames(catch.cpue_ba) == '2'] <- 'before_cpue'
colnames(catch.cpue_ba)[colnames(catch.cpue_ba) == '1'] <- 'after_cpue'

#remove na values in cpue columns
catch.cpue_ba$before_cpue <- ifelse(is.na(catch.cpue_ba$before_cpue), 0, catch.cpue_ba$before_cpue)
catch.cpue_ba$after_cpue <- ifelse(is.na(catch.cpue_ba$after_cpue), 0, catch.cpue_ba$after_cpue)

#Compact the data to line up before and after cpue
catch.cpue_ba <- catch.cpue_ba %>% select('id_site', 'waterbody', 'site_name', 'scientific_name', 'before_cpue', 'after_cpue' ) %>% group_by(id_site, waterbody, site_name, scientific_name) %>% summarise(before_cpue = max(before_cpue), after_cpue = max(after_cpue))

#remove species not caught in both before and after surveys (keep them in for now)
# catch.cpue_ba <- catch.cpue_ba[catch.cpue_ba$before_cpue > 0 | catch.cpue_ba$after_cpue > 0,]

#get a list of sites to check against site.list to make sure all sites are included
catch.cpue_site_list <- catch.cpue_ba %>% group_by(id_site) %>% summarise()




