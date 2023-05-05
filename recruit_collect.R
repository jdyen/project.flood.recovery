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

reformat_sp_recruit <- function(sp_data){
  no_added = 0

  sp_data <- sp_data %>% select(id_site, survey_year, id_project, scientific_name, rank, yoy) %>% group_by(id_site, survey_year, id_project, scientific_name, rank, yoy) %>% count()
  sp_data = pivot_wider(sp_data, names_from = yoy, values_from = n)
  
  if("No" %in% colnames(sp_data)){
    sp_data <- pivot_wider(sp_data, names_from = rank, values_from = No)
    colnames(sp_data)[colnames(sp_data) == '2'] <- 'before_1plus'
    colnames(sp_data)[colnames(sp_data) == '1'] <- 'after_1plus'
    no_added = 1
  }
  
  if("Yes" %in% colnames(sp_data) & no_added == 1){
    sp_data <- inner_join(sp_data, site.ba_years[,c('id_site', 'yr', 'id_project', 'rank')], by = c('id_site', "survey_year" = 'yr', 'id_project')) 
  }
  
  if("Yes" %in% colnames(sp_data)){
    sp_data <- pivot_wider(sp_data, names_from = rank, values_from = Yes)
    colnames(sp_data)[colnames(sp_data) == '2'] <- 'before_yoy'
    colnames(sp_data)[colnames(sp_data) == '1'] <- 'after_yoy'
  }
  
  sp_data[is.na(sp_data)] <- 0
  
  if(!("before_1plus" %in% colnames(sp_data))){sp_data$before_1plus = as.integer(NA)}
  if(!("after_1plus" %in% colnames(sp_data))){sp_data$after_1plus = as.integer(NA)}
  if(!("before_yoy" %in% colnames(sp_data))){sp_data$before_yoy = as.integer(NA)}
  if(!("after_yoy" %in% colnames(sp_data))){sp_data$after_yoy = as.integer(NA)}
  
  #Compact the data to line up before and after yoy/non counts
  reformat_sp_recruit = sp_data[,c('id_site', 'id_project', 'scientific_name', 'before_1plus', 'after_1plus', 'before_yoy', 'after_yoy')] %>% group_by(id_site, id_project, scientific_name) %>% summarise(before_1plus = max(before_1plus), after_1plus := max(after_1plus), before_yoy := max(before_yoy), after_yoy := max(after_yoy))

}


##==========================================================================================================================================
##==========================================================================================================================================
##==========================================================================================================================================
##==========================================================================================================================================
##==========================================================================================================================================


catch.lw <- fetch_project(projects_to_use)
#filter data to min year and over (split filter to make sure collect works)
catch.lw <- catch.lw %>% filter(survey_year >= min_yr & waterbody %in% !!waterbodies$waterbody & id_site %in% !!site.list$id_site)
catch.lw <- catch.lw %>% collect()

#inner join the analysis sites df and the cpue df to only retain relevant data
catch.lw <- inner_join(catch.lw, site.ba_years[,c('id_site', 'yr', 'id_project', 'rank')], by = c('id_site', "survey_year" = 'yr', 'id_project')) 

#factor rank (1 = after, 2 = before)
catch.lw$rank<- as.factor(catch.lw$rank)

##==========================================================================================================================================

#filter current dataset to focal species
catch.lw_filtered <- catch.lw[catch.lw$scientific_name %in% sp,]

#Only keep those samples with fork or total length
catch.lw_filtered <- catch.lw_filtered[!is.na(catch.lw_filtered$fork_length_cm) | !is.na(catch.lw_filtered$length_cm),]

##==========================================================================================================================================
##==========================================================================================================================================

# Murray Cod
catch.lw_mc <- catch.lw_filtered[catch.lw_filtered$scientific_name == "Maccullochella peelii", c('id_site', 'survey_year', 'id_project', 'scientific_name', 'length_cm', 'weight_g', 'rank' )]
catch.lw_mc$yoy <- ifelse(catch.lw_mc$length_cm >= 13, "No", "Yes")

#Trout Cod
catch.lw_tc <- catch.lw_filtered[catch.lw_filtered$scientific_name == "Maccullochella macquariensis", c('id_site', 'survey_year', 'id_project', 'scientific_name', 'length_cm', 'weight_g', 'rank' )]
catch.lw_tc$yoy <- ifelse(catch.lw_tc$length_cm >= 13, "No", "Yes")

#Macquarie Perch
# catch.lw_mp <- catch.lw_filtered[catch.lw_filtered$scientific_name == "Macquaria australasica", c('id_site', 'survey_year', 'id_project', 'scientific_name', 'length_cm', 'weight_g', 'rank' )]
# catch.lw_mp$yoy <- ifelse(catch.lw_mp$length_cm >= 10, "No", "Yes")

#Blackfish
catch.lw_bf <- catch.lw_filtered[catch.lw_filtered$scientific_name == "Gadopsis marmoratus", c('id_site', 'survey_year', 'id_project', 'scientific_name', 'length_cm', 'weight_g', 'rank' )]
catch.lw_bf$yoy <- ifelse(catch.lw_bf$length_cm >= 8, "No", "Yes")

"Carp"
catch.lw_cc <- catch.lw_filtered[catch.lw_filtered$scientific_name == "Cyprinus carpio", c('id_site', 'survey_year', 'id_project', 'scientific_name', 'fork_length_cm', 'length_cm','weight_g', 'rank' )]
catch.lw_cc$fork_length_cm <- ifelse(is.na(catch.lw_cc$fork_length_cm), catch.lw_cc$length_cm, catch.lw_cc$fork_length_cm)
catch.lw_cc$yoy <- ifelse(catch.lw_cc$fork_length_cm >= 15, "No", "Yes")

##==========================================================================================================================================
##==========================================================================================================================================

#Reformat the species dataset to append to the main flood data frame (see data_collect)
catch.lw_mc <- reformat_sp_recruit(catch.lw_mc)
catch.lw_tc <- reformat_sp_recruit(catch.lw_tc)
catch.lw_bf <- reformat_sp_recruit(catch.lw_bf)
catch.lw_cc <- reformat_sp_recruit(catch.lw_cc)



