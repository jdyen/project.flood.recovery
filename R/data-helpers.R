# helper functions to load data for flood recovery project

# function to add recruit info to the catch data set
populate_recruit_data <- function(flood_data_table, recruit_table) {
  
  site_sp_table <- recruit_table |> 
    distinct(id_site, scientific_name) |>
    mutate(base_val = 0)

  flood_data_table <- flood_data_table |>
    left_join(site_sp_table, by = c("id_site", "scientific_name")) |>
    mutate(
      plus1 = ifelse(is.na(plus1) & !is.na(base_val), 0, plus1),
      yoy = ifelse(is.na(yoy) & !is.na(base_val), 0, yoy)
    )
  
  flood_data_table <- flood_data_table |>
    left_join(
      recruit_table |> select(id_site, scientific_name, before_after, plus1, yoy), 
      by = c("id_site", "scientific_name", "before_after")
    ) |> 
    mutate(
      plus1.x = ifelse(
        !is.na(plus1.x) & is.na(plus1.y), 
        plus1.x, 
        ifelse(!is.na(plus1.y), plus1.y, NA)
      ), 
      yoy.x = ifelse(
        !is.na(yoy.x) & is.na(yoy.y),
        yoy.x, 
        ifelse(!is.na(yoy.y), yoy.y, NA)
      )
    )
  
  flood_data_table <- flood_data_table |>
    select(-ends_with(".y")) |>
    select(-"base_val") |>
    rename(plus1 = plus1.x, yoy = yoy.x)
  
  # return
  flood_data_table
  
}

# function to tidy up the recruit data
reformat_sp_recruit <- function(sp_data){
  
  sp_data <- sp_data |>
    select(id_site, survey_year, id_project, scientific_name, before_after, yoy) |>
    group_by(id_site, survey_year, id_project, scientific_name, before_after, yoy) |>
    count()
  
  sp_data <- sp_data |> 
    pivot_wider(names_from = yoy, values_from = n) |>
    rename(plus1 = No, yoy = Yes)

  sp_data[is.na(sp_data)] <- 0
  
  if (!("yoy" %in% colnames(sp_data)))
    sp_data <- sp_data |> mutate(yoy = as.integer(NA))
  if (!("plus1" %in% colnames(sp_data)))
    sp_data <- sp_data |> mutate(plus1 = as.integer(NA))
  
  # compact the data to line up before and after yoy/non counts
  sp_data
  
}
