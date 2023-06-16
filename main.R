# analysis of fish species responses to and recovery from flooding

# load some packages
library(aae.db)
library(aae.hydro)
library(qs)
library(dplyr)
library(tidyr)
library(lubridate)
library(brms)
library(ggplot2)
library(ggeffects)
library(ragg)
library(sf)
library(ggspatial)

# flag: do we want to refit the models?
refit_models <- FALSE

# load some helper functions
source("R/data-helpers.R")

# find sites fished this year (2023) and their previous fished year (only VEFMAP for now)
site.ba_years <- fetch_query(
  "SELECT id_site, site_name, waterbody, id_project, yr, sdate, rank
	 FROM projects.v_floods_site_survey_dates
   ORDER BY waterbody, site_name, id_site, id_project, rank",
  collect = FALSE
)

# collect the data and recode before/after states
site.ba_years <- site.ba_years |> collect()
site.ba_years$before_after <- ifelse(site.ba_years$rank == 1, "after", "before")

# which waterbodies are included? And which years and projects?
waterbodies <- site.ba_years |> pull(waterbody) |> unique()
min_yr <- site.ba_years |> pull(yr) |> min()
projects_to_use <- site.ba_years |> pull(id_project) |> unique()

# list project focal species
sp <- c(
  "Cyprinus carpio",
  "Maccullochella peelii", 
  "Maccullochella macquariensis",
  "Macquaria ambigua", 
  "Melanotaenia fluviatilis",
  "Perca fluviatilis", 
  "Retropinna semoni",
  "Bidyanus bidyanus",
  "Macquaria australasica", 
  "Salmo trutta",
  "Gadopsis marmoratus",
  "Gadopsis bispinosus"
)

# get list of species per site over site recent history
catch.sp <- fetch_cpue(projects_to_use) |>
  filter(waterbody %in% !!waterbodies) |> 
  select(c("id_site", "scientific_name", "catch")) |>
  group_by(id_site, scientific_name) |>
  summarise(catch_total = sum(catch), .groups = "keep") |>
  filter(catch_total > 0) |>
  collect() |>
  rbind(
    fetch_query(
      "SELECT id_site, scientific_name, catch_total FROM projects.v_floods_wetmap_sp_catch",
      collect = TRUE
    )
  )

# filter current dataset to include only sp recorded at the site and in
#    list of focal species
flood_data_ba <- site.ba_years |>
  inner_join(
    catch.sp |> select(id_site, scientific_name),
    by = c("id_site"), 
    relationship = "many-to-many"
  ) |>
  filter(scientific_name %in% sp) |>
  distinct(id_site, site_name, waterbody, id_project, scientific_name, before_after)

# create a list of all sites to be included
site_list <- flood_data_ba |>
  distinct(waterbody, id_site)

# download CPUE data and filter to target years, waterbodies, and sites
catch.cpue <- fetch_cpue(projects_to_use) |>
  filter(
    survey_year >= min_yr,
    waterbody %in% !!waterbodies,
    id_site %in% !!site_list$id_site
  ) |>
  collect() |>
  bind_rows(
    fetch_query(
      "SELECT id_site::integer, waterbody, site_name, id_project, survey_year, survey_date, gear_type, scientific_name, effort_h, catch, cpue FROM projects.v_floods_wetmap_cpue", 
      collect = TRUE
    ) |>
      select(
        id_site, waterbody, site_name, id_project,
        survey_year, survey_date, gear_type,
        scientific_name, effort_h, catch, cpue
      )
  ) |>
  mutate(
    gear_type = ifelse(gear_type == "netting", "netting", "electrofishing")
  )

# inner join the analysis sites df and the cpue df to only retain relevant data
catch.cpue_ba <- catch.cpue |>
  inner_join(
    site.ba_years |> select(id_site, yr, id_project, rank), 
    by = c("id_site", "survey_year" = "yr", "id_project")
  ) |>
  mutate(
    rank = as.factor(rank),
    before_after = ifelse(rank == 1, "after", "before")
  ) |>
  inner_join(catch.sp, by = c("id_site", "scientific_name")) |>
  filter(scientific_name %in% sp) |>
  select(id_site, gear_type, scientific_name, before_after, effort_h, catch, cpue)

# pull out list of sites to check against site_list
catch.cpue_site_list <- catch.cpue_ba |> pull(id_site) |> unique()

# check we have the correct number of sites
print(nrow(site_list) == length(catch.cpue_site_list))

# merge before/after discharge data (can make this a LEFT join to
#   include existing site species absent from BA surveys)
flood_data_ba <- flood_data_ba |>
  inner_join(
    catch.cpue_ba |> 
      select(
        id_site, scientific_name, before_after,
        gear_type, effort_h, catch, cpue
      ),
    by = c("id_site", "scientific_name", "before_after")
  )

# download flow data for the relevant date range at each site
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

# collect the data
site.station_date_range <- site.station_date_range |>
  collect() |>
  mutate(
    station = as.character(station),
    source = "TELEM"
  )

# fix a few stations
site.station_date_range <- site.station_date_range |>
  mutate(
    station = ifelse(id_site == 1285, "406201", station),
    station = ifelse(station %in% c("406218", "406264"), "406202", station),
    station = ifelse(id_site == 1521, "409399", station),
    station = ifelse(id_site == 4065, "404227", station),
    station = ifelse(id_site == 4145, "404214", station),
    station = ifelse(id_site == 1536, "405270", station),
    station = ifelse(id_site == 3157, "403200", station),
    station = ifelse(id_site == 3164, "403205", station),
    
    # Barmah
    station = ifelse(id_site %in% c(4281, 4282, 4283, 4286, 4284, 4285, 4288), "409202", station),
    
    # Lindsay River
    station = ifelse(id_site %in% c(3125, 3126, 3127, 3129, 3130), "414212", station),
    source = ifelse(id_site %in% c(3125, 3126, 3127, 3129, 3130), "A", source),
    
    # Mullaroo Creek
    station = ifelse(id_site %in% c(3142, 3144, 3146, 3147, 3149, 3152), "414211", station),
    station = ifelse(id_site %in% c(3311, 3283, 3284, 3285, 3287, 3268, 3213), "405234", station),
    
    # Goulburn
    station = ifelse(id_site %in% c(1536, 1535, 1530), "405204", station),
    station = ifelse(id_site == 1334, "405200", station),
    station = ifelse(id_site == 1531, "405232", station),
    
    # Ovens
    station = ifelse(id_site == 3156, "403241", station),
    
    # Sevens
    station = ifelse(id_site == 3286, "405234", station),
    
    # Gunbower Ck
    station = ifelse(id_site %in% c(3115, 3117, 3118, 3119, 3120, 3121, 3122, 3123, 3124), "409207", station),
    
    # Catfish and Duckfoots Lagoons
    station = ifelse(id_site %in% c(4439, 4441, 4442, 4440, 4443, 4444, 4445, 4446), "414207", station),
    
    # Margooya Lagoon
    station = ifelse(id_site %in% c(4447, 4448, 4449, 4450), "414203", station),
    
    # Loddon
    station = ifelse(id_site == 1357, "407224", station),
    station = ifelse(id_site %in% c(1363, 1365), "407205", station),
    station = ifelse(id_site %in% c(1366, 1517, 1518), "407202", station),
    
    # Mid-Murray
    station = ifelse(id_site == 4261, "409207", station),
    station = ifelse(id_site == 4263, "409204", station),
    station = ifelse(id_site %in% c(4265, 4264), "414201", station)
    
  )

# find the common date ranges for the stations
station.date_range <- site.station_date_range |>
  distinct(station, yr_m_group, min_sdate,  max_sdate, source)

# list the common date ranges to collect
date_groups <- station.date_range |> distinct(yr_m_group)

# work through each date group and download data for all relevant stations
all_flow_data <- vector("list", length = nrow(date_groups))

# collect the water data for the different gauges in the date groups
for (i in seq_len(nrow(date_groups))) {
  
  # work out all stations in that date range, plus their sources
  station_set <- station.date_range |>
    filter(yr_m_group == date_groups$yr_m_group[i])
  sources <- station_set |> pull(source) |> unique()
  
  # make a temp list to store flow data for all sources
  flow_tmp <- vector("list", length = length(sources))
  
  # work through each source and grab data
  for (j in seq_along(sources)) {
    
    flow_data <- fetch_hydro(
      sites = station_set$station,
      start = min(station_set$min_sdate),
      end = min(station_set$max_sdate),
      variables = c("discharge"),
      options = list(varfrom = c("100.00"),  varto = c("141.00")),
      data_source = sources[j],
      state = "vic"
    )
    
    flow_tmp[[j]] <- flow_data
  }
  
  # collapse all sources into a single table and save
  all_flow_data[[i]] <- bind_rows(flow_tmp)
  
}

# and collapse the flow data into a single table
all_flow_data <- bind_rows(all_flow_data)

# link the collected water data to the sites
site.water_data <- site.station_date_range |>
  left_join(
    all_flow_data,
    by = c("station" = "site_code"), 
    relationship = "many-to-many"
  ) |>
  filter(between(date_formatted, min_sdate, max_sdate))

# create summary stats of the water data per site (mean is used only)
site.water_data <- site.water_data |>
  filter(variable_code == "141.00") |>
  select(id_site, variable_name, value, min_sdate) |>
  mutate(yr= as.integer(format(min_sdate, format="%Y"))) |>
  group_by(id_site, variable_name, yr) |>
  summarise(
    min = min(value), 
    max = max(value), 
    discharge = mean(value), 
    range = max(value) - min(value), 
    .groups = "keep") |>
  add_count(name = "gauge_count") |>
  ungroup()

# rank the sites and year to determine before and after water data
site.water_data <- site.water_data |> 
  group_by(id_site) |>
  mutate(
    rank = rank(yr),
    before_after = ifelse(rank == 1, "before", "after")
  ) |>
  ungroup()

# and just keep a few cols
all_site.water_data <- site.water_data |> select(id_site, before_after, discharge)

# check that site count is equal
print(nrow(site_list) == nrow(all_site.water_data |> distinct(id_site)))

# merge before/after discharge data
flood_data_ba <- flood_data_ba |>
  inner_join(all_site.water_data, by = c("id_site", "before_after"))

# download recruit data
catch.lw <- fetch_project(projects_to_use) |> 
  filter(
    survey_year >= min_yr,
    waterbody %in% !!waterbodies,
    id_site %in% !!site_list$id_site
  ) |>
  collect() |>
  inner_join(
    site.ba_years |> select(id_site, yr, id_project, before_after), 
    by = c("id_site", "survey_year" = "yr", "id_project")
  )  |>
  filter(
    scientific_name %in% sp,
    !is.na(fork_length_cm) | !is.na(length_cm)
  )     

# specify recruit length thresholds
recruit_lengths <- data.frame(
  scientific_name = c(
    "Maccullochella peelii",
    "Maccullochella macquariensis",
    "Macquaria australasica",
    "Gadopsis marmoratus",
    "Cyprinus carpio"
  ),
  recruit_length_cm = c(13, 13, 10, 8, 15)
)

# and add these to the full data set
catch.lw <- catch.lw |>
  select(
    id_site, survey_year, id_project, scientific_name,
    fork_length_cm, length_cm, weight_g, before_after
  ) |>
  left_join(recruit_lengths, by = "scientific_name") |>
  mutate(
    any_length_cm = ifelse(is.na(length_cm), fork_length_cm, length_cm),
    yoy = ifelse(any_length_cm >= recruit_length_cm, "No", "Yes"),
  )

# reformat the species data set to append to the main flood data frame
catch.lw_mc <- reformat_sp_recruit(catch.lw |> filter(scientific_name == "Maccullochella peelii"))
catch.lw_tc <- reformat_sp_recruit(catch.lw |> filter(scientific_name == "Maccullochella macquariensis"))
catch.lw_mp <- reformat_sp_recruit(catch.lw |> filter(scientific_name == "Macquaria australasica"))
catch.lw_bf <- reformat_sp_recruit(catch.lw |> filter(scientific_name == "Gadopsis marmoratus"))
catch.lw_cc <- reformat_sp_recruit(catch.lw |> filter(scientific_name == "Cyprinus carpio"))

# add this info to the full data set
flood_data_ba <- flood_data_ba |>
  mutate(plus1 = NA, yoy = NA)

# merge before/after recruit data
flood_data_ba <- populate_recruit_data(flood_data_ba, catch.lw_mc)
flood_data_ba <- populate_recruit_data(flood_data_ba, catch.lw_tc)
flood_data_ba <- populate_recruit_data(flood_data_ba, catch.lw_mp)
flood_data_ba <- populate_recruit_data(flood_data_ba, catch.lw_bf)
flood_data_ba <- populate_recruit_data(flood_data_ba, catch.lw_cc)

# merge flood impact categories
site.flood_impact <- read.csv("data/site_flood_impact.csv", header = TRUE)
flood_data_ba <- flood_data_ba |>
  inner_join(site.flood_impact, by = c("id_site"))

# check that sites haven't been lost
print(nrow(site_list) == nrow(flood_data_ba |> distinct(id_site)))

# load the data
flood_data_ba <- flood_data_ba |>
  as.data.frame() |>
  mutate(
    id_site = as.factor(id_site),
    waterbody = as.factor(waterbody),
    scientific_name = as.factor(scientific_name)
  )

# reorganise so in right order when getting bayesian outputs 
flood_data_ba <- flood_data_ba |>
  mutate(before_after = factor(before_after, levels=(c("before", "after"))))

# we also need to rescale discharge relative to each waterbody
average_discharge <- flood_data_ba |>
  group_by(waterbody) |>
  summarise(average_discharge = mean(discharge, na.rm = TRUE))

# add this back to the main data set
flood_data_ba <- flood_data_ba |>
  left_join(average_discharge, by = "waterbody") |>
  mutate(discharge_std = discharge / average_discharge)

# and recategorise velocity and blackwater as separate things
flood_data_ba <- flood_data_ba |>
  mutate(
    velocity_rank = ifelse(hypoxia_rank == "V", "H", "L"),
    hypoxia_rank = ifelse(hypoxia_rank == "V", "L", hypoxia_rank)
  )

# refit models if required
files_fitted <- dir("outputs/fitted/")
mods_available <- any(grepl("mod", files_fitted)) &
  any(grepl("yoy", files_fitted)) &
  any(grepl("oneplus", files_fitted))
if (!mods_available | refit_models) {
  
  # model catch as a function of before/after and flood impacts
  mod_flood_impact <- brm(
    brmsformula(
      catch ~ before_after * hypoxia_rank +
        (before_after * hypoxia_rank | scientific_name) +
        (before_after * hypoxia_rank | waterbody) +
        discharge_std +
        (1 | site_name) +
        offset(effort_h),
      shape ~ (1 | waterbody) + (1 | scientific_name)
    ),
    data = flood_data_ba,
    iter = 4000,
    chains = 4,
    cores = 4,
    family = negbinomial()
  )
  
  # repeat for young of year
  mod_flood_impact_yoy <- brm(
    brmsformula(
      yoy ~ before_after * hypoxia_rank +
         (before_after * hypoxia_rank | scientific_name) +
         (before_after * hypoxia_rank | waterbody) +
         discharge_std +
         (1 | site_name) +
         offset(effort_h),
      shape ~ (1 | waterbody) + (1 | scientific_name)
    ),
    data = flood_data_ba,
    iter = 4000,
    chains = 4,
    cores = 4,
    family = negbinomial()
  )

  # save fitted models
  qsave(mod_flood_impact, file = "outputs/fitted/mod-flood-impact.qs")
  qsave(mod_flood_impact_yoy, file = "outputs/fitted/mod-flood-impact-yoy.qs")

} else {
  
  # load fitted models from saved versions
  mod_flood_impact <- qread("outputs/fitted/mod-flood-impact.qs")
  mod_flood_impact_yoy <- qread("outputs/fitted/mod-flood-impact-yoy.qs")

}

# posterior checks
ppc_all <- pp_check(mod_flood_impact, ndraws = 1000)
ppc_yoy <- pp_check(mod_flood_impact_yoy, ndraws = 1000)

# plot these and save to file
ggsave(
  filename = "outputs/figures/ppc-all.png",
  plot = ppc_all + xlim(c(0, 20)) + 
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    ),
  device = ragg::agg_png,
  width = 6,
  height = 5,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/ppc-yoy.png",
  plot = ppc_yoy + xlim(c(0, 20)) + 
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    ),
  device = ragg::agg_png,
  width = 6,
  height = 5,
  units = "in",
  dpi = 600
)

# calculate model fit
fitted_all <- apply(posterior_predict(mod_flood_impact), 2, median)
fitted_yoy <- apply(posterior_predict(mod_flood_impact_yoy), 2, median)
r2 <- c(
  "pearson_all" = cor(mod_flood_impact$data$catch, fitted_all),
  "pearson_yoy" = cor(mod_flood_impact_yoy$data$yoy, fitted_yoy),
  "spearman_all" = cor(mod_flood_impact$data$catch, fitted_all, method = "spearman"),
  "spearman_yoy" = cor(mod_flood_impact_yoy$data$yoy, fitted_yoy, method = "spearman")
)
write.csv(r2, file = "outputs/tables/correlations.csv")

# and plot the predicted impacts by species
flood_impacts <- ggpredict(
  mod_flood_impact, 
  terms = c("before_after", "hypoxia_rank", "scientific_name"),
  type = "random",
  ci.lvl = 0.8
)
flood_impacts_yoy <- ggpredict(
  mod_flood_impact_yoy, 
  terms = c("before_after", "hypoxia_rank", "scientific_name"),
  type = "random",
  ci.lvl = 0.8
)

# work out which species to include for each hypoxia level
spp_to_plot <- flood_data_ba |>
  group_by(scientific_name, hypoxia_rank) |>
  summarise(include = n() >= 5) |>
  ungroup() |>
  complete(scientific_name, hypoxia_rank, fill = list(include = FALSE))
spp_to_plot_yoy <- flood_data_ba |>
  filter(!is.na(yoy)) |>
  group_by(scientific_name, hypoxia_rank) |>
  summarise(include = n() >= 5) |>
  ungroup() |>
  complete(scientific_name, hypoxia_rank, fill = list(include = FALSE))

# species lookup
species_lookup <- c(
  "Bidyanus bidyanus" = "Silver Perch",
  "Cyprinus carpio" = "Common Carp",
  "Gadopsis bispinosus" = "Two-spined Blackfish",
  "Gadopsis marmoratus" = "River Blackfish",
  "Maccullochella macquariensis" = "Trout Cod",
  "Maccullochella peelii" = "Murray Cod",
  "Macquaria ambigua" = "Golden Perch",
  "Macquaria australasica" = "Macquarie Perch",
  "Melanotaenia fluviatilis" = "Murray-Darling Rainbowfish",
  "Perca fluviatilis" = "European Perch",
  "Retropinna semoni" = "Australian Smelt",
  "Salmo trutta" = "Brown Trout"
)

# plot flood impacts
cpue_change <- flood_impacts |>
  as_tibble() |>
  left_join(spp_to_plot, by = c("facet" = "scientific_name", "group" = "hypoxia_rank")) |>
  filter(include) |>
  mutate(
    group = factor(
      group,
      levels = c("L", "M", "H"),
      labels = c("Low", "Medium", "High")
    ),
    x = factor(x, levels = c("before", "after"), labels = c("Before", "After")),
    facet = species_lookup[as.character(facet)]
  )
cpue_plot_all <- ggplot() +
  geom_point(
    data = cpue_change,
    aes(
      x = x, 
      y = predicted, 
      group = group,
      col = group
    ),
    position = position_dodge(0.2),
    size = 3
  ) + 
  geom_line(
    data = cpue_change,
    aes(
      x = x, 
      y = predicted, 
      group = group,
      col = group
    ),
    position = position_dodge(0.2)
  ) +
  geom_point(
    data = flood_data_ba |> 
      mutate(
        group = factor(
          hypoxia_rank,
          levels = c("L", "M", "H"),
          labels = c("Low", "Medium", "High")
        ), 
        facet = species_lookup[as.character(scientific_name)],
        before_after = factor(
          before_after,
          levels = c("before", "after"),
          labels = c("Before", "After")
        ),
        val = catch / effort_h
      ), 
    mapping = aes(
      y = val,
      x = before_after,
      col = group
    ),
    alpha = 0.3,
    position = position_dodge(0.2)
  ) +
  scale_colour_brewer(palette = "Set2", name = "Hypoxia impact") + 
  xlab("Time period") +
  ylab("CPUE") +
  facet_wrap( ~ facet, scales = "free", ncol = 3) +
  theme(legend.position = "bottom")

# repeat for YOY
cpue_change_yoy <- flood_impacts_yoy |>
  as_tibble() |>
  left_join(spp_to_plot, by = c("facet" = "scientific_name", "group" = "hypoxia_rank")) |>
  filter(include) |>
  mutate(
    group = factor(
      group,
      levels = c("L", "M", "H"),
      labels = c("Low", "Medium", "High")
    ),
    x = factor(x, levels = c("before", "after"), labels = c("Before", "After")),
    facet = species_lookup[as.character(facet)]
  )
cpue_plot_yoy <- ggplot() +
  geom_point(
    data = cpue_change_yoy,
    aes(
      x = x, 
      y = predicted, 
      group = group,
      col = group
    ),
    position = position_dodge(0.2),
    size = 3
  ) + 
  geom_line(
    data = cpue_change_yoy,
    aes(
      x = x, 
      y = predicted, 
      group = group,
      col = group
    ),
    position = position_dodge(0.2)
  ) +
  geom_point(
    data = flood_data_ba |> 
      mutate(scientific_name = species_lookup[as.character(scientific_name)]) |>
      filter(scientific_name %in% as.character(cpue_change_yoy$facet)) |>
      mutate(
        group = factor(
          hypoxia_rank,
          levels = c("L", "M", "H"),
          labels = c("Low", "Medium", "High")
        ), 
        facet = scientific_name,
        before_after = factor(
          before_after,
          levels = c("before", "after"),
          labels = c("Before", "After")
        ),
        val = yoy / effort_h
      ), 
    mapping = aes(
      y = val,
      x = before_after,
      col = group
    ),
    alpha = 0.3,
    position = position_dodge(0.2)
  ) +
  scale_colour_brewer(palette = "Set2", name = "Hypoxia impact") + 
  xlab("Time period") +
  ylab("CPUE") +
  facet_wrap( ~ facet, scales = "free", ncol = 3) +
  theme(legend.position = "bottom")

# plot percentage change by species
pc_change <- flood_impacts |> 
  pivot_wider(
    id_cols = c(group, facet),
    values_from = c(predicted, conf.low, conf.high),
    names_from = x
  ) |>
  mutate(
    pc_change = 100 * (predicted_after - predicted_before) / predicted_before
  ) |>
  left_join(spp_to_plot, by = c("facet" = "scientific_name", "group" = "hypoxia_rank")) |>
  filter(include) |>
  mutate(facet = species_lookup[as.character(facet)])
  
pc_plot_all <- pc_change |>
  mutate(
    group = factor(
      group, 
      levels = c("L", "M", "H"),
      labels = c("Low", "Medium", "High")
    )
  ) |>
  ggplot(aes(
    x = facet,
    y = pc_change, 
    fill = group
  )) +
  ylab("Percentage change in CPUE") +
  xlab("Species") +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_brewer(palette = "Set2", name = "Hypoxia impact") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# repeat for YOY
pc_change_yoy <- flood_impacts_yoy |> 
  pivot_wider(
    id_cols = c(group, facet),
    values_from = c(predicted, conf.low, conf.high),
    names_from = x
  ) |>
  mutate(
    pc_change = 100 * (predicted_after - predicted_before) / predicted_before
  ) |>
  left_join(spp_to_plot_yoy, by = c("facet" = "scientific_name", "group" = "hypoxia_rank")) |>
  filter(include) |>
  mutate(facet = species_lookup[as.character(facet)])

pc_plot_yoy <- pc_change_yoy |>
  mutate(
    group = factor(
      group, 
      levels = c("L", "M", "H"),
      labels = c("Low", "Medium", "High")
    )
  ) |>
  ggplot(aes(
    x = facet,
    y = pc_change, 
    fill = group
  )) +
  ylab("Percentage change in CPUE") +
  xlab("Species") +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_brewer(palette = "Set2", name = "Hypoxia impact") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# save figs
ggsave(
  filename = "outputs/figures/cpue-all.png",
  plot = cpue_plot_all,
  device = ragg::agg_png,
  width = 8,
  height = 10,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/cpue-yoy.png",
  plot = cpue_plot_yoy,
  device = ragg::agg_png,
  width = 8,
  height = 5,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/percent-change-all.png",
  plot = pc_plot_all,
  device = ragg::agg_png,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/percent-change-yoy.png",
  plot = pc_plot_yoy,
  device = ragg::agg_png,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)

# repeat these plots but add waterbody info
# and plot the predicted impacts by species
flood_impacts_spatial <- ggpredict(
  mod_flood_impact, 
  terms = c("before_after", "hypoxia_rank", "scientific_name", "waterbody"),
  type = "random",
  ci.lvl = 0.8
)
flood_impacts_yoy_spatial <- ggpredict(
  mod_flood_impact_yoy, 
  terms = c("before_after", "hypoxia_rank", "scientific_name", "waterbody"),
  type = "random",
  ci.lvl = 0.8
)

# for all species
spp_to_plot_spatial <- flood_data_ba |>
  group_by(scientific_name, waterbody, hypoxia_rank) |>
  summarise(include = n() >= 5) |>
  ungroup() |>
  complete(scientific_name, waterbody, hypoxia_rank, fill = list(include = FALSE))
pc_change_spatial <- flood_impacts_spatial |> 
  pivot_wider(
    id_cols = c(group, facet, panel),
    values_from = c(predicted, conf.low, conf.high),
    names_from = x
  ) |>
  mutate(
    pc_change = 100 * (predicted_after - predicted_before) / predicted_before
  ) |>
  left_join(
    spp_to_plot_spatial,
    by = c("facet" = "scientific_name", "panel" = "waterbody", "group" = "hypoxia_rank")
  ) |>
  filter(include) |>
  mutate(facet = species_lookup[as.character(facet)])
pc_plot_spatial_all <- pc_change_spatial |>
  mutate(
    group = factor(
      group, 
      levels = c("L", "M", "H"),
      labels = c("Low", "Medium", "High")
    )
  ) |>
  ggplot(aes(
    x = facet,
    y = pc_change, 
    fill = group
  )) +
  ylab("Percentage change in CPUE") +
  xlab("Species") +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_brewer(palette = "Set2", name = "Hypoxia impact") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap( ~ panel, nrow = 6)

# and for yoy
spp_to_plot_spatial_yoy <- flood_data_ba |>
  filter(!is.na(yoy)) |>
  group_by(scientific_name, waterbody, hypoxia_rank) |>
  summarise(include = sum(yoy) > 1) |>
  ungroup() |>
  complete(scientific_name, waterbody, hypoxia_rank, fill = list(include = FALSE))
pc_change_spatial_yoy <- flood_impacts_yoy_spatial |> 
  pivot_wider(
    id_cols = c(group, facet, panel),
    values_from = c(predicted, conf.low, conf.high),
    names_from = x
  ) |>
  mutate(
    pc_change = 100 * (predicted_after - predicted_before) / predicted_before
  ) |>
  left_join(
    spp_to_plot_spatial_yoy,
    by = c("facet" = "scientific_name", "panel" = "waterbody", "group" = "hypoxia_rank")
  ) |>
  filter(include) |>
  mutate(facet = species_lookup[as.character(facet)])
pc_plot_spatial_yoy <- pc_change_spatial_yoy |>
  mutate(
    group = factor(
      group, 
      levels = c("L", "M", "H"),
      labels = c("Low", "Medium", "High")
    )
  ) |>
  ggplot(aes(
    x = facet,
    y = pc_change, 
    fill = group
  )) +
  ylab("Percentage change in CPUE") +
  xlab("Species") +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_brewer(palette = "Set2", name = "Hypoxia impact") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap( ~ panel, nrow = 6)

# alternative, grouped by hypoxia level
pc_plot_spatial_all_v2a <- pc_change_spatial |>
  mutate(
    group = factor(
      group, 
      levels = c("L", "M", "H"),
      labels = c("Low", "Medium", "High")
    )
  ) |>
  filter(facet %in% c("Common Carp", "European Perch", "Brown Trout")) |>
  ggplot(aes(
    x = panel,
    y = pc_change, 
    fill = facet
  )) +
  ylab("Percentage change in CPUE") +
  xlab("Waterbody") +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_brewer(palette = "Set2", name = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap( ~ group, nrow = 3, scales = "free")
pc_plot_spatial_all_v2b <- pc_change_spatial |>
  mutate(
    group = factor(
      group, 
      levels = c("L", "M", "H"),
      labels = c("Low", "Medium", "High")
    )
  ) |>
  filter(!facet %in% c("Common Carp", "European Perch", "Brown Trout")) |>
  ggplot(aes(
    x = panel,
    y = pc_change, 
    fill = facet
  )) +
  ylab("Percentage change in CPUE") +
  xlab("Waterbody") +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_brewer(palette = "Set2", name = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap( ~ group, nrow = 3, scales = "free")
pc_plot_spatial_yoy_v2a <- pc_change_spatial_yoy |>
  mutate(
    group = factor(
      group, 
      levels = c("L", "M", "H"),
      labels = c("Low", "Medium", "High")
    )
  ) |>
  filter(facet %in% c("Common Carp", "European Perch", "Brown Trout")) |>
  ggplot(aes(
    x = panel,
    y = pc_change, 
    fill = facet
  )) +
  ylab("Percentage change in CPUE") +
  xlab("Waterbody") +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_brewer(palette = "Set2", name = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  facet_wrap( ~ group, nrow = 3, scales = "free")
pc_plot_spatial_yoy_v2b <- pc_change_spatial_yoy |>
  mutate(
    group = factor(
      group, 
      levels = c("L", "M", "H"),
      labels = c("Low", "Medium", "High")
    )
  ) |>
  filter(!facet %in% c("Common Carp", "European Perch", "Brown Trout")) |>
  ggplot(aes(
    x = panel,
    y = pc_change, 
    fill = facet
  )) +
  ylab("Percentage change in CPUE") +
  xlab("Waterbody") +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_brewer(palette = "Set2", name = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap( ~ group, nrow = 3, scales = "free")

# save to file
ggsave(
  filename = "outputs/figures/percent-change-by-waterbody-all.png",
  plot = pc_plot_spatial_all,
  device = ragg::agg_png,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/percent-change-by-waterbody-yoy.png",
  plot = pc_plot_spatial_yoy,
  device = ragg::agg_png,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/percent-change-by-waterbody-all-v2-exotic.png",
  plot = pc_plot_spatial_all_v2a,
  device = ragg::agg_png,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/percent-change-by-waterbody-yoy-v2-exotic.png",
  plot = pc_plot_spatial_yoy_v2a,
  device = ragg::agg_png,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/percent-change-by-waterbody-all-v2-native.png",
  plot = pc_plot_spatial_all_v2b,
  device = ragg::agg_png,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600
)
ggsave(
  filename = "outputs/figures/percent-change-by-waterbody-yoy-v2-native.png",
  plot = pc_plot_spatial_yoy_v2b,
  device = ragg::agg_png,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600
)

# write a table of waterbody effects
percent_change_table_all <- pc_change_spatial |> 
  as.data.frame() |>
  rename(
    hypoxia_impact = group,
    species = facet,
    waterbody = panel,
    recorded = include,
    cpue_before = predicted_before,
    cpue_after = predicted_after,
    percent_change = pc_change
  ) |>
  mutate(
    hypoxia_impact = factor(hypoxia_impact, levels = c("L", "M", "H"), labels = c("Low", "Medium", "High")),
    stage = "All"
  ) |>
  select(species, waterbody, hypoxia_impact, stage, cpue_before, cpue_after, percent_change, recorded) |>
  complete(species, waterbody, hypoxia_impact, stage, fill = list(cpue_before = NA, cpue_after = NA, percent_change = NA, recorded = FALSE)) 
percent_change_table_yoy <- pc_change_spatial_yoy |> 
  as.data.frame() |>
  rename(
    hypoxia_impact = group,
    species = facet,
    waterbody = panel,
    recorded = include,
    cpue_before = predicted_before,
    cpue_after = predicted_after,
    percent_change = pc_change
  ) |>
  mutate(
    hypoxia_impact = factor(hypoxia_impact, levels = c("L", "M", "H"), labels = c("Low", "Medium", "High")),
    stage = "Recruits"
  ) |>
  select(species, waterbody, hypoxia_impact, stage, cpue_before, cpue_after, percent_change, recorded) |>
  complete(species, waterbody, hypoxia_impact, stage, fill = list(cpue_before = NA, cpue_after = NA, percent_change = NA, recorded = FALSE)) 

# combine these and write two versions (one with and one without NAs)
percent_change_table <- bind_rows(percent_change_table_all, percent_change_table_yoy) |>
  arrange(species, waterbody, hypoxia_impact, stage)
write.csv(percent_change_table, file = "outputs/tables/percent_change_all_categories.csv")
write.csv(percent_change_table |> filter(recorded), file = "outputs/tables/percent_change_observed_categories.csv")

# present this info in a map
watercourse_lm <- fetch_table("hy_watercourse_lm", schema = "spatial", collect = TRUE)
# spatial_cma <- fetch_table("cma100", schema = "spatial", collect = TRUE)
spatial_basin <- fetch_table("basin100", schema = "spatial", collect = TRUE)

# and basic outlines of Vic/Aus  
vic_outline <- fetch_table("victoria_outline", schema = "spatial", collect = TRUE)

# get fish site info
survey_coords <- flood_data_ba |> fetch_site_info() |> collect()

# load blackwater layer
bw_spatial <- sf::read_sf("data/blackwater_areas/blackwater_areas.shp")

# set geometries properly
st_geometry(watercourse_lm) <- st_as_sfc(watercourse_lm$geom, crs = 4283)
# st_geometry(spatial_cma) <- st_as_sfc(spatial_cma$geom, crs = 4283)
st_geometry(spatial_basin) <- st_as_sfc(spatial_basin$geom, crs = 4283)
st_geometry(vic_outline) <- st_as_sfc(vic_outline$geom, crs = 4283)
st_geometry(survey_coords) <- st_as_sfc(survey_coords$geom_pnt, crs = 4283)

# filter geofab to Victoria with a 5 km buffer
vic_poly <- st_polygonize(vic_outline)
vic_buffered <- vic_poly %>%
  st_transform(7899) %>%
  st_buffer(dist = 5000) %>%
  st_transform(4283)
watercourse_vic <- watercourse_lm %>%
  st_filter(vic_buffered, .predicate = st_intersects)

# add hypoxia ranks to survey coords so we can match pc_change with each site
sb_native <- c(
  "Melanotaenia fluviatilis",
  "Retropinna semoni"
)
lb_native <- c(
  "Bidyanus bidyanus",
  "Maccullochella peelii",
  "Macquaria ambigua",
  "Macquaria australasica",
  "Maccullochella macquariensis",
  "Gadopsis bispinosis",
  "Gadopsis marmoratus"
)
pc_change_map <- survey_coords |>
  mutate(id_site = as.integer(id_site)) |>
  left_join(
    flood_data_ba |> 
      distinct(id_site, hypoxia_rank) |>
      mutate(id_site = as.integer(as.character(id_site))),
    by = "id_site"
  ) |>
  left_join(
    pc_change_spatial |> 
      filter(include) |> 
      select(group, facet, panel, pc_change),
    by = c("hypoxia_rank" = "group", "waterbody" = "panel"),
    relationship = "many-to-many"
  ) |>
  mutate(
    species_group = as.character(facet),
    species_group = ifelse(species_group %in% sb_native, "Small-bodied native", species_group),
    species_group = ifelse(species_group %in% lb_native, "Large-bodied native", species_group)
  )

# average over all species in each group
pc_change_map <- pc_change_map |>
  select(waterbody, id_site, geometry, hypoxia_rank, species_group, pc_change) |>
  group_by(waterbody, id_site, geometry, hypoxia_rank, species_group) |>
  summarise(pc_change = mean(pc_change, na.rm = TRUE)) |>
  ungroup()

# plot all sites
watercourse_surveyed <- watercourse_vic |>
  filter(
    grepl(
      paste(
        pc_change_map |>
          mutate(waterbody = gsub(" River| Creek| Creeks| Lagoon| Swamp| Lake", "", waterbody)) |>
          pull(waterbody) |>
          unique(), 
        collapse = "|"
      ),
      name, 
      ignore.case = TRUE
    )
  )
pc_change_map_lb_native <- vic_outline %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf(data = spatial_basin) + 
  geom_sf(data = watercourse_surveyed, size = 1) +
  geom_sf(data = bw_spatial, col = "#276419", fill = "#276419", alpha = 0.5) +
  geom_sf(
    data = pc_change_map |> 
      filter(species_group == "Large-bodied native") |>
      mutate(
        scaled_pc_change = abs(pc_change),
        scaled_pc_change = scaled_pc_change - min(scaled_pc_change, na.rm = TRUE),
        scaled_pc_change = scaled_pc_change / max(scaled_pc_change, na.rm = TRUE),
        scaled_pc_change = 0.9 + (scaled_pc_change / 5)
      ),
    aes(fill = pc_change, size = scaled_pc_change),
    shape = 21, 
  ) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(
    location = "bl", 
    which_north = "true", 
    pad_x = unit(0.05, "in"), 
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  scale_fill_gradientn(
    colours = c(
      "#67001F",
      "#B2182B",
      "#D6604D",
      # "#F4A582",
      # "#FDDBC7",
      "#F7F7F7",
      "#D1E5F0",
      "#92C5DE",
      "#4393C3",
      "#2166AC",
      "#053061"
    ), 
    values = scales::rescale(c(-100, -50, -10, 0, 50, 100, 300, 500, 1000)),
    name = "Percentage change in CPUE",
    limits = c(-100, 1000)
  ) +
  scale_size(name = "", guide = "none") +
  theme(legend.position = "bottom")
pc_change_map_sb_native <- vic_outline %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf(data = spatial_basin) + 
  geom_sf(data = watercourse_surveyed, size = 1) +
  geom_sf(data = bw_spatial, col = "#276419", fill = "#276419", alpha = 0.5) +
  geom_sf(
    data = pc_change_map |> 
      filter(species_group == "Small-bodied native") |>
      mutate(
        scaled_pc_change = abs(pc_change),
        scaled_pc_change = scaled_pc_change - min(scaled_pc_change, na.rm = TRUE),
        scaled_pc_change = scaled_pc_change / max(scaled_pc_change, na.rm = TRUE),
        scaled_pc_change = 0.9 + (scaled_pc_change / 5)
      ),
    aes(fill = pc_change, size = scaled_pc_change),
    shape = 21, 
  ) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(
    location = "bl", 
    which_north = "true", 
    pad_x = unit(0.05, "in"), 
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  scale_fill_gradientn(
    colours = c(
      "#67001F",
      "#B2182B",
      "#D6604D",
      # "#F4A582",
      # "#FDDBC7",
      "#F7F7F7",
      "#D1E5F0",
      "#92C5DE",
      "#4393C3",
      "#2166AC",
      "#053061"
    ), 
    values = scales::rescale(c(-100, -50, -10, 0, 50, 100, 300, 500, 1000)),
    name = "Percentage change in CPUE",
    limits = c(-100, 1000)
  ) +
  scale_size(name = "", guide = "none") +
  theme(legend.position = "bottom")
pc_change_map_carp <- vic_outline %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf(data = spatial_basin) + 
  geom_sf(data = watercourse_surveyed, size = 1) +
  geom_sf(data = bw_spatial, col = "#276419", fill = "#276419", alpha = 0.5) +
  geom_sf(
    data = pc_change_map |> 
      filter(species_group == "Cyprinus carpio") |>
      mutate(
        scaled_pc_change = abs(pc_change),
        scaled_pc_change = scaled_pc_change - min(scaled_pc_change, na.rm = TRUE),
        scaled_pc_change = scaled_pc_change / max(scaled_pc_change, na.rm = TRUE),
        scaled_pc_change = 0.9 + (scaled_pc_change / 5)
      ),
    aes(fill = pc_change, size = scaled_pc_change),
    shape = 21, 
  ) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(
    location = "bl", 
    which_north = "true", 
    pad_x = unit(0.05, "in"), 
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  scale_fill_gradientn(
    colours = c(
      "#67001F",
      "#B2182B",
      "#D6604D",
      # "#F4A582",
      # "#FDDBC7",
      "#F7F7F7",
      "#D1E5F0",
      "#92C5DE",
      "#4393C3",
      "#2166AC",
      "#053061"
    ), 
    values = scales::rescale(c(-100, -50, -10, 0, 50, 100, 300, 500, 1000)),
    name = "Percentage change in CPUE",
    limits = c(-100, 1000)
  ) +
  scale_size(name = "", guide = "none") +
  theme(legend.position = "bottom")

# save to file
ggsave(
  file = "outputs/figures/mapped-pc-change-lb-native.png",
  plot = pc_change_map_lb_native,
  device = ragg::agg_png,
  width = 8,
  height = 8,
  units = "in",
  res = 600
)
ggsave(
  file = "outputs/figures/mapped-pc-change-sb-native.png",
  plot = pc_change_map_sb_native,
  device = ragg::agg_png,
  width = 8,
  height = 8,
  units = "in",
  res = 600
)
ggsave(
  file = "outputs/figures/mapped-pc-change-carp.png",
  plot = pc_change_map_carp,
  device = ragg::agg_png,
  width = 8,
  height = 8,
  units = "in",
  res = 600
)

