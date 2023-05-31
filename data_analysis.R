################
####=================== Data analysis ======================


###==================== full model 

library(aae.db)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggplot2)
library(tibble)
library(lme4)
library(car)
library(effects)
library(AICcmodavg)
library(bbmle)
library(corrplot)
library(Hmisc)
library(lmerTest)
library(rstanarm)
library(ggeffects)


#read in data as dataframe 
flood_data_ba <- as.data.frame(flood_data_ba)
str(flood_data_ba)
flood_data_ba$id_site = as.factor(flood_data_ba$id_site)
flood_data_ba$waterbody =as.factor(flood_data_ba$waterbody)
flood_data_ba$scientific_name = as.factor(flood_data_ba$scientific_name)
flood_data_ba$id_site = as.factor(flood_data_ba$id_site)

#duplicate data in case 
flood_data_ba_wide<- flood_data_ba


### Need to convert the flood_data_ba data set into long format
flood_data_ba <- flood_data_ba |>
  pivot_longer(cols = c(contains("before_"), contains("after_"))) |>
  mutate(
    before_after = sapply(strsplit(name, "_"), \(x) x[[1]]),
    variable = sapply(strsplit(name, "_"), \(x) x[[2]])
  ) |>
  pivot_wider(
    id_cols = c(id_site, site_name, waterbody, id_project, scientific_name, impact_type, hypoxia_rank, before_after),
    names_from = variable,
    values_from = value
  )

#reorganise so in right order when getting bayesian outputs 
flood_data_ba <- flood_data_ba|>
  mutate(before_after = factor(before_after, levels=(c("before", "after"))))


## but we also need to rescale discharge relative to each waterbody
average_discharge <- flood_data_ba |>
  group_by(waterbody) |>
  summarise(average_discharge = mean(discharge, na.rm = TRUE))

# add this back to the main data set
flood_data_ba <- flood_data_ba |>
  left_join(average_discharge, by = "waterbody") |>
  mutate(
    discharge_std = discharge / average_discharge,
    log_cpue_p1 = log(cpue + 1)
    )

#first test model 
flood_model_lmer_ba <- lmerTest::lmer(
  log_cpue_p1 ~ before_after * hypoxia_rank * scientific_name +
    discharge_std +
    (1 | site_name),
  data = flood_data_ba)

summary(flood_model_lmer_ba)
#problem with rank deficient, need to do something about zero's in data
#may need to use Bayesian model structure 
plot(flood_model_lmer_ba)
flood_model_lmer_aov<-Anova(flood_model_lmer_ba, "III")
flood_model_lmer_aov


#bayesian model attempt

##TAKES TIME TO RUN

#cpue
flood_model_stanlmer_jian <- stan_lmer(
  log_cpue_p1 ~ before_after * hypoxia_rank +
    (before_after * hypoxia_rank | scientific_name) +
    discharge_std +
    (1 | waterbody) + (1 | site_name),
  data = flood_data_ba)

summary(flood_model_stanlmer_jian)

plot(flood_model_stanlmer_jian, regex_pars = "after:hypoxia")

dat <- ggpredict(flood_model_stanlmer_jian, terms = c("before_after", "hypoxia_rank", "scientific_name"))
plot(dat, facet = TRUE, add.data = TRUE, connect.lines = TRUE)


##yoy


flood_model_stanlmer_yoy <- stan_lmer(
  yoy ~ before_after * hypoxia_rank +
    (before_after * hypoxia_rank | scientific_name) +
    discharge_std +
    (1 | waterbody) + (1 | site_name),
  data = flood_data_ba)

summary(flood_model_stanlmer_yoy)

plot(flood_model_stanlmer_yoy, regex_pars = "after:hypoxia")


## just adults 

flood_model_stanlmer_adult <- stan_lmer(
  `1plus` ~ before_after * hypoxia_rank +
    (before_after * hypoxia_rank | scientific_name) +
    discharge_std +
    (1 | waterbody) + (1 | site_name),
  data = flood_data_ba)

summary(flood_model_stanlmer_adult)

plot(flood_model_stanlmer_adult, regex_pars = "after:hypoxia")



