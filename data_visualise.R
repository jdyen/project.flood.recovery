

###======== DATA VISUALISATION ===========

## Plots looking at the raw data 

#cpue 
cpueplot<-flood_data_ba |>
  ggplot(aes(y = cpue, x = hypoxia_rank, fill = before_after)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap( ~ scientific_name, scales = "free")+
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.text.y = element_blank(), legend.position = "right") +
  theme_bw()

cpueplot

#same scales across all species  
cpueplot2<-flood_data_ba |>
  ggplot(aes(y = cpue, x = hypoxia_rank, fill = before_after)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap( ~ scientific_name, scales = "fixed")+
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.text.y = element_blank(), legend.position = "right") +
  theme_bw()

cpueplot2

#yoy
yoyplot<-flood_data_ba |>
  ggplot(aes(y = yoy, x = hypoxia_rank, fill = before_after)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap( ~ scientific_name, scales = "free")+
  theme_bw()

yoyplot

#1 year olds and older 
plus1plot<-flood_data_ba |>
  ggplot(aes(y = `1plus`, x = hypoxia_rank, fill = before_after)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap( ~ scientific_name, scales = "free")+
  theme_bw()

plus1plot


#######boxplots 

#simple cpue boxplot, specified with scientific name 
cpueboxplot<- ggplot(flood_data_ba, aes(scientific_name, cpue)) + 
  geom_boxplot() + 
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.text.y = element_blank(), legend.position = "right") +
  facet_wrap(vars(waterbody), scales = "free")

cpueboxplot

#boxplot using transformed data and before and after, grouped by hypoxia rank 

cpuelogboxplot<-ggplot(flood_data_ba, aes(y=log_cpue_p1, x=hypoxia_rank, fill=before_after)) + 
  geom_boxplot() + 
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.text.y = element_blank(), legend.position = "right") +
  facet_wrap(vars(waterbody), scales = "free")

cpuelogboxplot


