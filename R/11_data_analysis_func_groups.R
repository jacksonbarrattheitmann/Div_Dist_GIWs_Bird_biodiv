# Data analysis total richness, by functional group

# packages
library(readr)
library(dplyr)
library(tidyverse)
library(data.table)
library(lattice)
library(vegan)
library(lme4)
library(performance)
library(easystats)
library(see)
library(ggplot2)
library(patchwork)
library(marginaleffects)
library(margins)
library(GGally)
library(patchwork)
library(ggpubr)
library(arm)
library(broom.mixed)

# reading in the total richness dfs and env matrix
s_matrix <- readRDS("Data/species_matrix/s_matrix.RDS")
# subsetting to remove columns that we haven't calculated yet for functional groups
s_matrix <- s_matrix[, 1:6]

env_matrix <- readRDS("Data/earth_engine_env_data/env_matrix.RDS")

# Now all the functional tables
ins <- readRDS("Data/species_matrix/insessorial_matrix.RDS")

aqu <- readRDS("Data/species_matrix/aquatic_matrix.RDS")

gen <- readRDS("Data/species_matrix/generalist_matrix.RDS")

aer <- readRDS("Data/species_matrix/aerial_matrix.RDS")

ter <- readRDS("Data/species_matrix/terrestrial_matrix.RDS")

histogram(aer$total_richness)

# combining them all together 
big_matrix <- rbind(s_matrix, ins, aqu, gen, aer, ter)


# functional groups and env matrix
functional_bird_matrix <- big_matrix %>%
  left_join(env_matrix, by = "LOCALITY_ID")

functional_bird_matrix <- as.data.frame(functional_bird_matrix)

#### Now breaking them back into each of their separate lifestyle groups

# Generalist
gen <- gen %>%
  left_join(env_matrix, by = "LOCALITY_ID")

# Insessorial
ins <- ins %>%
  left_join(env_matrix, by = "LOCALITY_ID")

# Aquatic
aqu <- aqu %>%
  left_join(env_matrix, by = "LOCALITY_ID")

# Aerial
aer <- aer %>%
  left_join(env_matrix, by = "LOCALITY_ID")

# Terrestrial
ter <-ter %>%
  left_join(env_matrix, by = "LOCALITY_ID")


functional_bird_matrix$group <- as.factor(functional_bird_matrix$group)

functional_bird_matrix <- rename(functional_bird_matrix, "Primary Lifestyle" = group)

# Area Species plot, broken down by functional lifestyle

area_plot <- ggplot(data = functional_bird_matrix, aes(x = area_sqkm, y = total_richness, color = `Primary Lifestyle`)) +
  geom_point() + geom_smooth(method = 'lm') + 
  scale_color_discrete(breaks=c('All Species', 'aerial', 'aquatic', 'generalist', 'insessorial', 'terrestrial'),
                       labels = c("All Species", "Aerial", "Aquatic", "Generalist", "Insessorial", "Terrestrial")) +
  labs(fill = "Primary Lifestyle") + xlab("Area (km2)") + ylab("Species Richness") +
  theme_bw() +
  theme(legend.position = c(0.1, 0.82), 
        legend.background = element_rect(fill ="transparent")) +
  scale_x_log10() +
  coord_cartesian(expand = FALSE)


area_plot

ggsave("Spp_area_plot.png", bg = "transparent")

######## Making models by functional group for the SAR
# Using the rescaled to stay consistent with previosu models for total richness

######## Generalist ############

# Area and checklists
mod_gen_rescaled <- glm(total_richness ~ rescale(log10(area_sqkm)) + rescale(log10(total_checklists)), family=poisson, data = gen)
summary(mod_gen_rescaled)  

check_model(mod_gen_rescaled)


# Land Cover 25

big_mod_glm_gen <- glmer(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_25km) +
                              rescale(grass_wet) + rescale(grass_25km) + rescale(trees_wet) + rescale(trees_25km) +
                              rescale(water_wet) + rescale(water_25km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_25km)
                            + rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_25km) + rescale(shan_wet) +
                              rescale(shan_gamma_25) + (1| NA_L1NAME), family=poisson, data=gen)

summary(big_mod_glm_gen)

check_model(big_mod_glm_gen)
# make a plot...
big_mod_summary_glm_gen <- broom.mixed::tidy(big_mod_glm_gen, conf.int=TRUE) %>%
  dplyr::filter(effect=="fixed") %>%
  mutate(predictor=c("Intercept", "Total checklists", 
                     "Built local", "Built 25km",
                     "Grassland local", "Grassland 25km", 
                     "Trees local", "Trees 25km",
                     "Water local", "Water 25km",
                     "Flooded Veg local", "Flooded Veg 25km",
                     "Shrub local", "Shrub 25km",
                     "Heterogeneity local", "Heterogeneity 25km")) %>%
  mutate(Scale=c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape",
                 "Local", "Landscape", "Local", "Landscape", "Local", "Landscape")) %>%
  mutate(model = "Model 25km")

gen_plot <- big_mod_summary_glm_gen %>%
  dplyr::filter(! predictor %in% c("Intercept", "Total checklists")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x =element_text(color="black"), 
        axis.text.y = element_text(color = c("red", "black", "black", "black",
                                             "black", "black", "black", "black"
                                             ,"black", "black", "black", "black",
                                             "black", "black")))+
  ylab("Effect size")+
  xlab("")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")+
  ggtitle("Generalist")

gen_plot



######## Aquatic ##########

mod_aqu_rescaled <- glm(total_richness ~ rescale(log10(area_sqkm)) + rescale(log10(total_checklists)), family=poisson, data = aqu)
summary(mod_aqu_rescaled)  

check_model(mod_aqu_rescaled)

## Land cover impacts 

big_mod_glm_aqu <- glmer(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_25km) +
                           rescale(grass_wet) + rescale(grass_25km) + rescale(trees_wet) + rescale(trees_25km) +
                           rescale(water_wet) + rescale(water_25km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_25km)
                         + rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_25km) + rescale(shan_wet) +
                           rescale(shan_gamma_25) + (1| NA_L1NAME), family=poisson, data=aqu)

summary(big_mod_glm_aqu)

check_model(big_mod_glm_aqu)

# make a plot...
big_mod_summary_glm_aqu <- broom.mixed::tidy(big_mod_glm_aqu, conf.int=TRUE) %>%
  dplyr::filter(effect == "fixed") %>%
  mutate(predictor=c("Intercept", "Total checklists", 
                     "Built local", "Built 25km",
                     "Grassland local", "Grassland 25km", 
                     "Trees local", "Trees 25km",
                     "Water local", "Water 25km",
                     "Flooded Veg local", "Flooded Veg 25km",
                     "Shrub local", "Shrub 25km",
                     "Heterogeneity local", "Heterogeneity 25km")) %>%
  mutate(Scale=c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape",
                 "Local", "Landscape", "Local", "Landscape", "Local", "Landscape")) %>%
  mutate(model = "Model 25km")

aqu_plot <- big_mod_summary_glm_aqu %>%
  dplyr::filter(! predictor %in% c("Intercept", "Total checklists")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x =element_text(color="black"), 
        axis.text.y = element_text(color = c("red", "red", "red", "black",
                                             "red", "blue", "blue", "black",
                                             "black", "black", "red", "black",
                                             "black", "blue")))+
  ylab("Effect size")+
  xlab("")+
  theme(legend.position = "none")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")+
  ggtitle("Aquatic")

aqu_plot

######## Insessorial

mod_ins_rescaled <- glm(total_richness ~ rescale(log10(area_sqkm)) + rescale(log10(total_checklists)), family=poisson, data = ins)
summary(mod_ins_rescaled)  

check_model(mod_ins_rescaled)

## Land cover impacts 

big_mod_glm_ins <- glmer(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_25km) +
                           rescale(grass_wet) + rescale(grass_25km) + rescale(trees_wet) + rescale(trees_25km) +
                           rescale(water_wet) + rescale(water_25km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_25km)
                         + rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_25km) + rescale(shan_wet) +
                           rescale(shan_gamma_25) + (1| NA_L1NAME), family=poisson, data=ins)

summary(big_mod_glm_ins)

check_model(big_mod_glm_ins)
# make a plot...
big_mod_summary_glm_ins <- broom.mixed::tidy(big_mod_glm_ins, conf.int=TRUE) %>%
  dplyr::filter(effect=="fixed") %>%
  mutate(predictor=c("Intercept", "Total checklists", 
                     "Built local", "Built 25km",
                     "Grassland local", "Grassland 25km", 
                     "Trees local", "Trees 25km",
                     "Water local", "Water 25km",
                     "Flooded Veg local", "Flooded Veg 25km",
                     "Shrub local", "Shrub 25km",
                     "Heterogeneity local", "Heterogeneity 25km")) %>%
  mutate(Scale=c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape",
                 "Local", "Landscape", "Local", "Landscape", "Local", "Landscape")) %>%
  mutate(model = "Model 25km")

ins_plot <- big_mod_summary_glm_ins %>%
  dplyr::filter(! predictor %in% c("Intercept", "Total checklists")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x =element_text(color="black"), 
        axis.text.y = element_text(color = c("red", "black", "black", "black",
                                             "black", "black", "black", "black",
                                             "black", "black", "black", "blue",
                                             "black", "black")))+
  ylab("Effect size")+
  xlab("")+
  theme(legend.position = "none")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")+
  ggtitle("Insessorial")

ins_plot

####### Terrestrial
mod_ter_rescaled <- glm(total_richness ~ rescale(log10(area_sqkm)) + rescale(log10(total_checklists)), family=poisson, data = ter)
summary(mod_ter_rescaled)  

check_model(mod_ter_rescaled)


## Land cover impacts 

big_mod_glm_ter <- glmer(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_25km) +
                           rescale(grass_wet) + rescale(grass_25km) + rescale(trees_wet) + rescale(trees_25km) +
                           rescale(water_wet) + rescale(water_25km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_25km)
                         + rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_25km) + rescale(shan_wet) +
                           rescale(shan_gamma_25) + (1| NA_L1NAME), family=poisson, data=ter)

summary(big_mod_glm_ter)

check_model(big_mod_glm_ter)

qqmath(big_mod_glm_ter)

# make a plot...
big_mod_summary_glm_ter <- broom.mixed::tidy(big_mod_glm_ter, conf.int=TRUE) %>%
  dplyr::filter(effect == "fixed") %>%
  mutate(predictor=c("Intercept", "Total checklists", 
                     "Built local", "Built 25km",
                     "Grassland local", "Grassland 25km", 
                     "Trees local", "Trees 25km",
                     "Water local", "Water 25km",
                     "Flooded Veg local", "Flooded Veg 25km",
                     "Shrub local", "Shrub 25km",
                     "Heterogeneity local", "Heterogeneity 25km")) %>%
  mutate(Scale=c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape",
                 "Local", "Landscape", "Local", "Landscape", "Local", "Landscape")) %>%
  mutate(model = "Model 25km")

ter_plot <- big_mod_summary_glm_ter %>%
  dplyr::filter(! predictor %in% c("Intercept", "Total checklists")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x =element_text(color="black"), 
        axis.text.y = element_text(color = c("red", "red", "black", "blue",
                                             "blue", "black", "red", "black",
                                             "black", "black", "red", "red",
                                             "black", "black")))+
  ylab("Effect size")+
  xlab("")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")+
  ggtitle("Terrestrial")

ter_plot

####### Aerial
mod_aer_rescaled <- glm(total_richness ~ rescale(log10(area_sqkm)) + rescale(log10(total_checklists)), family=poisson, data = aer)
summary(mod_aer_rescaled)  

check_model(mod_aer_rescaled)

## Land cover impacts 

big_mod_glm_aer <- glmer(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_25km) +
                           rescale(grass_wet) + rescale(grass_25km) + rescale(trees_wet) + rescale(trees_25km) +
                           rescale(water_wet) + rescale(water_25km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_25km)
                         + rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_25km) + rescale(shan_wet) +
                           rescale(shan_gamma_25) + (1| NA_L1NAME), family=poisson, data=aer)

summary(big_mod_glm_aer)

check_model(big_mod_glm_aer)
# make a plot...
big_mod_summary_glm_aer <- broom.mixed::tidy(big_mod_glm_aer, conf.int=TRUE) %>%
  dplyr::filter(effect == "fixed") %>%
  mutate(predictor=c("Intercept", "Total checklists", 
                     "Built local", "Built 25km",
                     "Grassland local", "Grassland 25km", 
                     "Trees local", "Trees 25km",
                     "Water local", "Water 25km",
                     "Flooded Veg local", "Flooded Veg 25km",
                     "Shrub local", "Shrub 25km",
                     "Heterogeneity local", "Heterogeneity 25km")) %>%
  mutate(Scale=c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape",
                 "Local", "Landscape", "Local", "Landscape", "Local", "Landscape")) %>%
  mutate(model = "Model 25km")

aer_plot <- big_mod_summary_glm_aer %>%
  dplyr::filter(! predictor %in% c("Intercept", "Total checklists")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x =element_text(color="black"), 
        axis.text.y = element_text(color = c("red", "black", "black", "black",
        "black", "black", "black", "black","black", "black", "red", "black","black", "black")))+
  ylab("Effect size")+
  xlab("")+
  theme(legend.position = "none")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")+
  ggtitle("Aerial")

aer_plot 


######## Final mixed effect model plot with marginal effects
ggarrange(aer_plot, aqu_plot, gen_plot, ins_plot, ter_plot,
         labels = c("A", "B", "C", "D", "E"),
         ncol = 2, nrow = 3, common.legend = TRUE)







########################## SUPPLEMENTAY INFO ################

# Creating functional matrix table for species

func_tbl <- readRDS("Data/species_matrix/functional_traits_species_matrix.RDS")

 func_tbl <- as.data.frame(func_tbl)

list_spp_func <- func_tbl %>%
  dplyr::select(COMMON_NAME, SCIENTIFIC_NAME, Trophic.Niche, Trophic.Level, Primary.Lifestyle) %>%
  dplyr::distinct(Trophic.Niche, .keep_all = TRUE)

write.csv(list_spp_func, "Figures/Functional_Group_Example_species.csv", row.names = FALSE)

unique_common_names_count <- func_tbl %>%
  group_by(Primary.Lifestyle) %>%
  summarize(unique_common_name_count = n_distinct(COMMON_NAME), .groups = "drop")
