# analysis script of total richness
# this accompanies other scripts of 'mean richness' and 'resampled richness'
# that convey a similar message

# why we should include 'total checklists' in the model
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1046/j.1365-2656.2002.00618.x

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
library(scales)
library(broom.mixed)
library(arm)

# read in data
### Read in the env_matrix and the s_matrix to do some preliminary analysis

s_matrix <- readRDS("Data/species_matrix/s_matrix.RDS")

env_matrix <- readRDS("Data/earth_engine_env_data/env_matrix.RDS")

# Combine them to do analysis
wetland_bird_matrix <- s_matrix %>%
  full_join(env_matrix, by = "LOCALITY_ID")

###############################################
########################
######## get summary information for top-level results
#############################################
#####################################################
# how many wetlands
nrow(wetland_bird_matrix)
length(unique(wetland_bird_matrix$LOCALITY_ID))

# size of wetlands?
median(wetland_bird_matrix$area_sqkm)
range(wetland_bird_matrix$area_sqkm)
sd(wetland_bird_matrix$area_sqkm)

# supp figure?
ggplot(wetland_bird_matrix, aes(x=area_sqkm))+
  geom_histogram(fill="gray80", color="black")+
  geom_vline(xintercept = 0.16, col = "red")+
  theme_bw()+
  scale_x_log10()

biggest_wetland <- wetland_bird_matrix %>%
  filter(area_sqkm > 3)
# Other summary statistics for Results section

### total richness mean and standard deviation
mean(wetland_bird_matrix$total_richness)
sd(wetland_bird_matrix$total_richness)


##### Wetland area versus Hotspot area
# supp figure in the methods
# have to compute wetland area, which is just the land cover within wetland * hotspot area
wetland_bird_matrix <- wetland_bird_matrix %>%
  mutate(wet_area = (area_sqkm * water_wet))

# now make it a figure
ggplot(data = wetland_bird_matrix, aes(x = area_sqkm, y = wet_area)) +
  geom_point() + 
  geom_smooth(method="lm") +
  scale_x_log10() +
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black")) +
  xlab("Site Area") +
  ylab("Water Cover Area")

# other basic information to put in text (and maybe a supp figure)
# in the higher-level results
# table of states per number of wetlands (supplementary table)
# some information about checklists
summary(wetland_bird_matrix$total_observers)
sd(wetland_bird_matrix$total_observers)

# relationship between checklists and richness
ggplot(wetland_bird_matrix, aes(x=total_checklists, y=total_richness))+
  geom_point()+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()

# supp figure? methods supp figure? when we provide justification of using total richness

#################################################
#################################################
############## Let's think about modelling!! ####
##################################################
######################
#####################
######################
# hist of richness
hist(wetland_bird_matrix$total_richness)

# make a histogram of all the predictor variables
# could use facet_wrap (for each predictor)
# and then color it so in each facet there is a local and landscape histogram
# this would be a good supp figure and also let us know about transformations

# could also in combination make a ggcorrplot() of the predictor variables

# looks pretty normal... can probably use gaussian to approximate it
# let's do a quick check...
# using a simple model
mod1_lm <- lm(total_richness ~ log10(area_sqkm) + log10(total_checklists), data=wetland_bird_matrix)
summary(mod1_lm)

check_model(mod1_lm)

mod1_glm <- glm(total_richness ~ log10(area_sqkm) + log10(total_checklists), family=poisson, data=wetland_bird_matrix)
summary(mod1_glm)

check_model(mod1_glm)

# the residuals look better
# and so we are going to go with a glm as our model of choice throughout
# instead of gaussian
# it is count data
# Jackson add more justification


# scale and center variables
# https://stats.stackexchange.com/questions/29781/when-conducting-multiple-regression-when-should-you-center-your-predictor-varia
# https://statmodeling.stat.columbia.edu/2009/07/11/when_to_standar/

library(arm)
?rescale

# fitting a model for species area relationship only
mod1 <- glm(total_richness ~ log10(area_sqkm) + log10(total_checklists), family=poisson, data=wetland_bird_matrix)
summary(mod1)  

# refitting the model to look at the effect of scaling
# read references above
# need to mention this in methods!
mod1_rescaled <- glm(total_richness ~ rescale(log10(area_sqkm)) + rescale(log10(total_checklists)), family=poisson, data=wetland_bird_matrix)
summary(mod1_rescaled)  

check_model(mod1_rescaled)
# from here on we will use scaled version, using rescale() function.


######## Looking at constructed versus natural wetlands SUPPLEMENT
######## split up SAR plot by these two groups

# first calculate number of wetlands in each category

c_n_u_count <- wetland_bird_matrix %>%
  group_by(wet_type) %>%
  count()

# get rid of the unknown wetlands
wet_types_c_n <- wetland_bird_matrix %>%
  filter(wet_type != "U")

wet_type_areas <- wetland_bird_matrix %>%
  group_by(wet_type) %>%
  summarise(mean(area_sqkm), var(area_sqkm))



#
boxplot(wet_types_c_n$total_richness ~ wet_types_c_n$wet_type)

# GLM model with interaction term between area and wetland type
wet_c_n_mod <- glm(total_richness ~ log10(area_sqkm) * wet_type + log10(total_checklists), family = poisson, data = wet_types_c_n)

summary(wet_c_n_mod)
anova(wet_c_n_mod)
check_model(wet_c_n_mod)

## adding wetland type as a function of area

ggplot(data = wet_types_c_n, aes(x = area_sqkm, y = total_richness)) +
  geom_point(aes(color = wet_type)) +
    geom_smooth(aes(color = wet_type), method = 'lm', se = T) + theme_bw() +
  xlab("log Area (km2)") +
  ylab("Total Richness") +
  scale_x_log10()

?check_model




###########LANDCOVER
# look at 'built'
mod_built <- glm(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_gamma), 
                 family=poisson, data=wetland_bird_matrix)
summary(mod_built)

check_model(mod_built)

# look at 'trees'
mod_trees <- glm(total_richness ~ rescale(log10(total_checklists)) + rescale(trees_wet) + rescale(trees_gamma), 
                 family=poisson, data=wetland_bird_matrix)
summary(mod_trees)

check_model(mod_trees)

# look at 'flooded vegetation'
mod_veg <- glm(total_richness ~ rescale(log10(total_checklists)) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_gamma), 
                 family=poisson, data=wetland_bird_matrix)
summary(mod_veg)

check_model(mod_veg)

# look at 'water'
mod_water <- glm(total_richness ~ rescale(log10(total_checklists)) + rescale(water_wet) + rescale(water_gamma), 
               family=poisson, data=wetland_bird_matrix)
summary(mod_water)

check_model(mod_water)

# maybe do this for each 'pair' of variables?

################################################ 25km ###############
################################################
# fit a temporary 'big mod'
# refit it, but with glm...
big_mod_glm <- glm(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_25km) +
                     rescale(grass_wet) + rescale(grass_25km) + rescale(trees_wet) + rescale(trees_25km) +
                     rescale(water_wet) + rescale(water_25km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_25km)
                   + rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_25km) + rescale(shan_wet) + rescale(shan_gamma_25), family=poisson, data=wetland_bird_matrix)

summary(big_mod_glm)

check_model(big_mod_glm)
# make a plot...
big_mod_summary_glm <- broom::tidy(big_mod_glm, conf.int=TRUE) %>%
  mutate(predictor=c("Intercept", "Total checklists", 
                     "Built local", "Built landscape",
                     "Grassland local", "Grassland landscape", 
                     "Trees local", "Trees landscape",
                     "Water local", "Water landscape",
                     "Flooded Veg local", "Flooded Veg landscape",
                     "Shrub local", "Shrub landscape",
                     "Heterogeneity local", "Heterogeneity landscape")) %>%
  mutate(Scale=c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape",
                 "Local", "Landscape", "Local", "Landscape", "Local", "Landscape"))

big_mod_summary_glm %>%
  dplyr::filter(! predictor %in% c("Intercept", "Total checklists")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Effect size")+
  xlab("")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")+
  ggtitle("glm")

## exporting the table of model outputs
big_mod_res <- as.data.frame(tidy(big_mod_glm))
write.csv(big_mod_res, file = "Figures/big_mod_table.csv")
# probably should look at marginal effects still
# https://strengejacke.github.io/ggeffects/
# the idea here is we could look at the 'raw' data
# but accounting for/averaging over the effect of checklists...

######################################################
######################################################
# in reality we might want to do a random effects model
# allow intercept to vary based on geographic realm?
length(unique(wetland_bird_matrix$NA_L1CODE))
unique(wetland_bird_matrix$NA_L1NAME)

# check sample size among potential random effectS?
wetland_bird_matrix %>%
  group_by(NA_L1NAME) %>%
  summarize(N=n())

# so quite unbalanced, which is fine, but just need to note that there will be some shrinkage
# likely to take place among the random effects.
# I do kind of question this a little bit seeing this.

############## 25km ######################
big_mod_glm_re <- glmer(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_25km) +
                          rescale(grass_wet) + rescale(grass_25km) + rescale(trees_wet) + rescale(trees_25km) +
                          rescale(water_wet) + rescale(water_25km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_25km)
                        + rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_25km) + rescale(shan_wet) +
                          rescale(shan_gamma_25) + (1| NA_L1NAME), family=poisson, data=wetland_bird_matrix)

summary(big_mod_glm_re)

# Random effects: The term (1| NA_L1NAME) specifies that the model includes a random intercept for each level of NA_L1NAME. 
# This means that the model allows the baseline level of total_richness to vary by NA_L1NAME. In other words, 
# it is assuming that there's some grouping structure in your data specified by NA_L1NAME that 
# could cause observations within the same group to be more similar to each other than to observations in different groups. 
# This random intercept captures that variability.

# make a plot...
big_mod_summary_glm_re <- broom.mixed::tidy(big_mod_glm_re, conf.int=TRUE) %>%
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

big_mod_summary_glm_re %>%
  dplyr::filter(! predictor %in% c("Intercept", "Total checklists")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x =element_text(color="black"), 
        axis.text.y = element_text(color = c("red", "red", "black", "blue",
                                             "black", "blue", "black", "black",
                                             "black", "black", "red", "black",
                                             "black", "black")))+
  ylab("Effect size")+
  xlab("")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")

check_model(big_mod_glm_re)


################### 10km ######################
# so quite unbalanced, which is fine, but just need to note that there will be some shrinkage
# likely to take place among the random effects.
# I do kind of question this a little bit seeing this.
big_mod_glm_re_10 <- glmer(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_10km) +
                          rescale(grass_wet) + rescale(grass_10km) + rescale(trees_wet) + rescale(trees_10km) +
                          rescale(water_wet) + rescale(water_10km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_10km)
                        + rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_10km) + rescale(shan_wet) +
                          rescale(shan_gamma_10) + (1| NA_L1NAME), family=poisson, data=wetland_bird_matrix)

summary(big_mod_glm_re_10)

# Random effects: The term (1| NA_L1NAME) specifies that the model includes a random intercept for each level of NA_L1NAME. 
# This means that the model allows the baseline level of total_richness to vary by NA_L1NAME. In other words, 
# it is assuming that there's some grouping structure in your data specified by NA_L1NAME that 
# could cause observations within the same group to be more similar to each other than to observations in different groups. 
# This random intercept captures that variability.

# make a plot...
big_mod_summary_glm_re_10 <- broom.mixed::tidy(big_mod_glm_re_10, conf.int=TRUE) %>%
  dplyr::filter(effect=="fixed") %>%
  mutate(predictor=c("Intercept", "Total checklists", 
                     "Built local", "Built 10km",
                     "Grassland local", "Grassland 10km", 
                     "Trees local", "Trees 10km",
                     "Water local", "Water 10km",
                     "Flooded Veg local", "Flooded Veg 10km",
                     "Shrub local", "Shrub 10km",
                     "Heterogeneity local", "Heterogeneity 10km")) %>%
  mutate(Scale=c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape",
                 "Local", "Landscape", "Local", "Landscape", "Local", "Landscape")) %>%
  mutate(model = "Model 10km")

big_mod_summary_glm_re_10 %>%
  dplyr::filter(! predictor %in% c("Intercept", "Total checklists")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x =element_text(color="black"), 
        axis.text.y = element_text(color = c("red", "red", "black", "blue",
                                             "black", "black", "black", "black",
                                             "black", "black", "red", "black",
                                             "black", "black")))+
  ylab("Effect size")+
  xlab("")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")

check_model(big_mod_glm_re_10)

################ 5km #################

# so quite unbalanced, which is fine, but just need to note that there will be some shrinkage
# likely to take place among the random effects.
# I do kind of question this a little bit seeing this.
big_mod_glm_re_5 <- glmer(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_5km) +
                          rescale(grass_wet) + rescale(grass_5km) + rescale(trees_wet) + rescale(trees_5km) +
                          rescale(water_wet) + rescale(water_5km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_5km)
                        + rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_5km) + rescale(shan_wet) +
                          rescale(shan_gamma_5) + (1| NA_L1NAME), family=poisson, data=wetland_bird_matrix)

summary(big_mod_glm_re_5)

# Random effects: The term (1| NA_L1NAME) specifies that the model includes a random intercept for each level of NA_L1NAME. 
# This means that the model allows the baseline level of total_richness to vary by NA_L1NAME. In other words, 
# it is assuming that there's some grouping structure in your data specified by NA_L1NAME that 
# could cause observations within the same group to be more similar to each other than to observations in different groups. 
# This random intercept captures that variability.

# make a plot...
big_mod_summary_glm_re_5 <- broom.mixed::tidy(big_mod_glm_re_5, conf.int=TRUE) %>%
  dplyr::filter(effect=="fixed") %>%
  mutate(predictor=c("Intercept", "Total checklists", 
                     "Built local", "Built 5km",
                     "Grassland local", "Grassland 5km", 
                     "Trees local", "Trees 5km",
                     "Water local", "Water 5km",
                     "Flooded Veg local", "Flooded Veg 5km",
                     "Shrub local", "Shrub 5km",
                     "Heterogeneity local", "Heterogeneity 5km")) %>%
  mutate(Scale=c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape",
                 "Local", "Landscape", "Local", "Landscape", "Local", "Landscape")) %>%
  mutate(model = "Model 5km")

big_mod_summary_glm_re_5 %>%
  dplyr::filter(! predictor %in% c("Intercept", "Total checklists")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x =element_text(color="black"), 
        axis.text.y = element_text(color = c("red", "red", "black", "blue",
                                             "black", "black", "black", "black",
                                             "black", "black", "red", "black",
                                             "black", "black")))+
  ylab("Effect size")+
  xlab("")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")

check_model(big_mod_glm_re_5)



########## Creating the big model ###############



combine_summary <- bind_rows(big_mod_summary_glm_re, big_mod_summary_glm_re_10, big_mod_summary_glm_re_5)

#### LANDSCAPE SENSITIVITY
combine_summary %>%
  dplyr::mutate(color_group = substring(predictor, 1, 1)) %>%
  dplyr::filter(! predictor %in% c("Intercept", "Total checklists")) %>%
  dplyr::filter(Scale == "Landscape") %>%
  ggplot(., aes(x=predictor, y=estimate, color=color_group))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x =element_text(color="black"), 
        axis.text.y = element_text(color = c("black")),
        legend.position = "none")+
  ylab("Effect size")+
  xlab("")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")


####### LOCAL SENSITIVITY
combine_summary %>% 
  dplyr::mutate(color_group = substring(predictor, 1, 1)) %>% 
  dplyr::filter(!predictor %in% c("Intercept", "Total checklists")) %>% 
  dplyr::filter(Scale == "Local") %>% 
  ggplot(., aes(x = predictor, y = estimate, color = color_group, group = predictor)) + 
  # Dodge the points horizontally to avoid overlap
  geom_point(position = position_dodge2(width = 0.92)) +  
  # Dodge the error bars horizontally to avoid overlap
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge2(width = 0.75)) +  
  coord_flip() +  # Flip the coordinates to switch x and y axes
  theme_bw() + 
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"), 
        legend.position = "none") + 
  ylab("Effect size") + 
  xlab("") + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  scale_color_brewer(palette = "Dark2")



median(wetland_bird_matrix$area_sqkm)
var(wetland_bird_matrix$area_sqkm)











#######SUPPLEMENTAL ANALYSIS ###################
############# Adding Area into the land cover model to look at the interplay
big_mod_glm_re_area <- glmer(total_richness ~ rescale(log10(area_sqkm)) + rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_25km) +
                               rescale(grass_wet) + rescale(grass_25km) + rescale(trees_wet) + rescale(trees_25km) +
                               rescale(water_wet) + rescale(water_25km) + rescale(flooded_vegetation_wet) + rescale(flooded_vegetation_25km)
                             + rescale(shrub_and_scrub_wet) + rescale(shrub_and_scrub_25km) + rescale(shan_wet) +
                               rescale(shan_gamma_25) + (1| NA_L1NAME), family=poisson, data=wetland_bird_matrix)

summary(big_mod_glm_re_area)

check_model(big_mod_glm_re_area)

# make a plot...
big_mod_summary_glm_re_area <- broom.mixed::tidy(big_mod_glm_re_area, conf.int=TRUE) %>%
  dplyr::filter(effect=="fixed") %>%
  mutate(predictor=c("Intercept","Area", "Total checklists", 
                     "Built local", "Built landscape",
                     "Grassland local", "Grassland landscape", 
                     "Trees local", "Trees landscape",
                     "Water local", "Water landscape",
                     "Flooded Veg local", "Flooded Veg landscape",
                     "Shrub local", "Shrub landscape",
                     "Heterogeneity local", "Heterogeneity landscape")) %>%
  mutate(Scale=c(NA, "Local", NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape",
                 "Local", "Landscape", "Local", "Landscape", "Local", "Landscape"))

big_mod_summary_glm_re_area %>%
  dplyr::filter(! predictor %in% c("Intercept", "Total checklists")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x =element_text(color="black"), 
        axis.text.y = element_text(color = c("blue", "red", "red", "black", "black",
                                             "black", "blue", "black", "black",
                                             "black", "black", "red", "black",
                                             "black", "black")))+
  ylab("Effect size")+
  xlab("")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")






# plot of tree landscape and 
plot(rescale(wetland_bird_matrix$flooded_vegetation_gamma), rescale(wetland_bird_matrix$shan_gamma))


land_cover <- env_matrix[, c(3:20, 29:30)]

M = cor(log(land_cover))
corrplot(M, method = "circle") # colorful number





###########################
########################### 
######### checks of different things
######################### 
# check to make sure we get the same result for lm versus glm
big_mod <- lm(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_gamma) +
                rescale(grass_wet) + rescale(grass_gamma) + rescale(trees_wet) + rescale(trees_gamma) +
                rescale(water_wet) + rescale(water_gamma), data=wetland_bird_matrix)

summary(big_mod)

# make a plot...
big_mod_summary <- broom::tidy(big_mod, conf.int=TRUE) %>%
  mutate(predictor=c("Intercept", "Total checklists", 
                     "Built local", "Built landscape",
                     "Grassland local", "Grassland landscape", 
                     "Trees local", "Trees landscape",
                     "Water local", "Water landscape")) %>%
  mutate(Scale=c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape"))

big_mod_summary %>%
  dplyr::filter(! predictor %in% c("Intercept", "Total checklists")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Effect size")+
  xlab("")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")+
  ggtitle("lm")

















