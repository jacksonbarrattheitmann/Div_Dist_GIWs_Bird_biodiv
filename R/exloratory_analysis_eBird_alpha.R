# Exploratory analysis of eBird data from wetland sites, arranged by Locality ID

# First need to load data in 

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

### Read in the env_matrix and the s_matrix to do some preliminary analysis

s_matrix <- readRDS("Data/species_matrix/s_matrix.RDS")

env_matrix <- readRDS("Data/earth_engine_env_data/env_matrix.RDS")

# Combine them to do analysis

wetland_bird_matrix <- s_matrix %>%
  full_join(env_matrix, by = "LOCALITY_ID")

## Creating a matrix with new grouping variable, regional v local

# here's the local dataframe, with new grouping variable "level" == local
local <- wetland_bird_matrix[, c(1:5, 9:18)] %>% 
  rename(flooded_vegetation = flooded_vegetation_wet, water = water_wet, built = built_wet, trees = trees_wet) %>% 
  mutate(level = "local")

# make smaller, just the variables, we're interested in

local_trim <- local[, c(1:5, 7, 9, 14:16)]

# now do this for the regional scale

regional <- wetland_bird_matrix[, c(1:5, 19:28)] %>% 
  rename(flooded_vegetation = flooded_vegetation_gamma, water = water_gamma, built = built_gamma, trees = trees_gamma) %>% 
  mutate(level = "regional")

# trim the data frame for only our variables of interest
regional_trim <- regional[, c(1:5, 7, 9, 14:16)]

# Now we need to combine them based on common columns

alpha_scale_df <- rbind(local_trim, regional_trim)

# first checking the distribution of our predictor variables



# Now we need to make multiple line plots with our env variables of interest

# built area plot
p1 <- ggplot(data = alpha_scale_df, aes(x = built, y = total_richness, color = level)) +
  geom_smooth(method = 'lm', fill = 'light grey', linewidth = 3) + scale_x_continuous(trans = "log10", labels = scales::percent) + 
  scale_color_discrete(labels = c("Local", "Regional")) + xlab(NULL) + ylab(NULL) +
  theme(axis.text.x=element_text(size=25), axis.text.y = element_text(size=25),
        axis.title = element_text(size = 12),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))

# flooded_vegetation plot
p2 <- ggplot(data = alpha_scale_df, aes(x = flooded_vegetation, y = total_richness, color = level)) +
  geom_smooth(method = 'lm', fill = 'light grey', linewidth = 3) + scale_x_continuous(trans = "log10", labels = scales::percent) +
  scale_color_discrete(labels = c("Local", "Regional")) + xlab(NULL) + ylab(NULL) +
  theme(axis.text.x=element_text(size=25), axis.text.y = element_text(size=25),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 21),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))
p2

# tree cover plot
p3 <- ggplot(data = alpha_scale_df, aes(x = trees, y = total_richness, color = level)) +
  geom_smooth(method = 'lm', fill = 'light grey', linewidth = 3) + scale_x_continuous(trans = "log10", labels = scales::percent) + 
  scale_color_discrete(labels = c("Local", "Regional")) + xlab(NULL) + ylab(NULL) +
  theme(axis.text.x=element_text(size=25), axis.text.y = element_text(size=25),
        axis.title = element_text(size = 12),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))

# water cover plot
p4 <- ggplot(data = alpha_scale_df, aes(x = water, y = total_richness, color = level)) +
  geom_smooth(method = 'lm', fill = 'light grey', linewidth = 3) + scale_x_continuous(trans = "log10", labels = scales::percent) +
  scale_color_discrete(labels = c("Local", "Regional")) + xlab(NULL) + ylab(NULL) +
  theme(axis.text.x=element_text(size=25), axis.text.y = element_text(size=25),
        axis.title = element_text(size = 12),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))



p1
ggsave("built.png", bg = "transparent")

p2
ggsave("flooded_veg.png", bg = "transparent")

p3
ggsave("tree.png", bg = "transparent")

p4
ggsave("water.png", bg = "transparent")

# Let's visualize some of the data
hist(log(wetland_bird_matrix$flooded_vegetation_wet))

par(mfrow = c(1, 1))

p1 <- ggplot(data = wetland_bird_matrix, aes(x = built_wet, y = total_richness)) +
  geom_point() + geom_smooth(method = 'lm') + xlab("Built Area Cover Local") + ylab("Total Species Richness (S)") +
  theme_bw()

p2 <- ggplot(data = wetland_bird_matrix, aes(x = built_gamma, y = total_richness)) +
  geom_point() + geom_smooth(method = 'lm') + xlab("Built Area Cover Regional") + ylab("Total Species Richness (S)") +
  theme_bw()

p3 <- ggplot(data = wetland_bird_matrix, aes(x = log(water_wet), y = total_richness)) +
  geom_point() + geom_smooth(method = 'lm') + xlab("Water Cover Local") + ylab("Total Species Richness (S)") +
  theme_bw()

p4 <- ggplot(data = wetland_bird_matrix, aes(x = log(water_gamma), y = total_richness)) +
  geom_point() + geom_smooth(method = 'lm') + xlab("Water Cover Regional") + ylab("Total Species Richness (S)") +
  theme_bw()

p5 <- ggplot(data = wetland_bird_matrix, aes(x = log(flooded_vegetation_wet), y = total_richness)) +
  geom_point() + geom_smooth(method = 'lm') + xlab("Flooded Vegetation Cover Local") + ylab("Total Species Richness (S)") +
  theme_bw()

p6 <- ggplot(data = wetland_bird_matrix, aes(x = log(flooded_vegetation_gamma), y = total_richness)) +
  geom_point() + geom_smooth(method = 'lm') + xlab("Flooded Vegetation Regional") + ylab("Total Species Richness (S)") +
  theme_bw()

ggplot(data = wetland_bird_matrix, aes(x = log(hum_pop_mean), y = total_richness)) +
  geom_point() + geom_smooth(method = 'lm') + xlab("Human Population") + ylab("Total Species Richness (S)") +
  theme_bw()

# make the background transparent here
ggplot(data = wetland_bird_matrix, aes(x = area_sqkm*1000, y = total_richness)) +
  geom_point() + geom_smooth(method = 'lm') + xlab(expression(log~Area~(m^{2}))) + ylab("Total Species Richness (S)") +
  theme_bw() + scale_x_log10()

ggsave("Spp_area_plot.png", bg = "transparent")

?ggsave

ggplot(data = wetland_bird_matrix, aes(x = shrub_and_scrub_wet, y = mean_richness)) +
  geom_point() + geom_smooth()

(p1 + p2) /
  (p3 + p4) /
  (p5 + p6)

#### Creating the LM simple model first

simple_rich <- lm(mean_richness_across_checklists ~
                    log(area_sqkm) + log(hum_pop_mean) + built_gamma + built_wet + total_checklists +
                    evi_mean + water_wet
                    , data = wetland_bird_matrix)

summary(simple_rich)

margins_summary(simple_rich)

plot(margins(simple_rich))

?plot

avg_slopes(simple_rich, variables = "area_sqkm", by = "total_checklists")

plot_slopes(simple_rich, variables = "area_sqkm", by = "total_checklists")

# checking violations of normality
check_model(simple_rich)

# data looks non-normal
shapiro.test(simple_rich$residuals)

#### CREATING THE GLM MODEL for mean_richness

model <- glm(mean_richness ~ hum_pop_mean + flooded_vegetation_wet + area_sqkm +
      water_wet + shrub_and_scrub_gamma + water_gamma, data = wetland_bird_matrix)

summary(model)

check_model(model)

model_performance(model)

check_collinearity(model)

# qq plots to test normality
plot(model, which = 1)
plot(model, which = 2)

#### CREATING THE SAME MODEL for total_richness

model_total <- glm(total_richness ~ hum_pop_mean + flooded_vegetation_wet + area_sqkm +
               water_wet + shrub_and_scrub_wet, data = wetland_bird_matrix)

summary(model_total)

# qq plots to test normality
plot(model_total, which = 1)
plot(model_total, which = 2)




######################
#####################
######################
# Corey stuff

# hist of richness
hist(wetland_bird_matrix$total_richness)

# looks pretty normal... can probably use gaussian to approximate it
# let's do a quick check...
# using a simple model
mod1_lm <- lm(total_richness ~ log10(area_sqkm) + log10(total_checklists), data=wetland_bird_matrix)
summary(mod1_lm)

check_model(mod1_lm)

mod1_glm <- glm(total_richness ~ log10(area_sqkm) + log10(total_checklists), family=poisson, data=wetland_bird_matrix)
summary(mod1_glm)

check_model(mod1_glm)






# scale and center variables
# https://stats.stackexchange.com/questions/29781/when-conducting-multiple-regression-when-should-you-center-your-predictor-varia
# https://statmodeling.stat.columbia.edu/2009/07/11/when_to_standar/


library(arm)
?rescale


# why we should include 'total checklists' in the model
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1046/j.1365-2656.2002.00618.x



mod1 <- lm(total_richness ~ log10(area_sqkm) + log10(total_checklists), data=wetland_bird_matrix)
summary(mod1)  


mod_built <- lm(total_richness ~ log10(total_checklists) + built_wet + built_gamma, data=wetland_bird_matrix)
summary(mod_built)

mod_built <- lm(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_gamma), data=wetland_bird_matrix)
summary(mod_built)

mod_trees <- lm(total_richness ~ rescale(log10(total_checklists)) + rescale(trees_wet) + rescale(trees_gamma), data=wetland_bird_matrix)
summary(mod_trees)

# fit a temporary 'big mod'
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

# refit it, but with glm...
big_mod_glm <- glm(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_gamma) +
                rescale(grass_wet) + rescale(grass_gamma) + rescale(trees_wet) + rescale(trees_gamma) +
                rescale(water_wet) + rescale(water_gamma), family=poisson, data=wetland_bird_matrix)

summary(big_mod_glm)

# make a plot...
big_mod_summary_glm <- broom::tidy(big_mod_glm, conf.int=TRUE) %>%
  mutate(predictor=c("Intercept", "Total checklists", 
                     "Built local", "Built landscape",
                     "Grassland local", "Grassland landscape", 
                     "Trees local", "Trees landscape",
                     "Water local", "Water landscape")) %>%
  mutate(Scale=c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape"))

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



# probably should look at marginal effects still
# https://strengejacke.github.io/ggeffects/



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
big_mod_glm_re <- glmer(total_richness ~ rescale(log10(total_checklists)) + rescale(built_wet) + rescale(built_gamma) +
                              rescale(grass_wet) + rescale(grass_gamma) + rescale(trees_wet) + rescale(trees_gamma) +
                              rescale(water_wet) + rescale(water_gamma) + (1| NA_L1NAME), family=poisson, data=wetland_bird_matrix)

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
                     "Built local", "Built landscape",
                     "Grassland local", "Grassland landscape", 
                     "Trees local", "Trees landscape",
                     "Water local", "Water landscape")) %>%
  mutate(Scale=c(NA, NA, "Local", "Landscape", "Local", "Landscape", "Local", "Landscape", "Local", "Landscape"))

big_mod_summary_glm_re %>%
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
  ggtitle("glm with random effect")




##### Wetland area versus Hotspot area

# have to compute wetland area, which is just the land cover within wetland * hotspot area * 1000 to get it into m2

wetland_bird_matrix <- wetland_bird_matrix %>%
  mutate(wet_area = (area_sqkm * water_wet)* 1000)


# now make it a figure

ggplot(data = wetland_bird_matrix, aes(x = area_sqkm, y = wet_area)) +
  geom_point() + 
  geom_smooth(method="lm") +
  scale_x_log10() +
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

# log transform the land cover variables to visualize
# mean richness across checklists
ggpairs(wetland_bird_matrix[, c(10, 18:19, 21, 23, 31, 33, 40)])

# total richness
ggpairs(wetland_bird_matrix[, c(3, 18:19, 21, 23, 31, 33, 40)])

# resampled mean richness
ggpairs(wetland_bird_matrix[, c(15, 18:19, 21, 23, 31, 33, 40)])

# different richness measures plotted against each other
ggpairs(wetland_bird_matrix[, c(3, 10, 15)])
