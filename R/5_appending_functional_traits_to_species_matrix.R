# Creating function trait group RDS by the 5 Trophic Levels
# copy code into site-species matrix!

library(dplyr)
library(tidyverse)

file_list <- list.files(path = "Data/eBird_local_alpha_level/", pattern = "\\.RDS", full.names = TRUE)
r <- lapply(file_list, readRDS)
s <- bind_rows(r, .id = "column_label")
s <- as.data.frame(s)

s$OBSERVATION_COUNT <- as.numeric(s$OBSERVATION_COUNT)
s$COMMON_NAME <- as.factor(s$COMMON_NAME)
s$SAMPLING_EVENT_IDENTIFIER <- as.factor(s$SAMPLING_EVENT_IDENTIFIER)

#grabbing data for Muscovy Duck and Rock Piegon
# because of weird issue Jackson add details
temp <- s %>% dplyr::filter(CATEGORY == "domestic") %>%
  filter(COMMON_NAME == "Rock Pigeon" | COMMON_NAME == "Muscovy Duck", OBSERVATION_COUNT >= 1)

# Combining the rock pigeon and muscovy duck df with the larger s df
s_joined <- s %>%
  dplyr::filter(CATEGORY %in% c("species", "issf")) %>%
  bind_rows(temp)

get_group_checklists <- s_joined %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, GROUP_IDENTIFIER) %>%
  distinct() %>%
  replace_na(list(GROUP_IDENTIFIER="not shared")) %>%
  dplyr::filter(GROUP_IDENTIFIER != "not shared") %>%
  group_by(GROUP_IDENTIFIER) %>%
  sample_n(1)

s_not_shared_checklists <- s_joined %>%
  replace_na(list(GROUP_IDENTIFIER="not shared")) %>%
  dplyr::filter(GROUP_IDENTIFIER == "not shared")

s_trimmed_group_shared_checklists <- s_joined %>%
  dplyr::filter(SAMPLING_EVENT_IDENTIFIER %in% local(get_group_checklists$SAMPLING_EVENT_IDENTIFIER))

s_all_data_cleaned <- s_not_shared_checklists %>%
  bind_rows(s_trimmed_group_shared_checklists)

# filtering by species and issf, and Observation greater than or equal to 1 to get rid of Xs
species_counts_fixed <- s_all_data_cleaned %>%
  group_by(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME) %>%
  summarize(OBSERVATION_COUNT=sum(OBSERVATION_COUNT))

lists_meta <- s_all_data_cleaned %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, 6:22) %>%
  dplyr::select(-OBSERVER_ID, -GROUP_IDENTIFIER, -CATEGORY) %>%
  distinct()

s_final <- lists_meta %>%
  left_join(species_counts_fixed) 

# we want to append functional traits to the dataset above
# to then summarize the data by the five functional traits

# first get a list of all species and join that with the functional traits
species_list <- s_final %>%
  dplyr::select(COMMON_NAME, SCIENTIFIC_NAME) %>%
  distinct()

## Reading in the functional traits table
ebird_func_name <- read.csv("Data/functional_traits_data_AVONET/AVONET_Raw_Data.csv") %>%
  dplyr::select(1:26)

func_tble <- read.csv("Data/functional_traits_data_AVONET/AVONET1_BirdLife.csv") %>%
  dplyr::select(1:5, 30:32)

easy_join_works_first <- species_list %>%
  left_join(., func_tble %>%
              rename(., SCIENTIFIC_NAME=Species1)) %>%
  dplyr::filter(complete.cases(.))


join_number_2 <- species_list %>%
  dplyr::filter(!COMMON_NAME %in% local(easy_join_works_first$COMMON_NAME)) %>%
  left_join(., ebird_func_name %>%
              dplyr::select(1:5) %>%
              distinct() %>%
              rename(., SCIENTIFIC_NAME=Species2_eBird)) %>%
  rename(., func_name=Species1_BirdLife) %>%
  left_join(., func_tble %>%
              rename(., func_name=Species1)) %>%
  dplyr::select(1,2, 11:13) %>%
  distinct() %>%
  dplyr::filter(complete.cases(.))

second_join_species_list_func <- easy_join_works_first %>%
  bind_rows(join_number_2) %>%
  dplyr::select(1:2, 7:9)

missing_species <- species_list %>%
  dplyr::filter(!COMMON_NAME %in% local(second_join_species_list_func$COMMON_NAME))

final_trait_data_per_species <- second_join_species_list_func %>%
  bind_rows(., data.frame(COMMON_NAME=c("Mexican Duck", "Short-billed Gull", "Gray-headed Swamphen", "Tundra Bean-Goose"),
                          SCIENTIFIC_NAME=c("Anas diazi", "Larus brachyrhynchus", "Porphyrio poliocephalus", "Anser serrirostris"),
                          Trophic.Level=c("Herbivore", "Carnivore", "Herbivore", "Herbivore"),
                          Trophic.Niche=c("Herbivore Aquatic", "Omnivore", "Herbivore Aquatic", "Herbivore Terrestrial"),
                          Primary.Lifestyle=c("Aquatic", "Terrestrial", "Terrestrial", "Terrestrial")))



# Now appending this to the big bird matrix
func_matrix <- left_join(s_final, final_trait_data_per_species)

# Check to make sure we dont have any NAs
unique(func_matrix$Primary.Lifestyle)

# Saving the RDS
saveRDS(func_matrix, file = "Data/species_matrix/functional_traits_species_matrix.RDS")

