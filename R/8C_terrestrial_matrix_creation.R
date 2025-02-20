# Terrestrial Lifestyle summary statsitics
# Calculating speces richness, abundance, median richness per checklist, 
# and resampled richness

library(dplyr)

######## INSESSORIAL
terrestrial <- readRDS("Data/species_matrix/terrestrial_df.RDS")

terrestrial <- terrestrial %>%
  filter(OBSERVATION_COUNT >= 1) 

# Calculating total richness, total checklists, total observers, total abundance
s_matrix <- terrestrial %>%
  group_by(LOCALITY_ID) %>%
  summarise(total_checklists = length(unique(SAMPLING_EVENT_IDENTIFIER)), 
            total_richness = length(unique(COMMON_NAME)), 
            total_observers = sum(NUMBER_OBSERVERS),
            total_N = sum(OBSERVATION_COUNT),
            group = "terrestrial")

# calculating mean richness and abundance
# across all checklists submitted at a locality_id
mean_diversity_across_checklists <- terrestrial %>%
  group_by(LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER) %>%
  summarize(checklist_rich=length(unique(COMMON_NAME)),
            checklist_abund=sum(OBSERVATION_COUNT)) %>%
  ungroup() %>%
  group_by(LOCALITY_ID) %>%
  summarize(mean_richness_across_checklists=mean(checklist_rich),
            median_richness_across_checklists=median(checklist_rich),
            mean_abund_across_checklists=mean(checklist_abund),
            median_abund_across_checklists=median(checklist_abund))

# calculate a resampled species richness
# downsampling to 50 checklists at each wetland
downsample_function <- function(wetland_id, number_of_checklists){
  
  message(paste0("calculating richness for ", wetland_id))
  
  resample_function <- function(draw_number){
    
    wetland_dat <- terrestrial %>%
      dplyr::filter(LOCALITY_ID == wetland_id)
    
    checklist_sample <- wetland_dat %>%
      dplyr::select(SAMPLING_EVENT_IDENTIFIER) %>%
      distinct() %>%
      sample_n(number_of_checklists)
    
    sample_richness <- wetland_dat %>%
      dplyr::filter(SAMPLING_EVENT_IDENTIFIER %in% checklist_sample$SAMPLING_EVENT_IDENTIFIER) %>%
      group_by(LOCALITY_ID) %>%
      summarize(sample_richness=length(unique(COMMON_NAME)),
                sample_abund=sum(OBSERVATION_COUNT)) %>%
      mutate(draw=draw_number)
    
    return(sample_richness)
  }
  
  resampled_richness <- bind_rows(lapply(1:100, function(x) resample_function(x)))
  
  final_locality_resampled_rich <- resampled_richness %>%
    group_by(LOCALITY_ID) %>%
    summarize(resampled_median_richness=median(sample_richness),
              resampled_mean_richness=mean(sample_richness),
              resampled_median_abund=median(sample_abund),
              resampled_mean_abund=mean(sample_abund))
  
  return(final_locality_resampled_rich)
}

resampled_richness_per_locality <- bind_rows(lapply(unique(terrestrial$LOCALITY_ID), function(x) downsample_function(x, 50)))

# appending this output to the s_matrix data frame
s_final <- s_matrix %>%
  inner_join(mean_diversity_across_checklists, by="LOCALITY_ID") #%>%
#  inner_join(resampled_richness_per_locality, by = "LOCALITY_ID")


# Still need to work on the resample for the median richness, from 100 checklists
saveRDS(s_matrix, file = "Data/species_matrix/terrestrial_matrix.RDS")
