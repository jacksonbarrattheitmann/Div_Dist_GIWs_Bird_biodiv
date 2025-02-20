library(readr)
library(dplyr)
library(tidyverse)
library(vegan)
library(mobr)

# Now going to try to load in gamma data for each individual wetland, and loop through to extract the 
# species richness, abundance, S_PIE, Sn
combined_master_gamma_ref_wet <- readRDS(file = "Data/processed_buffer_chunks_with_ref_wetland/master_list_gamma_ref.RDS")

wetland_list <- unique(combined_master_gamma_ref_wet$ref_wetland)

calc_gamma_metrics <- function(gamma_chunk_id){
  
  test_chunk <- readRDS(file = paste0("Data/eBird_data_by_ref_wetland/",gamma_chunk_id,".RDS"))
  
  test_chunk$OBSERVATION_COUNT <- as.numeric(test_chunk$OBSERVATION_COUNT)
  
  samp_vector <- test_chunk %>%
    filter(is.na(OBSERVATION_COUNT)) %>%
    select(SAMPLING_EVENT_IDENTIFIER) %>%
    unique()
  
  filtered_chunk <- test_chunk %>%
    filter(!(SAMPLING_EVENT_IDENTIFIER %in% samp_vector$SAMPLING_EVENT_IDENTIFIER))
  
  # create a vector of the unique sampling event identifiers
  gamma_vector_event <- filtered_chunk %>%
    pull(SAMPLING_EVENT_IDENTIFIER) %>%
    unique()
  
  # create a function to apply 100 times
  random_sample_function <- function(draw, number_of_samples){
    
    # creating the vector of sampling event identifiers we want to sample
    random_sample <- sample(gamma_vector_event, number_of_samples)
    
    # calculated the avg abundance and average species richness per checklist
    gamma_checklists_sample <- filtered_chunk %>%
      filter(SAMPLING_EVENT_IDENTIFIER %in% random_sample) %>%
      select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, OBSERVATION_COUNT, ref_wetland) %>%
      mutate(total_SR = length(unique(COMMON_NAME)), total_N = sum(OBSERVATION_COUNT)) %>%
      group_by(SAMPLING_EVENT_IDENTIFIER) %>%
      summarise(N = sum(OBSERVATION_COUNT), SR = length(unique(COMMON_NAME)), 
                total_SR = unique(total_SR), total_N = unique(total_N)) %>%
      summarise(avg_n = mean(N), avg_SR = mean(SR), total_SR = unique(total_SR), total_N = unique(total_N))
    
  }
  
  # apply the function 100 times
  ref_resampled_SR <- bind_rows(lapply(c(1:100), function(x){random_sample_function(x, 500)}))
  
  # hist(ref_resampled_SR$total_SR)
  
  # here we have created a data frame with all the averages from the 100 samples of 500 checklists in gamma_chunk, and
  # adding the ref_wetland id
  ref_wetland_gamma_div_metrics <- ref_resampled_SR %>%
    summarize(median_SR=median(total_SR),
              mean_SR=mean(total_SR),
              sd_SR=sd(total_SR),
              median_N=median(total_N),
              mean_N=mean(total_N),
              sd_N=sd(total_N)) %>%
    mutate(total_observed_SR=length(unique(filtered_chunk$COMMON_NAME))) %>%
    mutate(total_number_samples=length(unique(filtered_chunk$SAMPLING_EVENT_IDENTIFIER))) %>%
    mutate(total_number_of_locations=length(unique(filtered_chunk$LOCALITY_ID))) %>%
    mutate(ref_wetland=gamma_chunk_id)
  
  return(ref_wetland_gamma_div_metrics)
  
}

ref_wetland_gamma_metrics <- map_dfr(wetland_list[1:10], calc_gamma_metrics)
  




