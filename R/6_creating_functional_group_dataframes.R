# Creating functional group matrices by primary lifestyle

library(readr)
library(dplyr)

func_matrix <- read_rds("Data/species_matrix/functional_traits_species_matrix.RDS")

unique(func_matrix$Primary.Lifestyle)
# Now need to break this data frame up into 5 seperate dfs based
# on the primary lifestyle

insessorial <- func_matrix %>% 
  filter(Primary.Lifestyle == "Insessorial")

aquatic <- func_matrix %>% 
  filter(Primary.Lifestyle == "Aquatic")

terrestrial <- func_matrix %>% 
  filter(Primary.Lifestyle == "Terrestrial")

generalist <- func_matrix %>% 
  filter(Primary.Lifestyle == "Generalist")

aerial <- func_matrix %>% 
  filter(Primary.Lifestyle == "Aerial")

# Now export each one, so we can make a separate script for each

saveRDS(insessorial, "Data/species_matrix/insessorial_df.RDS")
saveRDS(aquatic, "Data/species_matrix/aquatic_df.RDS")
saveRDS(terrestrial, "Data/species_matrix/terrestrial_df.RDS")
saveRDS(generalist, "Data/species_matrix/generalist_df.RDS")
saveRDS(aerial, "Data/species_matrix/aerial_df.RDS")
