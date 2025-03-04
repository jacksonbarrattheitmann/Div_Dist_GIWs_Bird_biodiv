# Div_Dist_GIWs_Bird_biodiv
Code to recreate figures and analysis for Diversity and Distributions manuscript titled "Land cover and area influence bird biodiversity in geographically isolated wetlands".

Easiest way to run all the code is to open up the .Rproj file and run scripts sequentially from 1-13.

# R folder
  - These scripts process all data from eBird and Google Earth Engine, filter these data, and then run the analysis.
  - Be aware that scipts 1 + 3 require an API key to pay for downloaded data--therefore this code will not run--but the subsequent data are available in the "Data" folder. 
  - Script 8A-E create species-site matricies for each of the 5 functional from AVONET
  - Script 10-13 are strickly for data analysis and re-creating any figures seen in the manuscript

# Data folder
  - archived_wetlands_not_isolated : this contains eBird hotspots that were found to NOT be isolated wetlands
  - combined_shp_geojson : this is a giant shapefile that merges all of the 207 isolated wetlands into a single file for GEE script
  - earth_engine_env_data : this has all of the 6 land cover classes from Dynamic World dataset for each of the 207 isolated wetlands within their boundaries (local), and at 5km, 10km, and 25km buffers
  - ebird_local_alpha_level : this is the raw eBird data downloaded for all of the wetlands broken up by month, these data are unfiltered
  - functional_traits_data_AVONET : this is the data table from AVONET published by Tobias et al. (2022) in *Ecology Letters*
  - mapped_wetlands_geojson : these are the raw .geojson files for each of the individual wetlands, that are combined in folder above
  - potential_study_sites_list : .csv file of all eBird hotspots downloaded with the word "wetland" or "Wetland" in the name
  - species_matrix : these are the compiled and filtered species-site matricies broken up by the 5 functional groups from AVONET
  - state_centroids : .csv used to create Figure 1 of map with wetlands by state
  - supp_data : .csv used to create the supplementary tables
  - us_counties: .shapefile used to create supplementary table with wetlands by county

# Figures folder
  - .jpeg and .png files of every figure, supplementary figure, and table in the manuscript
