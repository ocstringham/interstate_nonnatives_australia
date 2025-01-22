library(tidyverse)
library(sf)
# library(mapview)
library(glue)
library(CoordinateCleaner)


# # load in distribs
# distribs = read_rds('data/spp_geometries/_all_spp_geoms_v2.rds')

# load in species
# check all spp in folder
gbifs_in_folder = str_remove(list.files("data/occurences/species/"), "\\.rds")
# distribs %>% filter(!gbif_id %in% gbifs_in_folder)


# Loop over each spp
for(gbifId in gbifs_in_folder){
  
  # load in occs
  temp = read_rds(glue('data/occurences/species/{gbifId}.rds')) %>% 
    mutate(decimalLongitude = unlist(map(geometry,1)),
           decimalLatitude = unlist(map(geometry,2))
    ) %>% 
    st_drop_geometry()
  
  # get flags
  flags = clean_coordinates(x = temp,
                            lon = "decimalLongitude",
                            lat = "decimalLatitude",
                            countries = "countryCode",
                            species = "species",
                            capitals_rad = 1000, 
                            centroids_rad = 1000,
                            tests = c("capitals", "centroids", "duplicates",
                                      "equal","gbif", "institutions", # "seas",
                                      "zeros"))
  
  # get inds of  flags 
  # sum(flags$.summary)
  temp2 = flags$.summary 
  rm_inds = which(temp2)
  
  # save
  saveRDS(rm_inds, glue('data/occurences/rm_pts/clean/{gbifId}.rds'))
  
  # rm temps
  rm(temp, flags, temp2, rm_inds)
  gc()
  
  # print update
  print(glue("{which(gbifs_in_folder == gbifId)} of {length(gbifs_in_folder)}"))
  
}




# test = read_rds(glue('data/occurences/species/{gbifs_in_folder[1]}.rds')) %>% 
#   mutate(decimalLongitude = unlist(map(geometry,1)),
#          decimalLatitude = unlist(map(geometry,2))
#   ) %>% 
#   st_drop_geometry()
# 
# # Run CoordCleaner
# 
# flags = clean_coordinates(x = st_drop_geometry(test),
#                   lon = "decimalLongitude",
#                   lat = "decimalLatitude",
#                   countries = "countryCode",
#                   species = "species",
#                   capitals_rad = 1000, 
#                   centroids_rad = 1000,
#                   tests = c("capitals", "centroids", "duplicates",
#                             "equal","gbif", "institutions", # "seas",
#                             "zeros")) # most test are on by default
# # summary(flags)
# 
# 
# # flags %>% filter(.sea == 0) %>% 
# #   ggplot()+ coord_fixed()+ 
# #   geom_point(aes(x = decimalLongitude, y = decimalLatitude),
# #              colour = "darkred", size = 0.5)+
# #   theme_bw()
# 
# # exclude problem occ pts & rm if uncer  > 10km
# test_cl <- test[flags$.summary,] %>% 
#   filter(coordinateUncertaintyInMeters / 1000 <= 10 | 
#            is.na(coordinateUncertaintyInMeters))
# 
# 
# # back to sf
# 
# 
# 
# # hist(test$coordinateUncertaintyInMeters / 1000, breaks = 100)
# # table(test$basisOfRecord)
# # table(test$individualCount)
# 
# # get uncert < 20 km
# temp3 = (flags$coordinateUncertaintyInMeters / 1000) > 20
# temp3[is.na(temp3)] = FALSE
# # get uncert na
# temp4 = is.na(flags$coordinateUncertaintyInMeters)
# sum(temp3 | temp4)
# 
# rm_ind = (temp2 | (temp3 | temp4)) %>% which()
