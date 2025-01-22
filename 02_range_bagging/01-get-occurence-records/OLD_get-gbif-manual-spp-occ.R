library(tidyverse)
library(sf)
library(glue)


# get names of files
dirs = list.dirs('data/occurences/gbif_manual/')

files = map(dirs[2:length(dirs)], ~paste0(.x, "/", list.files(.x)))
files = unlist(files)

# loop over each and save

# ## test one
# test = read.csv(files[1], sep="\t", header=T)
# 
# test2 = 
#   test %>% 
#   mutate(gbif_id = speciesKey, .before = gbifID) %>% 
#   st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

## now save all
# d = 1
for(file in files[11:length(files)]){
  temp = read.csv(file, sep="\t", header=T, stringsAsFactors = F, quote = "")
  temp = 
    temp %>% 
    mutate(gbif_id = speciesKey, .before = gbifID) %>% 
    mutate(decimalLatitude = as.numeric(decimalLatitude), 
           decimalLongitude = as.numeric(decimalLongitude)) %>% 
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  saveRDS(temp, glue("data/occurences/species/{temp$gbif_id[1]}.rds"))
  message(glue("{d} of {length(files)}"))
  d = d + 1
}


# check all spp in folder
gbifs_in_folder = str_remove(list.files("data/occurences/species/"), "\\.rds")
spp_list = read_rds("data/occurences/df_spp_occ_syns.rds")

spp_list %>% 
  filter(!gbif_id %in% gbifs_in_folder)

# all good 
