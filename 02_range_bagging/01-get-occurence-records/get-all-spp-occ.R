# Get occurence records for all species in the dataset from GBIF

library(raster)
library(tidyverse)
library(rgbif)
library(sf)
library(mapview)


# Get list of species (gbif)

## load ad data
df = read_rds('data/_final_gt_with_oor.rds') %>% 
  select(species, common_name, gbif_id:subspecies) %>% 
  distinct() %>% 
  filter(gbif_rank %in% c("species", "subspecies"))

## Are all subspecies also recorded as species?
df %>% 
  filter(gbif_rank == "subspecies") %>% 
  distinct(species) %>% 
  filter(!species %in% (df %>% filter(gbif_rank == "species") %>% pull(species)))

## add in row, to get all species
df_updated = df %>% 
  select(-subspecies) %>% 
  filter(gbif_rank == "species") %>% 
  bind_rows(tibble("species"    = "Nephrurus levis", 
                   "common_name"  =  "Common Knob-tailed Gecko", 
                   "gbif_id"  =  "2447717", 
                   "gbif_name" = "Nephrurus levis", 
                   "gbif_rank"  = "species", 
                   "class"   = "Reptilia", 
                   "order"  = "Squamata", 
                   "family" = "Carphodactylidae", 
                   "genus" = "Nephrurus")
  )


# check for synonoyms (bc gbif updates)
syn_check = purrr::map_df(df_updated$gbif_id, ~name_usage(.x)[["data"]] )
syn_check$vernacularName[ syn_check$taxonomicStatus != "ACCEPTED" ]

# replace syns with accepted names
syns_df = syn_check %>% filter(taxonomicStatus == "SYNONYM") %>% 
  select(species = scientificName, acc_species = species, acc_gbif_id = speciesKey, acc_class = class) %>% 
  mutate(species = str_extract(species, "[A-z]+ [A-z]+") %>% str_trim())


## any repeated species now?
df_updated2 = 
  (
  df_updated %>% 
  select(species, gbif_id, common_name, class) %>% 
  left_join(syns_df) %>% 
  filter(!is.na(acc_species)) %>% 
  select(species = acc_species, gbif_id = acc_gbif_id, common_name, 
         syn_species = species, syn_gbif_id = gbif_id, class, acc_class) %>% 
  mutate(gbif_id = as.character(gbif_id))) %>% 
  
  bind_rows(df_updated %>% 
              select(species, gbif_id, common_name, class) %>% 
              anti_join(syns_df)) %>% 
  select(-acc_class)

# save
saveRDS(df_updated2, 'data/occurences/df_spp_occ_syns_2.rds')

## 4 of 6 syns are now duplicated species
gbif_ids = df_updated2 %>% distinct(gbif_id) %>% pull()  


# Loop through all spp, if over 10k records, then don't download
spp_list = list(); d = 1
over = list(); e = 1
for(i in gbif_ids[138:length(gbif_ids)]){
  
  # check n occ
  n_occs = occ_count(taxonKey = i, georeferenced=TRUE)
  
  # download if less than 10k
  if(n_occs <= 10000 & n_occs > 0){
    temp = occ_data(i, 
             limit = 10000,
             hasCoordinate = TRUE,
             hasGeospatialIssue = FALSE)
    
    spp_list[[d]] =  temp$data %>% 
      filter(occurrenceStatus == "PRESENT") %>% 
      mutate( gbif_id = i, .before = key) %>% 
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    message(d)
    d = d + 1
    
  }else{
    # save spp in not
    over[[e]] = i
    e = e + 1
  }
  
}


# get no hits
over_spp = 
  df_updated2 %>% 
  filter(gbif_id %in% unlist(over))

# save 
saveRDS(spp_list, "data/occurences/spp_under_10k_occ.rds")
saveRDS(over_spp, "data/occurences/spp_over_10k_occ_df.rds")
write.csv(over_spp, "data/occurences/spp_over_10k_occ_df.csv", row.names = F)

# save each as own file?
library(glue)
for(item in spp_list){
  saveRDS(item, glue("data/occurences/species/{item$gbif_id[1]}.rds"))
}



# try get more
over_spp

## get n occ
over_n_ooc = c(); d = 1
for(i in over_spp$gbif_id){
  
  # check n occ
  over_n_ooc[d] = occ_count(taxonKey = i, georeferenced=TRUE)
  d = d + 1
  
}

over_spp = over_spp %>% mutate(n_occ = over_n_ooc, .before=species)


# try to get 100k and under
gbif_ids2 = over_spp %>% 
  arrange(n_occ) %>% 
  filter(n_occ <= 60000 & n_occ > 0) %>% 
  pull(gbif_id)


# get remaining spp for manual download
over_spp2 = over_spp %>% filter(!gbif_id %in% gbif_ids2)
write.csv(over_spp2, "data/occurences/spp_over_60k_occ_df.csv")


# Loop through all spp, if over 100k records, then don't download
spp_list2 = list(); d2 = 1
for(i in gbif_ids2){
    temp = occ_data(i, 
                    limit = 60000,
                    hasCoordinate = TRUE,
                    hasGeospatialIssue = FALSE)
    
    spp_list2[[d2]] =  temp$data %>% 
      filter(occurrenceStatus == "PRESENT") %>% 
      mutate( gbif_id = i, .before = key) %>% 
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    message(d2)
    d2 = d2 + 1
}

# save 
saveRDS(spp_list2, "data/occurences/spp_under_60k_occ.rds")
for(item in spp_list2){
  saveRDS(item, glue("data/occurences/species/{item$gbif_id[1]}.rds"))
}


# get and save n occ
df_spp_w_occ = 
  df_updated2 %>% 
  mutate(n_occ_gbif = map_dbl(gbif_id, ~occ_count(taxonKey = .x, georeferenced=TRUE)))

saveRDS(df_spp_w_occ, "data/occurences/df_spp_occ_syns.rds")
