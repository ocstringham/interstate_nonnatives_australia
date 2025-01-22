library(raster)
library(tidyverse)
library(rgbif)
library(sf)
library(mapview)


# Get list of species (gbif)
df = read_rds('data/_final_gt_with_oor.rds') %>% 
  select(species, common_name, gbif_id:subspecies) %>% 
  distinct() %>% 
  filter(gbif_rank %in% c("species", "subspecies"))

# Are all subspecies also recorded as species?
df %>% 
  filter(gbif_rank == "subspecies") %>% 
  distinct(species) %>% 
  filter(!species %in% (df %>% filter(gbif_rank == "species") %>% pull(species)))

# add in row
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




# K , select one to play around
spp_name = "Tiliqua nigrolutea"
spp = df_updated %>% filter(species == spp_name)

# takes a min
test = occ_data(spp$gbif_id, limit = 1e6,
                hasCoordinate = TRUE,
                hasGeospatialIssue = FALSE)

# check end of records, if notincrease limit or maybe need to get api key
occ = test$data %>% 
  filter(occurrenceStatus == "PRESENT") %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

mapview(occ)

# get points within native range ... make sure native ranges extends beyond Aus, if applicable
reptile_distribs = read_rds("data/spp_geometries/reptiles_subset.rds")
# reptile_key = read_rds("data/spp_geometries/reptiles_gt_geom_key.rds")

spp_distrib = reptile_distribs %>% filter(Binomial == spp_name)
sf::st_crs(spp_distrib) = 4326
mapview(spp_distrib) + mapview(occ)


# clip to native range (add 100km buffer)
occ_clip = occ %>% 
  st_intersection(spp_distrib %>% 
                    st_transform(crs = 3112) %>% 
                    st_buffer(200000) %>% # 200km
                    st_transform(4326))

mapview(occ_clip) + mapview(spp_distrib)



# check distribs
birds_distribs = read_rds('data/spp_geometries/birds_subset.rds')
test = birds_distribs %>% filter(binomial == "Eclectus polychloros")
sf::st_crs(test) = 4326
mapview(test)



# check spp with many observations
spp_name = "Eolophus roseicapilla"
spp = df_updated %>% filter(species == spp_name)
test = occ_data(spp$gbif_id, limit = 1e6,
                hasCoordinate = TRUE,
                hasGeospatialIssue = FALSE)
sf::st_crs(test) = 4326
mapview(test)