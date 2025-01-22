library(raster)
library(tidyverse)
library(sf)
library(mapview)

# figure out how to calculate results

# match species range distrib to RB suitability
# buffer 200km (take native only?)
# look at suitability outside range
# See if species was sold in state with suitable area (and how many cells/km) or prop of state
# See if suitable area was within x km of inhabited area



# 1. match species range files to gbif id

## Load in spp that i did RB on
spp_all = read_rds('data/occurences/df_spp_occ_syns_2.rds')
spp = spp_all %>% distinct(gbif_id, .keep_all = TRUE)
spp %>% count(class)


## load in ranges
bird_distribs = read_rds('data/spp_geometries/birds_subset.rds')
amph_distribs = read_rds("data/spp_geometries/amphibs_subset.rds")
rept_distribs = read_rds('data/spp_geometries/reptiles_subset.rds')

bird_key = read_rds('data/spp_geometries/birds_gt_geom_key.rds')
amph_key = read_rds('data/spp_geometries/amphibs_gt_geom_key.rds')
rept_key = read_rds('data/spp_geometries/reptiles_gt_geom_key.rds')


# ----

## see what species are in and not in distribs

### birds
spp %>% 
  filter(class=="Aves") %>% 
  left_join(bird_key) %>% 
  filter(!species2 %in% bird_distribs$binomial)

#### update bird key
bird_key2 = 
  bird_key %>% 
  mutate(species = 
           case_when(species2 == "Eolophus roseicapilla" ~ "Eolophus roseicapilla",
                     species2 == "Trichoglossus moluccanus" ~ "Trichoglossus haematodus",
                     TRUE ~ species))
#### recheck
spp %>% 
  filter(class=="Aves") %>% 
  left_join(bird_key2) %>% 
  filter(!species2 %in% bird_distribs$binomial)

#### add in gbif_id to key
bird_key3 = 
  bird_key2 %>% 
  left_join(spp %>% filter(class=="Aves") %>% select(species, gbif_id)) %>% 
  rename(gbif_spp = species, distrib_spp = species2)


#### last check
bird_distribs %>% 
  st_drop_geometry() %>% 
  left_join(bird_key3, by = c("binomial"="distrib_spp")) -> temp

#### save
saveRDS(bird_key3, "data/spp_geometries/birds_gbif_distrib_key.rds")


#----

### amphibs
spp %>% 
  filter(class=="Amphibia") %>% 
  left_join(amph_key) %>% 
  filter(!species2 %in% amph_distribs$binomial)

#### all good so just add in gbif id and proceed
amph_key2 = 
  amph_key %>% 
  left_join(spp %>% filter(class=="Amphibia") %>% select(species, gbif_id)) %>% 
  rename(gbif_spp = species, distrib_spp = species2)

saveRDS(amph_key2, "data/spp_geometries/amphib_gbif_distrib_key.rds")


# ----

### reptiles
spp %>% 
  filter(class=="Reptilia") %>% 
  left_join(rept_key) %>% 
  filter(!species2 %in% rept_distribs$Binomial)

rept_key2 = 
  rept_key %>% 
  mutate(species = 
           case_when(species2 == "Myuchelys purvisi" ~ "Flaviemys purvisi",
                     TRUE ~ species)) %>% 
  left_join(spp %>% filter(class=="Reptilia") %>% select(species, gbif_id)) %>% 
  rename(gbif_spp = species, distrib_spp = species2) %>% 
  mutate(gbif_spp = ifelse(distrib_spp=="Flaviemys purvisi", 
                           "Myuchelys purvisi", gbif_spp)) %>% 
  mutate(gbif_spp = ifelse(distrib_spp=="Intellagama lesueurii", 
                           "Intellagama lesueurii", gbif_spp)) %>% 
  mutate(gbif_id = ifelse(distrib_spp=="Flaviemys purvisi", 7009386, gbif_id)) %>% 
  mutate(gbif_id = ifelse(distrib_spp=="Intellagama lesueurii", 8161292, gbif_id)) %>% 
  filter(!is.na(gbif_id)) %>% distinct()
  

# last check
rept_distribs %>% 
  st_drop_geometry() %>% 
  left_join(rept_key2, by = c("Binomial"="distrib_spp")) %>% 
  filter(!gbif_spp %in% (spp %>% filter(class=="Reptilia") %>% pull(species)))

spp %>% 
  filter(class=="Reptilia") %>% 
  left_join(rept_key2) %>% 
  filter(!distrib_spp %in% rept_distribs$Binomial)

rept_key2 %>% 
  filter(!gbif_spp %in% 
           ((spp %>% filter(class=="Reptilia") %>% pull(species))))

n_distinct(rept_key2$gbif_spp)  

saveRDS(rept_key2, "data/spp_geometries/reptile_gbif_distrib_key.rds")
