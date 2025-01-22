# Script to get the native species ranges for the species in the market clipped to Australia.

library(sf)
library(rnaturalearth)
library(tidyverse)
library(mapview)
library(rnaturalearth)
library(tidylog)

# load in species native range geometries
reptiles = st_read('C:/data/spp_distributions/GARD1.1_dissolved_ranges/modeled_reptiles.shp')
amphibians = st_read('C:/data/spp_distributions/amphibians/AMPHIBIANS.shp')

birds = read_rds('C:/data/spp_distributions/botw_2020/BOTW/botw_sf.rds') # should prob get updated bird geoms
# birds = birds %>% mutate_if(is.factor, as.character)
birds = birds %>% mutate(row_id = row_number(), .before = SISID)

# load in Market data
gt_birds = readRDS('data/market/bird_listings.rds')

gt_herps = readRDS('data/market/herp_listings.rds')
gt_reptiles = gt_herps %>% filter(class == "Reptilia")
gt_amphibians = gt_herps %>% filter(class == "Amphibia")


# match species

## Market species
gt_bird_spp = gt_birds %>% distinct(species)
gt_reptile_spp = gt_reptiles %>% distinct(species)
gt_amphibs_spp = gt_amphibians %>% distinct(species)


View(st_drop_geometry(reptiles))
View(st_drop_geometry(amphibians))
View(st_drop_geometry(birds))

## get species names not in geometries
missing_birds = gt_bird_spp %>% filter(!species %in% birds$binomial)
missing_reptiles = gt_reptile_spp %>% filter(!species %in% c(reptiles$Binomial))
missing_amphibs = gt_amphibs_spp %>% filter(!species %in% c(amphibians$binomial))


## create key for species that don't line up
missing_birds_key = tribble(~species, ~species2,
                            "Cacatua roseicapilla", "Eolophus roseicapilla",
                            "Lophochroa leadbeateri", "Cacatua leadbeateri", 
                            "Psephotus dissimilis", "Psephotellus dissimilis",
                            "Erythrura gouldiae", "Chloebia gouldiae",
                            "Psephotus varius", "Psephotellus varius", 
                            "Psephotus chrysopterygius", "Psephotellus chrysopterygius",
                            "Calyptorhynchus funereus", "Zanda funerea",
                            "Coturnix ypsilophora", "Synoicus ypsilophorus"
                            )


missing_reptiles_key = tribble(~species, ~species2,
                           "Lophosaurus spinipes", "Hypsilurus spinipes",
                           "Gowidon longirostris", "Lophognathus longirostris",
                           "Lophosaurus boydii", "Hypsilurus boydii",
                           "Cyclodomorphus gerrardii", "Hemisphaeriodon gerrardii",
                           "Itellagama lesueurii", "Intellagama lesueurii")


missing_amphibs_key = tribble(~species, ~species2,
                              "Ranoidea chloris", "Litoria chloris",
                              "Ranoidea caerulea", "Litoria caerulea",
                              "Ranoidea gilleni", "Litoria gilleni",
                              "Ranoidea aurea", "Litoria aurea",
                              "Ranoidea moorei", "Litoria moorei",
                              "Ranoidea gracilenta", "Litoria gracilenta",
                              "Ranoidea splendida", "Litoria splendida")



# edit key as needed
gt_bird_spp = 
  gt_bird_spp %>% 
  left_join(missing_birds_key) %>% 
  mutate(species2 = ifelse(is.na(species2), species, species2)) %>% 
  mutate(species2 = case_when(species == "Taeniopygia guttata" ~ "Taeniopygia castanotis", # zebra finch
                              species == "Eclectus roratus" ~"Eclectus polychloros", #elcletus parrot
                              TRUE ~ species2))

gt_reptile_spp = 
  gt_reptile_spp %>% 
  left_join(missing_reptiles_key) %>% 
  mutate(species2 = ifelse(is.na(species2), species, species2))

gt_amphibs_spp = 
  gt_amphibs_spp %>% 
  left_join(missing_amphibs_key) %>% 
  mutate(species2 = ifelse(is.na(species2), species, species2))


# double check all names are in it
missing_birds2 = gt_bird_spp %>% filter(!species2 %in% birds$binomial)
missing_reptiles2 = gt_reptile_spp %>% filter(!species2 %in% c(reptiles$Binomial))
missing_amphibs2 = gt_amphibs_spp %>% filter(!species2 %in% c(amphibians$binomial))


# subset geometries for desired spp
reptiles_subset = reptiles %>% filter(Binomial %in% gt_reptile_spp$species2)
birds_subset = birds %>% filter(binomial %in% gt_bird_spp$species2)
amphibians_subset = amphibians %>% filter(binomial %in% gt_amphibs_spp$species2)




# Remove polygons that don't intersect with Australia (birds only)

## Australia geometry
countries = ne_download(scale = 'large', returnclass = 'sf')
aus = countries %>% filter(SOVEREIGNT == "Australia") # %>% st_transform(crs = 3112)
mapview(aus)

## project to same projection

### also buffer 50km
aus_buf = aus %>% st_transform(crs = 3112) %>% st_buffer(50000)
mapview(aus_buf)

birds_reproj = birds_subset %>% st_transform(crs = 3112)
# reptiles_reproj = reptiles_subset %>% st_transform(crs = 3112)
# amphibians_reproj = amphibians_subset %>% st_transform(crs = 3112)


## get difference
birds_intersect = st_intersection(birds_reproj, aus_buf)
birds_diff = birds_reproj %>% 
  filter(!row_id %in% birds_intersect$row_id)

mapview(birds_diff, zcol = "binomial")
View(st_drop_geometry(birds_diff))

## remove
birds_subset2 = 
  birds_subset %>% 
  filter(row_id %in% birds_intersect$row_id)


View(birds_subset2 %>% st_drop_geometry())


# check if some species where not matched
gt_bird_spp %>% filter(!species2 %in% birds_subset2$binomial)

birds %>% 
  filter(binomial == "Taeniopygia castanotis") %>% # zebra finch we call "Taeniopygia guttata"
  mapview()


birds %>% 
  filter(binomial == "Eclectus polychloros") %>%  # ecletus parrot we call "Eclectus roratus"
  mapview()


# final checks
gt_reptile_spp %>% filter(!species2 %in% reptiles_subset$Binomial)
gt_reptile_spp %>% group_by(species2) %>% filter(n()>1)

gt_amphibs_spp %>% filter(!species2 %in% amphibians_subset$binomial)


# save australian subset of ranges
saveRDS(birds_subset2, 'data/spp_geometries/birds_subset.rds')
saveRDS(reptiles_subset, 'data/spp_geometries/reptiles_subset.rds')
saveRDS(amphibians_subset, 'data/spp_geometries/amphibs_subset.rds')

# save key to ranges
saveRDS(gt_bird_spp, 'data/spp_geometries/birds_gt_geom_key.rds')
saveRDS(gt_reptile_spp, 'data/spp_geometries/reptiles_gt_geom_key.rds')
saveRDS(gt_amphibs_spp, 'data/spp_geometries/amphibs_gt_geom_key.rds')
