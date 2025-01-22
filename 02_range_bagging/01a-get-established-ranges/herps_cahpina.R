library(tidyverse)
library(readxl)
library(taxize)
library(rgbif)


# load in gt
gt = readRDS('data/_final_gt_with_oor.rds')
spp = gt %>% 
  filter(gbif_rank == "species") %>% # ?
  distinct(species, class, gbif_id) %>% arrange(class) %>% filter(class!="Aves")

# Load in cahpina
ca = read_excel('data/establishments/Cahpina et al 2017 supporting data.xlsx', sheet = 2)


# get spp
ca_spp = ca %>% 
  filter(!is.na(Species)) %>% 
  distinct(Species)

# get gbif name
check_gbif = map_df(ca_spp$Species, ~name_backbone(.x))


# get exact matches
match1 = check_gbif %>% 
  filter(matchType =="EXACT")

unmatch1 = 
  check_gbif %>% 
  filter(matchType !="EXACT")

## all unmatches are NON aussie, can proceed without them

# make key
ca_key = 
  check_gbif %>% 
  select(Species = verbatim_name, species, gbif_id = speciesKey) %>% 
  distinct() %>% 
  filter(!is.na(species)) %>% 
  mutate(gbif_id = as.character(gbif_id))


# check if any species sold are in ca
estab1 = 
  ca %>% 
  filter(!is.na(Species)) %>% 
  filter( Species %in%
            (  spp %>% 
                 filter(species %in% ca_key$species) %>% 
                 left_join(ca_key) %>% 
                 pull(species))
  ) %>% 
  left_join(ca_key) %>% 
  select(species, gbif_id, everything())


# save
saveRDS(estab1, "data/establishments/herps/cahpina_herps.rds")
