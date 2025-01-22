
library(tidyverse)
library(readxl)
library(taxize)
library(rgbif)


# load in gt
gt = readRDS('data/_final_gt_with_oor.rds')
spp = gt %>% 
  filter(gbif_rank == "species") %>% # ?
  distinct(species, class, gbif_id) %>% arrange(class) %>% filter(class!="Aves")

# Load in kraus
kraus = read_excel('data/establishments/kraus2009.xls')


# get spp
kraus %>% count(`Success?`)
kraus_spp = kraus %>% 
  filter(`Success?` =="Y") %>% 
  distinct(TaxonCode)


# get gbif name

rgbif::name_backbone(kraus_spp$TaxonCode[2]) -> temp
test = name_backbone("dfdasfd")

check_gbif = map_df(kraus_spp$TaxonCode, ~name_backbone(.x))


# get exact matches
match1 = check_gbif %>% 
  filter(matchType =="EXACT")

unmatch1 = 
  check_gbif %>% 
  filter(matchType !="EXACT")

## only aussue unmatched is Saproscincus mustelinus, but not in trade?


# make key
kraus_key = 
  check_gbif %>% 
  select(TaxonCode = verbatim_name, species, gbif_id = speciesKey) %>% 
  distinct() %>% 
  filter(!is.na(species)) %>% 
  mutate(gbif_id = as.character(gbif_id))



# check if any species sold are in kraus
estab1 = 
  kraus %>% 
  filter(`Success?` =="Y") %>% 
  filter( TaxonCode %in%
            (  spp %>% 
              filter(species %in% kraus_key$species) %>% 
                left_join(kraus_key) %>% 
              pull(species))
  ) %>% 
  left_join(kraus_key) %>% 
  select(species, gbif_id, everything())


# save
saveRDS(estab1, "data/establishments/herps/kraus_herps.rds")
