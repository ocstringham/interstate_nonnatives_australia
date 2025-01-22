# Script to resolve synonym names because by the time I first ran the analysis to writing up the paper some species have split into mulitple species 
# I did this after running the workflow once and then reran everything again

library(tidyverse)
library(sf)

## load ads data (with updated taxa from synonyms)
df = readRDS('data/_final_gt_with_oor.rds')

## load in geometries
distribs = read_rds("data/range_bagging/distribs_gbifid.rds")


### add in names
sn_cn = df %>% distinct(gbif_id, species, class)

distribs2 = distribs %>% left_join(sn_cn) %>% st_transform(crs = 3112)
distribs2 = distribs2 %>% select(gbif_id, species, class, geometry)


# save
saveRDS(distribs2, "data/spp_geometries/_all_spp_geoms_v1.rds")
