library(raster)
library(tidyverse)
library(sf)
library(glue)
library(mapview)


# get df of spp, n obs, n pixels suitable

# clean data

## rm spp with under 100 pts
spp_occ_list = list.files("data/occurences/clean/", full.names = T)
spp_occ = map(spp_occ_list, ~read_rds(.x)) # takes minutes
spp_occ_n = map_dbl(spp_occ, ~nrow(.x))
rm(spp_occ)

# ### save
# saveRDS(spp_occ_n, "data/occurences/spp_occ_n.rds")
# spp_occ_n = readRDS("data/occurences/spp_occ_n.rds")
# 
# ### load in points to rm
# pt_rm = readRDS('data/occurences/rm_pts_outside/df_inds_outside_to_rm_ALL.rds')
# pt_rm = pt_rm %>% mutate(n_rm = map_dbl(inds_to_rm, ~length(.x))) %>% select(-inds_to_rm)
# 
spp_occ_id  = spp_occ_list %>% str_split("/") %>%
  map_chr(., ~.x[length(.x)]) %>%
  str_extract("[0-9]+")


## to df
df_n_occ = tibble(gbif_id = spp_occ_id, n_occ = spp_occ_n) 

# rm(spp_occ)


## rm spp with no preds from RB

# load in rb preds
rb_list = list.files("data/range_bagging/pca/", full.names = T)

rb = map(rb_list, ~read_rds(glue("{.x}/r_aus_pred_rc.rds")))
sum(rb[[1]]@data@values, na.rm = T)
rb_n = map_dbl(rb, ~sum(.x@data@values, na.rm = T))
rb_id = rb_list %>%  str_split("/") %>% 
  map_chr(., ~.x[length(.x)])
rb_df = tibble(gbif_id = rb_id, n_suit_pix = rb_n)


## add in spp name and other chars
df2 = df_n_occ %>% 
  left_join(rb_df)

df3 = read_rds('data/occurences/df_spp_occ_syns_2.rds')
df3a = df3 %>% distinct(species, gbif_id, class)

df4 = 
  df2 %>% 
  left_join(df3a) %>% 
  mutate(n_occ = ifelse(is.na(n_occ), 0, n_occ))

## add in traded outside
gt = read_rds('data/_final_gt_with_oor.rds')
gt2  = gt %>% filter(outside_range == 1) %>% distinct(species) %>% mutate(oor = 1)


df5 = 
  df4 %>% 
  left_join(gt2) %>% 
  mutate(oor = ifelse(is.na(oor), 0, oor))

# save
saveRDS(df5, 'data/range_bagging/df_spp_occ_n.rds')
