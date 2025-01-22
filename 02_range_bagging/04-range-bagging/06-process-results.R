library(raster)
library(tidyverse)
library(sf)
library(glue)
library(mapview)

# gt data
gt = read_rds('data/_final_gt_with_oor.rds')
all_spp_n = gt %>% distinct(species) %>% pull() %>% length()
spp_outside = gt %>% filter(outside_range == 1) %>% distinct(species) %>% pull()

# sample occ size df
df_n = read_rds('data/range_bagging/df_spp_occ_n.rds') %>% 
  filter(species %in% spp_outside)
spp_rm = df_n %>% 
  filter(n_occ < 100 )

# get data
gbif_ids = list.files('data/range_bagging/pca/')
gbif_ids = gbif_ids[!gbif_ids %in% spp_rm$gbif_id]

# get results into one df
results = map(gbif_ids, ~read_rds(glue('data/range_bagging/pca/{.x}/sf_results.rds')))
df = bind_rows(results)
n_distinct(df$gbif_id)

# ## cast to multi poly
# df = df %>% st_cast("MULTIPOLYGON")

# summarize by spp
df_s1 =
  df %>%
  st_drop_geometry() %>%
  filter(traded_oor_state_suitable_oor == 1) %>%
  group_by(gbif_id) %>%
  summarise(n_states_traded_oor_state_suitable_oor = sum(traded_oor_state_suitable_oor),
            states = paste(STE_NAME16, collapse = ", ")) %>% 
  full_join(df_n %>% filter(n_occ>=100) %>% dplyr::select(species:class, gbif_id)) %>% 
  dplyr::select(gbif_id, species:class, 
         everything()) %>% 
  mutate(n_states_traded_oor_state_suitable_oor = 
           ifelse(is.na(n_states_traded_oor_state_suitable_oor), 
                  0, n_states_traded_oor_state_suitable_oor))
# 
hist(df_s1$n_states_traded_oor_state_suitable_oor, 9)

saveRDS(df_s1, 'data/range_bagging/summary_spp_traded_suitable.rds')
write.csv(df_s1, 'data/range_bagging/summary_spp_traded_suitable.csv', row.names = F)


df_s1 %>% 
  count(n_states_traded_oor_state_suitable_oor)
mean(df_s1$n_states_traded_oor_state_suitable_oor)
