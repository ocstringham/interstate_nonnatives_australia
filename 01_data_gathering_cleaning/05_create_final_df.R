# Script to clean up final 'out of range' dataset

library(sf)
library(leaflet)
library(mapview)
library(tidyverse)
library(tidylog)


# ---------------------------------------------------------------------------- #


# load in data
suburbs = read_rds('data/suburb_geometries/website_suburb_geometries.rds')
suburbs = st_transform(suburbs, crs = 3112)

# load in results
reptiles_results = read_rds('data/raw_results/reptiles_raw.rds')
amphibs_results = read_rds('data/raw_results/amphibs_raw.rds')
birds_results = read_rds('data/raw_results/birds_raw.rds')


# load in GT data
gt_birds = readRDS('data/market/bird_listings.rds')
gt_herps = readRDS('data/market/herp_listings.rds')

## quick fix for Intellagama lesueurii
gt_herps = 
  gt_herps %>% 
  mutate(species = ifelse(species == "Itellagama lesueurii", "Intellagama lesueurii", species))


# updates syns
syns = read_rds('data/occurences/df_spp_occ_syns_2.rds') %>% filter(!is.na(syn_gbif_id))

## rename common name
syns2 = 
  syns %>% 
  mutate(common_name = case_when(species == "Trichoglossus haematodus" ~ "Rainbow lorikeet",
                                 species == "Antaresia childreni" ~ "Children's python",
                                 TRUE ~ common_name)) %>% 
  mutate(genus = str_extract(species, "[A-z]+")) %>% 
  bind_rows(tibble(species = "Antaresia childreni", 
                   gbif_id = "2465003", common_name = "Children's python",
                   syn_species = "Antaresia stimsoni", syn_gbif_id = "6162835", 
                   class = "Reptilia", genus = "Antaresia"))

# replace all syns
replace_syns_fun = function(df, syns_df){
  df2 = df
  for(i in 1:nrow(syns_df)){
    df2 = 
      df2 %>% 
      mutate(species = if_else(gbif_id == syns_df$syn_gbif_id[i], syns_df$species[i], species),
             common_name = if_else(gbif_id == syns_df$syn_gbif_id[i], syns_df$common_name[i], common_name),
             genus = if_else(gbif_id == syns_df$syn_gbif_id[i], syns_df$genus[i], genus),
             gbif_name = if_else(gbif_id == syns_df$syn_gbif_id[i], syns_df$species[i], species),
             gbif_id = if_else(gbif_id == syns_df$syn_gbif_id[i], syns_df$gbif_id[i], gbif_id) # this one needs to be last
      )
  }
  return(df2)
}

## run fun
gt_birds2 = replace_syns_fun(gt_birds, syns2)
gt_herps2 = replace_syns_fun(gt_herps, syns2)





# ---------------------------------------------------------------------------- #


# combine but resolve cols

colnames(gt_birds2)
colnames(gt_herps2)

colnames(gt_birds2)[!colnames(gt_birds2) %in% colnames(gt_herps2)]
colnames(gt_herps2)[!colnames(gt_herps2) %in% colnames(gt_birds2)]

gt_all = bind_rows(gt_birds2 %>% 
                     rename(quantity_unspecified = qty_unspecified,
                            price_per_unit = price_final_per_unit,
                            quantity = qty) %>% 
                     select(-person, -native.species),
                   gt_herps2 %>% rename(data_page = page, 
                                       page_row_num = row) %>% 
                     select(-scientific_name, -price, -price_quantity)
                   ) %>% 
  mutate(across(where(is.character), str_trim))


# Join in distrib data
all_results_raw = bind_rows(
  bind_rows(birds_results) %>% select(classifieds_unique_listing_id, species, state, suburb, outside_range),
  bind_rows(reptiles_results)%>% select(classifieds_unique_listing_id, species, state, suburb, outside_range),
  bind_rows(amphibs_results)%>% select(classifieds_unique_listing_id, species, state, suburb, outside_range)) %>% 
  mutate(across(where(is.character), str_trim)) %>% 
  left_join(st_drop_geometry(suburbs), by = c('state' = 'state_website', 'suburb' = 'suburb_website')) %>% 
  select(classifieds_unique_listing_id, species, outside_range, STE_CODE16:SSC_NAME16) %>% 
  distinct() ### Needed bc some subspp, repeated spp in same ad


## check if id/species is unique
all_results_raw %>% 
  group_by(classifieds_unique_listing_id, species) %>% 
  filter(n()>1)

gt_all %>% 
  group_by(classifieds_unique_listing_id, species) %>% 
  filter(n()>1)


## Join
df_all = 
  gt_all %>% 
  left_join(all_results_raw, by = c("classifieds_unique_listing_id", "species"))

check1 = 
  gt_all %>% 
  anti_join(all_results_raw, by = c("classifieds_unique_listing_id", "species"))


### Zebra finch and one water dragon didn't match .... investigate further into the names/geoms
## OK all good now. 

# ---------------------------------------------------------------------------- #

# Reorganize cols and Export
df_all %>% distinct(gbif_rank)


df_all %>% colnames()


df_all2 = 
  df_all %>% 
  tibble() %>% 
  select(classifieds_unique_listing_id, page = data_page, row = page_row_num,
         species, common_name, outside_range, STE_CODE16:SSC_NAME16,
         gbif_id, gbif_name, gbif_rank, class, order, family, genus, subspecies, form,
         quantity, quantity_unspecified, price_per_unit, price_includes_other_items, 
         hand_raised, wild_caught, morph)


df_all2 = df_all2 %>% filter(species != "Varanus timorensis")

# Export
saveRDS(df_all2, 'data/_final_gt_with_oor.rds')
# df_all2 = read_rds('data/_final_gt_with_oor.rds')

# # # n spp, n ads
# gt_all %>%
#   filter(!is.na(state)) %>%
#   summarise(n_row = n(),
#             n_ads = n_distinct(classifieds_unique_listing_id),
#             n_spp = n_distinct(species, na.rm = TRUE),
#             n_taxa = n_distinct(gbif_name))


# # ---------------------------------------------------------------------------- #
# 
# # add in synonym species
# # df_all2 = readRDS('data/_final_gt_with_oor.rds')
# syns = read_rds('data/occurences/df_spp_occ_syns_2.rds') %>% filter(!is.na(syn_gbif_id))
# 
# ## rename common name
# syns2 = 
#   syns %>% 
#   mutate(common_name = case_when(species == "Trichoglossus haematodus" ~ "Rainbow lorikeet",
#                                  species == "Antaresia childreni" ~ "Children's python",
#                                  TRUE ~ common_name)) %>% 
#   mutate(genus = str_extract(species, "[A-z]+")) %>% 
#   bind_rows(tibble(species = "Antaresia childreni", 
#                    gbif_id = "2465003", common_name = "Children's python",
#                    syn_species = "Antaresia stimsoni", syn_gbif_id = "6162835", 
#                    class = "Reptilia", genus = "Antaresia"))
# 
# # test = df_all2 %>% 
# #   filter(gbif_id %in% syns$syn_gbif_id) %>% 
# #   distinct(species, common_name, gbif_id)
# 
# # replace all syns
# df_all3 = df_all2
# for(i in 1:nrow(syns2)){
#   df_all3 = 
#     df_all3 %>% 
#     mutate(species = if_else(gbif_id == syns2$syn_gbif_id[i], syns2$species[i], species),
#            common_name = if_else(gbif_id == syns2$syn_gbif_id[i], syns2$common_name[i], common_name),
#            genus = if_else(gbif_id == syns2$syn_gbif_id[i], syns2$genus[i], genus),
#            gbif_name = if_else(gbif_id == syns2$syn_gbif_id[i], syns2$species[i], species),
#            gbif_id = if_else(gbif_id == syns2$syn_gbif_id[i], syns2$gbif_id[i], gbif_id) # this one needs to be last
#            )
#   
# }
# 
# ## update common names
# df_all3 = df_all3 %>% 
#   mutate(common_name = ifelse(species == "Chelodina oblonga", 
#                               "Narrow-breasted Snake-necked Turtle", common_name))
# 
# df_all3 %>% 
# filter(gbif_id %in% syns2$syn_gbif_id) %>% 
#   distinct(species, common_name, gbif_id)
# 
# df_all3 %>% 
#   filter(species %in% syns2$species) %>% 
#   distinct(species, common_name, gbif_id, genus, gbif_name)
# 
# 
# n_distinct(df_all2$species)
# n_distinct(df_all3$species)
# 
# 
# # save
# saveRDS(df_all3, 'data/_final_gt_with_oor.rds')
# 
