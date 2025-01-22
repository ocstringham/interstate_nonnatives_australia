# Script to compare the location of sale with the native range and export all ads that are outside the native range.

library(sf)
library(tidyverse)
# library(tidylog)
library(leaflet)
library(htmltools)
library(mapview)
library(rnaturalearth)

# ---------------------------------------------------------------------------- #

# load in function
source('scripts/functions/detect_out_of_range_fun.R')

# load in data

## Australia geometry
countries = ne_download(scale = 'medium', returnclass = 'sf')
aus = countries %>% 
  filter(SOVEREIGNT == "Australia") %>% 
  summarise() %>% 
  st_transform(crs = 3112)
mapview(aus)


## load in spp geometries
distribs = read_rds('data/spp_geometries/_all_spp_geoms_v2.rds')


## simpligy boiga to aus bc taking too much RAM
distribs$geometry[distribs$species=="Boiga irregularis"] = 
  distribs %>% 
  filter(species == "Boiga irregularis") %>% 
  st_intersection(aus) %>% 
  st_geometry()

# distribs %>%
#   filter(species == "Boiga irregularis") %>%
#   mapview()


## simplify birds because take too long
bird_spp = distribs %>% st_drop_geometry() %>% 
  filter(class=="Aves") %>% distinct(species) %>% pull()

for(spp in bird_spp){
  distribs$geometry[distribs$species==spp] = 
    distribs %>% 
    filter(species == spp) %>%
    st_intersection(aus) %>% 
    st_geometry()
  print(spp)
}
rm(spp, bird_spp)

# birds_geom = read_rds('data/spp_geometries/birds_subset.rds')
# reptiles_geom = read_rds('data/spp_geometries/reptiles_subset.rds')
# amphibs_geom = read_rds('data/spp_geometries/amphibs_subset.rds')
# 
# ## load in geom keys
# birds_gt_geom_key = read_rds('data/spp_geometries/birds_gt_geom_key.rds')
# reptiles_gt_geom_key = read_rds('data/spp_geometries/reptiles_gt_geom_key.rds')
# amphibs_gt_geom_key = read_rds('data/spp_geometries/amphibs_gt_geom_key.rds')
# 
# ### double check for duplicated spp
# birds_gt_geom_key %>% group_by(species2) %>% filter(n()>1)
# reptiles_gt_geom_key %>% group_by(species2) %>% filter(n()>1)
# amphibs_gt_geom_key %>% group_by(species2) %>% filter(n()>1)
# 
# ---------------------------------------------------------------------------- #

# load in GT data
gt_birds = readRDS('data/market/bird_listings.rds')
gt_herps = readRDS('data/market/herp_listings.rds')
gt_reptiles = gt_herps %>% filter(class == "Reptilia")
gt_amphibians = gt_herps %>% filter(class == "Amphibia")

## quick fix for Intellagama lesueurii
gt_reptiles =
  gt_reptiles %>%
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
gt_reptiles2 = replace_syns_fun(gt_reptiles, syns2)
gt_amphibians2 = replace_syns_fun(gt_amphibians, syns2)


## update common names
gt_reptiles2 = gt_reptiles2 %>% 
  mutate(common_name = ifelse(species == "Chelodina oblonga", 
                              "Narrow-breasted Snake-necked Turtle", common_name))

gt_birds2 %>% 
  filter(gbif_id %in% syns2$syn_gbif_id) %>% 
  distinct(species, common_name, gbif_id)

gt_birds2 %>% 
  filter(species %in% syns2$species) %>% 
  distinct(species, common_name, gbif_id, genus, gbif_name)


gt_reptiles2 %>% 
  filter(gbif_id %in% syns2$syn_gbif_id) %>% 
  distinct(species, common_name, gbif_id)

gt_reptiles2 %>% 
  filter(species %in% syns2$species) %>% 
  distinct(species, common_name, gbif_id, genus, gbif_name)


n_distinct(gt_birds$species)
n_distinct(gt_birds2$species)

n_distinct(gt_reptiles$species)
n_distinct(gt_reptiles2$species)


# ---------------------------------------------------------------------------- #

## load in suburbs geometries
suburbs = read_rds('data/suburb_geometries/website_suburb_geometries.rds')

# ---------------------------------------------------------------------------- #
# 
# # Make all geometries in same projection ... Lambert Conformal Conic
# # Need to do this in order for any spatial operations to work
# 
# birds_geom = st_transform(birds_geom, crs = 3112)
# 
# reptiles_geom = st_transform(reptiles_geom, crs = 3112)
# 
# amphibs_geom = st_transform(amphibs_geom, crs = 3112)
# 
# suburbs = st_transform(suburbs, crs = 3112)

# ---------------------------------------------------------------------------- #

# subset ranges to get native distribs only

# ## View the data
# View(st_drop_geometry(birds_geom))
# View(st_drop_geometry(reptiles_geom))
# View(st_drop_geometry(amphibs_geom))
# 
# 
# birds_geom_native = 
#   birds_geom %>% 
#   filter(origin == 1)
# 
# reptiles_geom_native = reptiles_geom
# 
# amphibs_geom_native = 
#   amphibs_geom %>% 
#   filter(origin == 1)
# 
# distribs %>% 
#   filter(species == "Trichoglossus haematodus") %>% 
#   mapview()

# ---------------------------------------------------------------------------- #


# convert suburbs to point (centroids) for analysis
suburbs_point = suburbs %>% st_centroid() %>% st_transform(crs = 3112)# st_point_on_surface()


## quickview in mapview
mapview(suburbs_point, zcol = "SSC_NAME16", legend = FALSE)

# ---------------------------------------------------------------------------- #

# test if function works


## reptile
test_reptile = detect_out_of_range(spp_name =  #"Antaresia childreni",
                               spp_geometries = distribs,
                               # geom_gt_key = reptiles_gt_geom_key,
                               # taxa = "reptiles", 
                               website_trade = gt_reptiles2, 
                               suburb_geometries = suburbs_point, 
                               distrib_buffer_km = 200,
                               view_map = TRUE
)

## birds
test_bird = detect_out_of_range(spp_name = "Taeniopygia guttata",
                                spp_geometries = distribs,
                                # geom_gt_key = birds_gt_geom_key,
                                # taxa = "birds", 
                                website_trade = gt_birds2, 
                                suburb_geometries = suburbs_point,
                                distrib_buffer_km = 200,
                                view_map = TRUE
)

test_bird = detect_out_of_range(spp_name = "Trichoglossus haematodus",
                                spp_geometries = distribs,
                                # geom_gt_key = birds_gt_geom_key,
                                # taxa = "birds", 
                                website_trade = gt_birds2, 
                                suburb_geometries = suburbs_point,
                                distrib_buffer_km = 200,
                                view_map = TRUE
)


# amphib test
test_amphib = detect_out_of_range(spp_name = "Ranoidea caerulea",
                                spp_geometries = distribs,
                                # geom_gt_key = amphibs_gt_geom_key,
                                # taxa = "amphibians", 
                                website_trade = gt_amphibians2, 
                                suburb_geometries = suburbs_point,
                                distrib_buffer_km = 200,
                                view_map = TRUE
)


# ---------------------------------------------------------------------------- #

# run function for each species
detach("package:tidylog", unload=TRUE)

# all_results = 

## reptiles
reptiles_results = map(unique(gt_reptiles2$species),
                    function(spp){
                      detect_out_of_range(spp_name = spp,
                                          spp_geometries = distribs,
                                          # geom_gt_key = reptiles_gt_geom_key,
                                          # taxa = "reptiles",
                                          website_trade = gt_reptiles2,
                                          suburb_geometries = suburbs_point,
                                          distrib_buffer_km = 200
                      )
                    })
  
## amphibians
amphibs_results = map(unique(gt_amphibians2$species),
                       function(spp){
                         detect_out_of_range(spp_name = spp,
                                             spp_geometries = distribs,
                                             # geom_gt_key = amphibs_gt_geom_key,
                                             # taxa = "amphibians",
                                             website_trade = gt_amphibians2,
                                             suburb_geometries = suburbs_point,
                                             distrib_buffer_km = 200
                         )
                       })
  
  
## birds
birds_results = map(unique(gt_birds2$species),
                    function(spp){
                      detect_out_of_range(spp_name = spp,
                                          spp_geometries = distribs,
                                          # geom_gt_key = birds_gt_geom_key,
                                          # taxa = "birds",
                                          website_trade = gt_birds2,
                                          suburb_geometries = suburbs_point,
                                          distrib_buffer_km = 200
                      )
                    })
  
  
## save raw
saveRDS(reptiles_results, 'data/raw_results/reptiles_raw.rds')
saveRDS(amphibs_results, 'data/raw_results/amphibs_raw.rds')
saveRDS(birds_results, 'data/raw_results/birds_raw.rds')

# reptiles_results = read_rds('data/raw_results/reptiles_raw.rds')
# amphibs_results = read_rds('data/raw_results/amphibs_raw.rds')
# birds_results = read_rds('data/raw_results/birds_raw.rds')

# # ---------------------------------------------------------------------------- #
# 
# # quick summary
# birds_summary = 
#   birds_results %>% 
#     bind_rows() %>% 
#     group_by(species) %>% 
#     summarise(n_listings = n(),
#               n_listings_w_loc = sum(!is.na(outside_range)),
#               n_listings_outside = sum(outside_range, na.rm = T),
#               p_out = sum(outside_range, na.rm = T)/sum(!is.na(outside_range)))
# 
# reptiles_summary = 
#   reptiles_results %>% 
#     bind_rows() %>% 
#     group_by(species) %>% 
#     summarise(n_listings = n(),
#               n_listings_w_loc = sum(!is.na(outside_range)),
#               n_listings_outside = sum(outside_range, na.rm = T),
#               p_out = sum(outside_range, na.rm = T)/sum(!is.na(outside_range)))
# 
# amphibs_summary = 
#   amphibs_results %>% 
#     bind_rows() %>% 
#     group_by(species) %>% 
#     summarise(n_listings = n(),
#               n_listings_w_loc = sum(!is.na(outside_range)),
#               n_listings_outside = sum(outside_range, na.rm = T),
#               p_out = sum(outside_range, na.rm = T)/sum(!is.na(outside_range)))
# 
# 
# # ---------------------------------------------------------------------------- #
# 
# # combine and visualize
# all_results_raw = bind_rows(
#   bind_rows(birds_results) %>% select(classifieds_unique_listing_id, species, state, suburb, outside_range),
#   bind_rows(reptiles_results)%>% select(classifieds_unique_listing_id, species, state, suburb, outside_range),
#   bind_rows(amphibs_results)%>% select(classifieds_unique_listing_id, species, state, suburb, outside_range)) %>% 
#   left_join(st_drop_geometry(suburbs), by = c('state' = 'state_website', 'suburb' = 'suburb_website'))
# 
# 
# all_summary = 
#   birds_summary %>% mutate(taxa = "birds") %>% 
#   bind_rows(reptiles_summary %>% mutate(taxa = "reptiles")) %>% 
#   bind_rows(amphibs_summary %>% mutate(taxa = "amphibians"))
# 
# 
# # plot histogram
# all_summary %>% 
#   ggplot(aes(x = p_out, fill = taxa)) +
#   geom_histogram(binwidth = 0.2, color = "grey") +
#   scale_y_continuous(expand = c(0,1.1)) +
#   # facet_wrap(~taxa) +
#   labs(y = 'number of species', x = 'proportion of adverts\noutside of native range',
#        color = 'taxa') + 
#   scale_fill_manual(values = c("amphibians" = "#1b9e77", "birds" = "#d95f02", "reptiles" = "#7570b3")) +
#   egg::theme_presentation() +
#   theme(axis.title = element_blank(),
#         legend.title = element_blank()) + 
#   NULL
# 
# 
# # plot n listings vs p out
# all_summary %>% 
#   ggplot(aes(x = n_listings_w_loc, y = n_listings_outside, color = taxa)) + 
#   geom_point() +
#   scale_x_continuous(trans = "log10") +
#   scale_y_continuous(trans = 'log1p') +
#   guides(color = FALSE) +
#   facet_wrap(~taxa, scales = "free") +
#   NULL
# 


# # quick summary stats
# 
# ## n species
# all_summary %>% 
#   # group_by(taxa) %>% 
#   summarise(n_distinct(species))
# 
# ## n species sold out of range
# all_summary %>% 
#   filter(p_out > 0) %>% 
#   # group_by(taxa) %>% 
#   summarise(n_distinct(species))
# 
# ## n where all are outside
# all_summary %>% 
#   filter(p_out == 1) %>% 
#   # group_by(taxa) %>% 
#   summarise(n_distinct(species))
# 
# ## n listings
# nrow(gt_herps %>% filter(!is.na(state))) + nrow(gt_birds %>% filter(!is.na(state)))
# 
# ## median/median p out
# all_summary %>% 
#   summarise(median(p_out), mean(p_out))
# 
# all_summary %>% 
#   arrange(-n_listings_outside) %>% 
#   select(species, n_listings_outside) %>% 
#   rename(`N ads outside` = n_listings_outside) %>% 
#   write.csv('data/quick.csv')
# 
# 
# # view indivs maps
# 
# pvit = detect_out_of_range(spp_name = "Pogona vitticeps",
#                     spp_geometries = reptiles_geom_native,
#                     geom_gt_key = reptiles_gt_geom_key,
#                     taxa = "reptiles", 
#                     website_trade = gt_reptiles, 
#                     suburb_geometries = suburbs_point,
#                     view_map = TRUE,
#                     return_map_only = TRUE)
# 
# mspi = detect_out_of_range(spp_name = "Morelia spilota",
#                            spp_geometries = reptiles_geom_native,
#                            geom_gt_key = reptiles_gt_geom_key,
#                            taxa = "reptiles", 
#                            website_trade = gt_reptiles, 
#                            suburb_geometries = suburbs_point,
#                            view_map = TRUE,
#                            return_map_only = TRUE)
# 
# ## birds
# tmol = detect_out_of_range(spp_name = "Trichoglossus moluccanus",
#                                 spp_geometries = birds_geom_native,
#                                 geom_gt_key = birds_gt_geom_key,
#                                 taxa = "birds", 
#                                 website_trade = gt_birds, 
#                                 suburb_geometries = suburbs_point,
#                                 view_map = TRUE,
#                                 return_map_only = TRUE)
# 
# 
# # amphib
# test_amphib = detect_out_of_range(spp_name = "Ranoidea caerulea",
#                                   spp_geometries = amphibs_geom_native,
#                                   geom_gt_key = amphibs_gt_geom_key,
#                                   taxa = "amphibians", 
#                                   website_trade = gt_amphibians, 
#                                   suburb_geometries = suburbs_point,
#                                   view_map = TRUE,
#                                   return_map_only = TRUE)
# 
# 
# 
# # by state
# 
# all_results_raw %>% 
#   group_by(STE_NAME16) %>% 
#   summarise(n_listings = n(),
#             n_listings_w_loc = sum(!is.na(outside_range)),
#             n_listings_outside = sum(outside_range, na.rm = T),
#             p_out = sum(outside_range, na.rm = T)/sum(!is.na(outside_range)))


# # For SA
# 
# taxa_key = bind_rows(
#   gt_birds %>% select(species, family, order, class) %>% distinct(),
#   gt_herps %>% select(species, family, order, class) %>% distinct()
# )
# 
# sa = all_results_raw %>% 
#   filter(STE_NAME16 == "South Australia") %>% 
#   group_by(species) %>% 
#   summarise(n_listings = sum(outside_range)) %>% 
#   filter(n_listings > 0) %>% 
#   left_join(taxa_key) %>% 
#   arrange(class, species)
# 
# 
# # export 
# write.csv(sa, 'data/sa_out_of_range_sales.csv', row.names = F)
