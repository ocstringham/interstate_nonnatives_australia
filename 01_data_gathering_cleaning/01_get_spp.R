# Script to create dataset of advertisment data along with suburb it was sold

library(dplyr)
library(tidylog)

# functions
source('scripts/functions/connect_to_mysql.R')

# load data
birds_market = readRDS('data/market_raw/all_cleaned_final_live_birds.rds')
herps_market = readRDS('data/market_raw/_live_for_sale_herps_listings.rds')


# subset for natives only
birds_market = birds_market %>% filter(native.species == "yes")
herps_market = herps_market %>% filter(scientific_name != "Ambystoma mexicanum")


# get suburb of species

## connect to mysql
conn = connect_mysql("webscraped_data")

dbListTables(conn)

## get location key
locations = tbl(conn, sql("select * from website_au_locations")) %>% collect()


### birds

# query mysql
gt_birds_q = 
  tbl(conn, sql("select * from website_au_birds")) %>% 
  filter(classifieds_unique_listing_id %in% 
           !! birds_market$classifieds_unique_listing_id) %>% 
  select(classifieds_unique_listing_id, website_location )

# to R
gt_birds_df = 
  gt_birds_q %>% 
  collect() %>% 
  distinct(classifieds_unique_listing_id, .keep_all = TRUE)

# join in real locations
gt_birds_locs = 
  gt_birds_df %>% 
  left_join(locations) %>% 
  select(classifieds_unique_listing_id, state, 
         region = region_only, area = area_only, suburb)


### herps

# query mysql
gt_herps_q = 
  tbl(conn, sql("select * from website_au_herps")) %>% 
  filter(classifieds_unique_listing_id %in% 
           !! herps_market$classifieds_unique_listing_id) %>% 
  select(classifieds_unique_listing_id, website_location )

# to R
gt_herps_df = 
  gt_herps_q %>% 
  collect() %>% 
  distinct(classifieds_unique_listing_id, .keep_all = TRUE)

# join in real locations (ie suburb)
gt_herps_locs = 
  gt_herps_df %>% 
  left_join(locations) %>% 
  select(classifieds_unique_listing_id, state, 
         region = region_only, area = area_only, suburb)

# to df
bird_listings2 = 
  birds_market %>% 
  left_join(gt_birds_locs) %>% 
  filter(!is.na(species))

herp_listings2 = 
  herps_market %>% 
  left_join(gt_herps_locs) %>% 
  filter(!is.na(species))



# view spp
bird_spp = bird_listings2 %>% distinct(species) %>% left_join(birds_market %>% select(species, common_name, family) %>% distinct())
herp_spp = herp_listings2 %>% distinct(species) %>% left_join(herps_market %>% select(species, common_name, family) %>% distinct())

# fix rainbow lorri
bird_listings3 = 
  bird_listings2 %>% 
  mutate(species = ifelse(species == "Trichoglossus haematodus", 
                          "Trichoglossus moluccanus", species))

# export df of listing with suburb location attached
saveRDS(bird_listings3, 'data/market/bird_listings.rds')
saveRDS(herp_listings2, 'data/market/herp_listings.rds')

# 
# bird_listings2 = readRDS('data/subset_of_spp/birds_subset.rds')
