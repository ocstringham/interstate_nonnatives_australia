# Script to resolve some of the suburb names that have formatting inconsistencies

library(sf)
library(tidyverse)
library(tidylog)


# # load in spp geometries
# birds_geom = st_read('data/subset_geometries/birds/birds_subset.shp')
# herps_geom = st_read('data/subset_geometries/herps/herps_subset.shp')

# load in GT data
gt_birds = readRDS('data/market/bird_listings.rds')
gt_herps = readRDS('data/market/herp_listings.rds')
gt = bind_rows(gt_herps, gt_birds)

# load in Aus suburb geometries
suburbs = st_read('data/suburb_geometries/1270055003_ssc_2016_aust_shape/SSC_2016_AUST.shp')

# 1. get subset of suburbs from in website

## get distinct records of suburbs in GT
gt_suburbs = 
  gt %>% 
  distinct(state, suburb)

## join in geometries
gt_suburbs_geom = 
  gt_suburbs %>% 
  left_join(suburbs, by = c('state' = 'STE_NAME16', 'suburb' = 'SSC_NAME16')) %>% 
  filter(!is.na(SSC_CODE16))

missed_suburbs = 
  gt_suburbs %>% 
  anti_join(suburbs, by = c('state' = 'STE_NAME16', 'suburb' = 'SSC_NAME16'))

View(st_drop_geometry(suburbs))

## 2. add in abbrv to suburbs to match more
missed_suburbs =
  missed_suburbs %>%
  mutate(suburb2 = case_when(str_detect(suburb, "CBD") ~ suburb %>% str_remove("CBD") %>% str_trim(),
                             state == "Australian Capital Territory"  ~ paste(suburb, "(ACT)"),
                             state == "New South Wales" ~ paste(suburb, "(NSW)"),
                             state == "Queensland" ~ paste(suburb, "(Qld)"),
                             state == "South Australia" ~ paste(suburb, "(SA)"),
                             state == "Tasmania" ~ paste(suburb, "(Tas.)"),
                             state == "Victoria" ~ paste(suburb, "(Vic.)"),
                             state == "Western Australia" ~ paste(suburb, "(WA)"),
                             state == "Northern Territory" ~ paste(suburb, "(NT)"),
                             TRUE ~ suburb
                             )
         )

## retry join
gt_suburbs_geom2 =
  missed_suburbs %>%
  left_join(suburbs, by = c('state' = 'STE_NAME16', 'suburb2' = 'SSC_NAME16')) %>% 
  filter(!is.na(SSC_CODE16))


missed_suburbs2 =
  missed_suburbs %>%
  anti_join(suburbs, by = c('state' = 'STE_NAME16', 'suburb2' = 'SSC_NAME16')) %>% 
  left_join(gt %>% distinct(state, region, area, suburb)) %>% 
  arrange(state, suburb)



## manually match them
# write.csv(missed_suburbs2, 'data/suburb_geometries/resolved_suburbs/missed_suburbs.csv', row.names = F)
missed_suburbs2_manual = read.csv('data/suburb_geometries/resolved_suburbs/missed_suburbs_manual.csv', colClasses = "character")

gt_suburbs_geom3 = 
  missed_suburbs2_manual %>% 
  left_join(suburbs, by = 'SSC_CODE16') %>% 
  filter(!is.na(SSC_CODE16))

nrow(gt_suburbs) == nrow(gt_suburbs_geom) + nrow(gt_suburbs_geom2) + nrow(gt_suburbs_geom3) + 1 # for NA




# combine all together
gt_suburbs_geoms_all = 
  gt_suburbs_geom %>% 
  select(state, suburb, SSC_CODE16, geometry) %>% 
  bind_rows(gt_suburbs_geom2 %>% select(state, suburb, SSC_CODE16, geometry)) %>% 
  bind_rows(gt_suburbs_geom3 %>% select(state, suburb, SSC_CODE16, geometry)) %>% 
  rename(state_website = state, suburb_website = suburb) %>% 
  left_join(suburbs %>% select(SSC_CODE16, SSC_NAME16, STE_CODE16, STE_NAME16)) %>% 
  select(state_website, suburb_website, 
         STE_CODE16, STE_NAME16, SSC_CODE16, SSC_NAME16, 
         geometry) %>% 
  st_as_sf()

View(st_drop_geometry(gt_suburbs_geoms_all))

# export
saveRDS(gt_suburbs_geoms_all, 'data/suburb_geometries/website_suburb_geometries.rds')

gt_suburbs_geoms_all = readRDS('data/suburb_geometries/website_suburb_geometries.rds')



# make key with website location

# get suburb of species

## connect to mysql
source('scripts/functions/connect_to_mysql.R')
conn = connect_mysql("webscraped_data")

dbListTables(conn)

## get location key
locations = tbl(conn, sql("select * from website_au_locations")) %>% collect()
locations2 = locations %>% select(state_website = state,
                                  suburb_website = suburb,
                                  website_location)

locations2 %>% group_by(website_location) %>% filter(n()>1)
gt_suburbs_geoms_all %>% group_by(state_website, suburb_website) %>% filter(n()>1)

## combine to resolved geoms

loc2 = 
  gt_suburbs_geoms_all %>% 
  left_join(locations2, by = c("state_website", "suburb_website"))

locs2a = 
  loc2 %>% 
  # st_drop_geometry() %>% 
  group_by(state_website, suburb_website) %>% 
  filter(n()>1)

locs2a = locs2a %>% 
  filter(website_location %in% c("green point central coast nsw", 
                                 "kingswood sydney", "mount pleasant mackay",
                                 "white rock cairns", "hillside melbourne"))

locs2b = 
  loc2 %>% 
  # st_drop_geometry() %>% 
  group_by(state_website, suburb_website) %>% 
  filter(n()==1)

locs3 = bind_rows(locs2a, locs2b)  %>% 
  select(website_location, everything())


# try to match others not in reptiles/birds
suburbs_geom1 = 
  suburbs %>% 
  st_drop_geometry() %>% 
  anti_join(locs3, by = "SSC_CODE16") %>% 
  left_join(locations, by = c('STE_NAME16' = 'state', 'SSC_NAME16'='suburb')) %>% 
  filter(!is.na(id)) %>% 
  select(website_location, state_website = STE_NAME16, suburb_website = SSC_NAME16,
         STE_CODE16, STE_NAME16, SSC_CODE16, SSC_NAME16) %>% 
  left_join(suburbs %>% select(SSC_CODE16, geometry)) %>% 
  st_as_sf()

  
# add to it
suburbs_all = bind_rows(locs3, suburbs_geom1)

mapview::mapview(suburbs_all)


# save
st_write(suburbs_all, 'data/suburb_geometries/resolved_suburbs_with_geoms.gpkg')
# test = st_read('data/suburb_geometries/resolved_suburbs_with_geoms.gpkg')
