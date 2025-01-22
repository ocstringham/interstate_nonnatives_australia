library(raster)
library(tidyverse)
library(sf)
library(glue)
library(mapview)


#
# Takes input of the range bagging prediction clipped by native range
# Other input is suburbs geometry, buffered,

# Eventual Output to state, sold in state, suitable in state near population
#

# ---------------------------------------------------------------------------- #

# load in data

## aus states
states = st_read('data/ne_50m_admin_1_states_provinces/')
aus_states = states %>%
  filter(admin == "Australia") %>%
  filter(name != "Jervis Bay Territory") %>%
  dplyr::select(name, abbrev)

## website trade data
gt = read_rds('data/_final_gt_with_oor.rds')

## website suburbs + buffer
gt_suburbs = read_rds("data/suburb_geometries/website_suburb_geometries.rds")

## apply buffer to suburbs
gt_subrubs_buf =
  gt_suburbs %>%
  st_transform(crs = 3112) %>%
  st_buffer(50 * 1000) %>%  # 50 KM BUFFER
  st_transform(4326) %>%
  summarise() # dissolve

# ---------------------------------------------------------------------------- #

# define function
process_rb = function(gbif_id_temp, aus_states, gt_data, gt_subrubs_buf){
  
  # 1. Get rb output clipped to native range
  rb_clip = readRDS(glue("data/range_bagging/pca/{gbif_id_temp}/r_aus_pred_clip.rds"))
  
  # 2. Get what state spp was sold in
  gt_sp = gt_data %>% filter(`gbif_id` == gbif_id_temp) 
  gt_sp_states = gt_sp %>% distinct(STE_NAME16) %>% pull()
  
  # 2. Get what state spp was sold in OUT OF RANGE
  gt_sp_oor = gt_data %>% filter(`gbif_id` == gbif_id_temp) %>% filter(outside_range == 1)
  gt_sp_states_oor = gt_sp_oor %>% distinct(STE_NAME16) %>% pull()
  
  # 3. apply buffer to suburbs
  # gt_subrubs_buf
  
  # 4. Convert range bagging clipped output to polygon
  rb_clip_poly <- sf::st_as_sf(stars::st_as_stars(rb_clip)) %>%
    filter(.[[1]] > 0) %>%
    summarise()
  
  
  # 5. Get intersection of RB suitability and suburbs buffered
  rb_int_suburbs = 
    rb_clip_poly %>% 
    st_intersection(gt_subrubs_buf) %>% # fall withing buffered suburbs
    st_intersection(aus_states)
  # mapview(rb_int_suburbs, zcol = 'name')
  
  
  # 6. Create output df
  df = rb_int_suburbs %>% 
    mutate(traded_in_state_oor = 
             if_else(name %in% gt_sp_states_oor, 1, 0), .after=abbrev) %>% 
    mutate(state_suitable_near_ppl_oor = 1, .after = traded_in_state_oor) %>% 
    full_join(aus_states %>% st_drop_geometry(), by = c("name", "abbrev")) %>% 
    rename(STE_NAME16 = name) %>% 
    mutate(gbif_id = gbif_id_temp, .before = STE_NAME16) %>% 
    replace(is.na(.), 0) %>% 
    mutate(traded_in_state_oor = 
             if_else(STE_NAME16 %in% gt_sp_states_oor, 1, 0)) %>% 
    mutate(traded_in_state = 
             if_else(STE_NAME16 %in% gt_sp_states, 1, 0), 
           .before=traded_in_state_oor) %>% 
    mutate(traded_oor_state_suitable_oor = 
             ifelse(traded_in_state_oor + state_suitable_near_ppl_oor == 2,
                    1, 0), .before = geometry)
  
  return(df)
}


# ---------------------------------------------------------------------------- #

# test
test = process_rb(
                  gbif_id_temp = "2441974", #"2462522",
                  aus_states = aus_states,
                  gt_data =  gt, 
                  gt_subrubs_buf = gt_subrubs_buf
                )
mapview(test, zcol = "STE_NAME16")

# ---------------------------------------------------------------------------- #

# run for every spp & save

gbif_ids = list.files('data/range_bagging/pca/') 
# gbif_ids = c("2479686", "2479598", "2465003", "2441974", "7009386") # for synonyms that needed to be rerun
# spp = read_rds('data/occurences/df_spp_occ_syns_2.rds')
d = 1 # d = 111

for(gbif_id in gbif_ids){
# for(gbif_id in gbif_ids[111:length(gbif_ids)]){
  
  temp = process_rb(
    gbif_id_temp = gbif_id,
    aus_states = aus_states,
    gt_data =  gt, 
    gt_subrubs_buf = gt_subrubs_buf
  )
  # mapview(temp, zcol = "STE_NAME16")
  
  # save
  saveRDS(temp, glue("data/range_bagging/pca/{gbif_id}/sf_results.rds"))
  
  # rm
  rm(temp)
  
  # message
  message(glue("{d} of {length(gbif_ids)}"))
  d=d+1
  
}




# ---------------------------------------------------------------------------- #
# gbif_id_temp = "2427671"
# temp = readRDS(glue("data/range_bagging/pca/{gbif_id_temp}/r_aus_pred_clip500.rds"))
# plot(temp)
# 
# sum(temp@data@values >= 0.95, na.rm = TRUE)
# sum(temp@data@values < 0.95, na.rm = TRUE)
# 
# # sum(rb_temp@data@values >= 0.95, na.rm = TRUE)
# # sum(rb_temp@data@values < 0.95, na.rm = TRUE)
# 
# # think ahead to what type of analysis i wanna do
# 
# # See if species was sold in state with suitable area (and how many cells/km) or prop of state
# 
# # Load in states
# states = st_read('data/ne_50m_admin_1_states_provinces/')
# aus_states = states %>%
#   filter(admin == "Australia") %>%
#   filter(name != "Jervis Bay Territory") %>%
#   select(name, abbrev)
# # plot(aus_states)
# 
# ## get n cells suitable in each states
# 
# temp1 = mask(temp, aus_states[1, ], updatevalue = NA)
# plot(temp1)
# 
# sum(temp1@data@values >= 0.95, na.rm = TRUE)
# sum(temp1@data@values < 0.95, na.rm = TRUE)
# 
# 
# # See if suitable area was within x km of inhabited area (cities where gt sellers are)
# 
# ## get what state is was sold in
# gt = read_rds('data/_final_gt_with_oor.rds')
# gt_sp = gt %>% filter(`gbif_id` == gbif_id_temp)
# gt_sp_states = gt_sp %>% distinct(STE_NAME16) %>% pull()
# 
# # load in cities
# gt_cities = read_rds("data/suburb_geometries/website_suburb_geometries.rds")
# # mapview(gt_cities)
# ## subset cities by states it's sold in
# # gt_cities_sp_state_sold = gt_cities %>% filter(STE_NAME16 %in% gt_sp_states)
# 
# ## apply buffer
# gt_cities_buf =
#   gt_cities %>%
#   st_transform(crs = 3112) %>%
#   st_buffer(50000) %>%
#   st_transform(4326) %>%
#   summarise() # dissolve
# mapview(gt_cities_buf) + mapview(temp)
# 
# 
#   
# ## calc by state if overlap
# # temp_p = rasterToPolygons(temp)
# temp_p <- sf::st_as_sf(stars::st_as_stars(temp)) %>%
#   filter(.[[1]] > 0) %>%
#   summarise()
# 
# mapview(temp_p) + mapview(gt_cities_buf)
# 
# 
# # get intersection of suitability and cities
# int = temp_p %>% 
#   st_intersection(gt_cities_buf) %>% 
#   st_join(aus_states) 
# mapview(int)
# 
# 
# # Create output df
# # cols: 
# # state, 
# # traded in state, 
# # suitable in populated areas in state, 
# # polygon
# 
# df = int %>% 
#   mutate(spp_traded_in_state = if_else(name %in% gt_sp_states, 1, 0), .after=abbrev) %>% 
#   mutate(spp_suitable_near_ppl = 1, .after = spp_traded_in_state) %>% 
#   full_join(aus_states %>% st_drop_geometry(), by = c("name", "abbrev")) %>% 
#   rename(STE_NAME16 = name) %>% 
#   mutate(gbif_id = gbif_id_temp, .before = STE_NAME16) %>% 
#   replace(is.na(.), 0)
#   
#   
#   

