library(raster)
library(tidyverse)
library(sf)
library(glue)
library(mapview)

# buffer 200km (take NN distrib too but no buffer)
# look at suitability outside range

## Load in spp that i did RB on
spp_all = read_rds('data/occurences/df_spp_occ_syns_2.rds')
spp = spp_all %>% distinct(gbif_id, .keep_all = TRUE) %>% 
  filter(species != "Varanus timorensis")
# spp_all = read_rds('data/_final_gt_with_oor.rds')
# spp = spp_all %>% distinct(gbif_id, .keep_all = TRUE)

## load in ranges
distribs = read_rds("data/spp_geometries/_all_spp_geoms_v2.rds") %>% 
  filter(species != "Varanus timorensis")

# distribs = read_rds("data/range_bagging/distribs_gbifid.rds")
# bird_distribs = read_rds('data/spp_geometries/birds_subset.rds')
# amph_distribs = read_rds("data/spp_geometries/amphibs_subset.rds")
# st_geometry(bird_distribs) <- "geometry"
# rept_distribs = read_rds('data/spp_geometries/reptiles_subset.rds')
# 
# bird_key = read_rds('data/spp_geometries/birds_gbif_distrib_key.rds')
# amph_key = read_rds('data/spp_geometries/amphib_gbif_distrib_key.rds')
# rept_key = read_rds('data/spp_geometries/reptile_gbif_distrib_key.rds')
# 
# distribs1 = 
#   (bird_distribs %>% 
#      filter(origin != 3) %>%  #rm introduced pops
#      left_join(bird_key, by = c("binomial"="distrib_spp")) %>% 
#      select(distrib_spp=binomial, gbif_spp, gbif_id, presence:seasonal)
#    ) %>% 
#   bind_rows(
#     (amph_distribs %>% 
#        filter(origin != 3) %>%  #rm introduced pops
#        left_join(amph_key, by = c("binomial"="distrib_spp")) %>% 
#        select(distrib_spp=binomial, gbif_spp, gbif_id, presence:seasonal)
#     )
#   ) %>% 
#   bind_rows(
#     (rept_distribs %>% 
#        left_join(rept_key, by = c("Binomial"="distrib_spp")) %>% 
#        select(distrib_spp=Binomial, gbif_spp, gbif_id)
#     )
#   )
# 
# # dissolve by spp, takes a min
# distribs = distribs1 %>% 
#   st_make_valid() %>% 
#   group_by(gbif_id) %>% 
#   summarise()
# # save for easy load
# saveRDS(distribs, "data/range_bagging/distribs_gbifid.rds")


# Function for one species
get_suitability_outside_range = function(gbif_id_temp, distribs, range_buffer_km){
  
  # load in rb results
  rb_temp = read_rds(glue('data/range_bagging/pca/{gbif_id_temp}/r_aus_pred_rc.rds'))
  
  # load in range distrib
  distrib_temp = distribs %>% filter(`gbif_id`== gbif_id_temp)
  
  # filter by native and dissolve (?)
   
  # buffer N km
  distrib_temp_buf = 
    distrib_temp %>% 
    st_transform(crs = 3112) %>% 
    st_buffer(range_buffer_km * 1000) %>% 
    st_transform(4326)
  
  # clip raster
  rb_clip_temp = mask(rb_temp, distrib_temp_buf, inverse=T, updatevalue = 0)
  
  return(rb_clip_temp)
}


test = get_suitability_outside_range("2447717", distribs, 200)
plot(test)

test = get_suitability_outside_range(gbif_ids[4], distribs, 200)
plot(test)



# loop over spp & Save

gbif_ids = list.files('data/range_bagging/pca/') %>% 
  str_remove('\\.rds')
gbif_ids[ ! gbif_ids %in% spp$gbif_id ]

d = 1
for(gbifId in gbif_ids){
  
  # get class
  temp_class = spp %>% filter(gbif_id == gbifId) %>% pull(class)
  if(temp_class == "Aves"){
    buf_temp = 200
  }else{
    buf_temp = 100
  }
  
  temp_rb_clip = get_suitability_outside_range(gbifId, distribs, buf_temp)
  plot(temp_rb_clip, main = spp$species[spp$gbif_id == gbifId] )
  saveRDS(temp_rb_clip, glue('data/range_bagging/pca/{gbifId}/r_aus_pred_clip.rds'))
  message(d) ; d = d + 1
  rm(temp_rb_clip, temp_class, buf_temp)
}


# ## Try for one species
# 
# # load in rb results
# gbif_id_temp = "2427671"
# rb_temp = read_rds(glue('data/range_bagging/pca/{gbif_id_temp}/r_aus_pred_rc.rds'))
# plot(rb_temp)
# 
# # load in range distrib
# distrib_temp = distribs %>% filter(gbif_id==gbif_id_temp)
# mapview(rb_temp) + mapview(distrib_temp)
# 
# # buffer 500 km
# distrib_temp_buf = 
#   distrib_temp %>% 
#   st_transform(crs = 3112) %>% 
#   st_buffer(500000) %>% 
#   st_transform(4326)
# mapview(rb_temp) + mapview(distrib_temp_buf) +  mapview(distrib_temp)
# 
# 
# # clip raster
# temp = mask(rb_temp, distrib_temp_buf, inverse=T, updatevalue = 0)
# # temp2 = crop(temp, rb_temp)
# # temp@data@values[is.na(temp@data@values)] = 0
# plot(temp)
# 
# sum(temp@data@values >= 0.95, na.rm = TRUE)
# sum(temp@data@values < 0.95, na.rm = TRUE)
# 
# sum(rb_temp@data@values >= 0.95, na.rm = TRUE)
# sum(rb_temp@data@values < 0.95, na.rm = TRUE)

# # think ahead to what type of analysis i wanna do
# 
# # See if species was sold in state with suitable area (and how many cells/km) or prop of state
# 
# # Load in states
# states = st_read('data/ne_50m_admin_1_states_provinces/')
# plot(states)
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
# gt_cities = read_rds("data/suburb_geometries/website_suburb_geometries.rds")
# mapview(gt_cities)
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
# int = temp_p %>% st_intersection(gt_cities_buf)
# mapview(int)
