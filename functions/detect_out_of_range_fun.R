library(sf)
library(tidyverse)
# library(tidylog)
library(leaflet)




## define function for to do these operations for one species
## then later loop over function for all species

detect_out_of_range = function(spp_name, 
                               spp_geometries,
                               # geom_gt_key,
                               # taxa,
                               website_trade, 
                               suburb_geometries,
                               distrib_buffer_km,
                               view_map = FALSE,
                               print_update = TRUE,
                               return_map_only = FALSE){
  
  ## 1. subset spp range geometry
  # geom_spp_name = geom_gt_key %>% filter(species == spp_name) %>% pull(species2)
  # 
  # if(taxa == "amphibians"){
  #   
  #   spp_geom = spp_geometries %>% filter(binomial == geom_spp_name)
  # 
  # }else if(taxa == "birds"){
  #   
  #   spp_geom = spp_geometries %>% filter(binomial == geom_spp_name)
  #   
  #   
  # }else if(taxa == "reptiles"){
  #   
  #   spp_geom = spp_geometries %>% filter(Binomial == geom_spp_name)
  #   
  # }else{
  #   print("Could not find species range geometry")
  #   return()
  # }
  spp_geom = spp_geometries %>% filter(species == spp_name)
  
  
  ## check if geom exists
  if(nrow(spp_geom) == 0 ){
    print("no geometry for this spp, returning NULL")
    return()
  }
  
  
  
  ## 1b. check if multi featured bc intersection doesn't work on multi polygons
  if(nrow(spp_geom) > 1){
    spp_geom = spp_geom %>% st_union()
  }
  
  
  ## 1a buffer
  spp_geom = spp_geom %>% st_buffer(distrib_buffer_km * 1000) # 25000
  
  
  ## 2. subset website data
  spp_gt = website_trade %>% filter(species == spp_name)
  
  
  ## 3. attach geometry to website listings
  spp_suburbs = 
    spp_gt %>% 
    left_join(suburb_geometries, by = c('state' = 'state_website', 'suburb' = 'suburb_website')) %>% 
    dplyr::select(classifieds_unique_listing_id, SSC_CODE16, geometry) %>% 
    # distinct() %>% # if many repeating suburbs
    st_as_sf()
  
  ## 4. check intersection/difference again
  spp_outside = st_difference(spp_suburbs, spp_geom)
  
  
  ## 5. make a final dataframe of results
  spp = 
    spp_gt %>% 
    left_join(spp_outside %>% 
                st_drop_geometry() %>% 
                dplyr::select(classifieds_unique_listing_id) %>% 
                mutate(outside_range = 1), 
              by = "classifieds_unique_listing_id") %>% 
    mutate(outside_range = case_when(is.na(state) ~ NA_real_,
                                     is.na(outside_range) ~ 0, 
                                     TRUE ~ outside_range))
  
  
  # view map
  if(view_map){
      p = 
    leaflet() %>% 
    addProviderTiles("OpenStreetMap") %>% 
    addPolygons(data = spp_geom %>% st_transform(crs = 4326) ,
                color = 'green') %>% 
    addMarkers(data = spp_outside %>% st_transform(crs = 4326),
               clusterOptions = markerClusterOptions()) %>% 
    addControl(spp_name, position = 'topleft')
      
    print(p)
    
    if(return_map_only){
      return(p)
    }
  
  }

  if(print_update){
    message(paste("Finished:", spp_name))
  }
  
  
  # return dataframe of results
  return(spp)
  
}