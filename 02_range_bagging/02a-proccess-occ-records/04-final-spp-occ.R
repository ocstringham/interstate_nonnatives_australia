library(raster)
library(sf)
# library(stars)
library(tidyverse)
library(glue)
library(mapview)


# Loop over spp

## load in data

### list of gbifIDs
gbifs_in_folder = str_remove(list.files("data/occurences/species/"), "\\.rds")



for(i in 1:length(gbifs_in_folder)){
# for(i in 138:length(gbifs_in_folder)){

  gbifId = gbifs_in_folder[i]
  
  # Load in occ
  occ = readRDS(glue('data/occurences/species/{gbifId}.rds'))
  
  # get points to rm from cleaning
  pts_clean_rm =  read_rds(glue("data/occurences/rm_pts/clean/{gbifId}.rds"))
  
  # get pts outside native ranve
  pts_oor =  read_rds(glue("data/occurences/rm_pts/oor/{gbifId}.rds"))
  
  # get pts outside native range to keep
  if(file.exists(glue('data/occurences/rm_pts/in_estab_range/{gbifId}.rds'))){
    pts_oor_keep = read_rds(glue('data/occurences/rm_pts/in_estab_range/{gbifId}.rds'))
    pts_oor_rm = pts_oor[ !pts_oor %in% pts_oor_keep ]
  }else{
    pts_oor_rm = pts_oor
  }
  
  # subset
  occ_sub = occ %>% 
    mutate(id = row_number()) %>% 
    filter(!id %in% c(pts_clean_rm, pts_oor_rm)) %>% 
    st_geometry() %>% 
    st_sf() %>% 
    distinct()
  
  if(nrow(occ_sub) > 0){
    
    saveRDS(occ_sub, glue("data/occurences/clean/{gbifId}.rds"))
  
  }
  
  # print update
  message(glue("{i} of {length(gbifs_in_folder)}"))
  
  # rm temps
  rm(gbifId, occ, pts_clean_rm, pts_oor, count_raster, 
     pts_oor_rm, occ_sub)
  if(exists(x = "pts_oor_keep")) rm(pts_oor_keep)

}


# test = read_rds("data/occurences/occ_raster/count/2427671.rds")


# # test on one spp. 
# 
# # Load in occ
# gbifId = "2441333"
# occ = readRDS(glue('data/occurences/species/{gbifId}.rds'))
# 
# # load in points to rm
# pt_rm = readRDS('data/occurences/rm_pts_outside/df_inds_outside_to_rm_ALL.rds')
# 
# 
# # rm points
# pts_to_rm = 
#   pt_rm %>% 
#     filter(`gbif_id` == gbifId) %>% 
#     pull(inds_to_rm) %>% 
#     unlist()
# 
# occ_sub = occ %>% 
#   mutate(id = row_number()) %>% 
#   filter(!id %in% pts_to_rm) %>% 
#   st_geometry() %>% 
#   st_sf()
# 
# occ_sub_sp = as_Spatial(occ_sub)
# 
# 
# # to raster
# 
# ## load in template raster, 5 min resolution world clim (can load upfront)
# r = raster('data/occurences/world_clim_template/wc2.1_5m_tmin_01.tif')
# r
# 
# 
# count_raster = rasterize(occ_sub_sp, r, fun = 'count')
# # count_raster
# # sum(count_raster@data@values, na.rm = T)
# # mapview(count_raster)
# 
# # to 1/NA raster
# m <- c(1, max(count_raster@data@values, na.rm = T), 1)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# presence_raster = reclassify(count_raster, rclmat)
# # presence_raster
# # mapview(presence_raster)
# 
# 
# # save
# saveRDS(count_raster, glue("data/occurences/occ_raster/count/{gbifId}.rds"))
# saveRDS(presence_raster, glue("data/occurences/occ_raster/presence/{gbifId}.rds"))

