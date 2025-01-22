# load occ
# load clean pts to rm
# load oor to rm
# subset not in clean and yes in oor
# check if pts overlap bird establishments
# save seperate


library(tidyverse)
library(sf)
library(mapview)
library(glue)


# load in distribs
distribs = read_rds('data/spp_geometries/_all_spp_geoms_v2.rds')

# load in herp establishments
he = st_read('data/establishments/herps/herp_estab.gpkg') %>% 
  st_transform(crs = 4326) %>% 
  mutate(i = row_number(), .before = species) %>% 
  st_make_valid()

# load in species
# check all spp in folder
gbifs_in_folder = str_remove(list.files("data/occurences/species/"), "\\.rds")




# loop over spp then loop over each established polygon
he_n_pts_list = list(); f = 1
for(gbifId in unique(he$gbif_id)){
  
  # load in occ
  occ = read_rds(glue("data/occurences/species/{gbifId}.rds")) %>%
    mutate(i = row_number())
  
  # get pts outside native ranve
  pts_oor =  read_rds(glue("data/occurences/rm_pts/oor/{gbifId}.rds"))
  
  # subset occ
  occ_oor = occ %>% filter(i %in% pts_oor)
  
  #get all establishments
  he_subset = he %>% filter(gbif_id == gbifId)
  
  
  # loop over each establishment for given spp
  rm_pts_temp = list(); d = 1
  for( i in 1:nrow(he_subset)){
    
    # check if overlaps
    pts_int = occ_oor %>% st_intersection(he_subset[i,])
    
    # append
    rm_pts_temp[[d]] = pts_int$i
    
    # update counter
    d = d + 1
    
    # print update
    message(glue("{i} of {nrow(he_subset)}"))
    
    # append n points, to track polys with no points
    he_n_pts_list[[f]] = tribble(~i, ~gbif_id, ~n_occ,
                                 he_subset$i[i], he_subset$gbif_id[i], length(pts_int$i))
    f = f + 1
    
    # rm temps
    rm(pts_int)
    
  }
  
  # unlist
  pts = unlist(rm_pts_temp)
  
  if(length(pts) > 0){
    # save to file
    saveRDS(pts, glue('data/occurences/rm_pts/in_estab_range/{gbifId}.rds'))
  }
  
  # rm temps
  rm(occ, pts_oor, occ_oor, he_subset, rm_pts_temp, d, pts)
  gc()
  
  # print update
  print(glue("{which(unique(he$gbif_id) == gbifId)} of {length(unique(he$gbif_id))}"))
}


# See what spp had no points in polygons
he_n_pts = bind_rows(he_n_pts_list)

## save
saveRDS(he_n_pts, "data/occurences/rm_pts/in_estab_range/_herps_gbifid_n_occin_estab_polys.rds")








# # loop over spp
# spp_no_coords_in_estab = list(); d = 1
# for(i in 1:nrow(he)){
#   
#   # load in occ
#   occ = read_rds(glue("data/occurences/species/{he$gbif_id[i]}.rds")) %>% 
#     mutate(i = row_number())
#   
#   # get pts outside native ranve
#   pts_oor =  read_rds(glue("data/occurences/rm_pts/oor/{he$gbif_id[i]}.rds"))
#   
#   # subset occ
#   occ_oor = occ %>% filter(i %in% pts_oor)
#   
#   # load estab range
#   spp = he[i ,] %>% st_make_valid()
#   # mapview(spp) + mapview(st_geometry(occ_oor))
#   
#   # get intersection 
#   pts_int = occ_oor %>% st_intersection(spp)
#   # mapview(spp) +  mapview(st_geometry(pts_int))
#   
#   # save if no points 
#   if( nrow(pts_int) == 0){
#     spp_no_coords_in_estab[[d]] = he[i,]
#     d = d +1
#   }else{
#     # save to file
#     saveRDS(pts_int$i, glue('data/occurences/rm_pts/in_estab_range/{he$gbif_id[i]}.rds'))
#   }
#   
#   
#   # rm temps
#   rm(spp, occ, pts_oor, occ_oor, pts_int)
#   gc()
#   
#   # print update
#   print(glue("{i} of {nrow(he)}"))
#   
# }
# 
# 
# # look at spp with no points
# df_no = bind_rows(spp_no_coords_in_estab)
# 
# # save gbif ids of ones without pts, to later get mean worldclim vals
# saveRDS(df_no, "data/occurences/rm_pts/in_estab_range/_herps_gbifid_no_pt_in_estab_range.rds")
# 
