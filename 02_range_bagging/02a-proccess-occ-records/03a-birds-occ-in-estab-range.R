library(tidyverse)
library(sf)
library(mapview)
library(glue)


# load in distribs
distribs = read_rds('data/spp_geometries/_all_spp_geoms_v2.rds')

# load in bird establishments
be = read_rds('data/establishments/birds/bird_estab_ranges.rds') %>% 
  st_transform(crs = 4326)

# load in species
# check all spp in folder
gbifs_in_folder = str_remove(list.files("data/occurences/species/"), "\\.rds")


# loop over spp then loop over each established polygon
be_n_pts_list = list(); f = 1
for(gbifId in unique(be$gbif_id)){
  
  # load in occ
  occ = read_rds(glue("data/occurences/species/{gbifId}.rds")) %>%
    mutate(i = row_number())

  # get pts outside native ranve
  pts_oor =  read_rds(glue("data/occurences/rm_pts/oor/{gbifId}.rds"))

  # subset occ
  occ_oor = occ %>% filter(i %in% pts_oor)
  
  #get all establishments
  be_subset = be %>% filter(gbif_id == gbifId)
  
  
  # loop over each establishment for given spp
  rm_pts_temp = list(); d = 1
  for( i in 1:nrow(be_subset)){
    
    # check if overlaps
    pts_int = occ_oor %>% st_intersection(be_subset[i,])
    
    # append
    rm_pts_temp[[d]] = pts_int$i
    
    # update counter
    d = d + 1
    
    # print update
    message(glue("{i} of {nrow(be_subset)}"))
    
    # append n points, to track polys with no points
    be_n_pts_list[[f]] = tribble(~i, ~gbif_id, ~n_occ,
                  be_subset$i[i], be_subset$gbif_id[i], length(pts_int$i))
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
    rm(occ, pts_oor, occ_oor, be_subset, rm_pts_temp, d, pts)
    gc()
  
    # print update
    print(glue("{which(unique(be$gbif_id) == gbifId)} of {length(unique(be$gbif_id))}"))
}


# See what spp had no points in polygons
be_n_pts = bind_rows(be_n_pts_list)

## save
saveRDS(be_n_pts, "data/occurences/rm_pts/in_estab_range/_birds_gbifid_n_occin_estab_polys.rds")










# !!!!! need to change this here and for herps

# Need double loop
# first species
# for spp in species
## get subset of establ polygons for spp
## load occ records, pts to rm etc
### for polygon in estab_polygons
#### check if occ are in
#### if yes, save inds
#### if no, save polygon row



# # loop over spp
# spp_no_coords_in_estab = list(); d = 1
# for(i in 1:nrow(be)){
#   
#   # load in occ
#   occ = read_rds(glue("data/occurences/species/{be$gbif_id[i]}.rds")) %>% 
#     mutate(i = row_number())
#   
#   # get pts outside native ranve
#   pts_oor =  read_rds(glue("data/occurences/rm_pts/oor/{be$gbif_id[i]}.rds"))
#   
#   # subset occ
#   occ_oor = occ %>% filter(i %in% pts_oor)
#   
#   # load estab range
#   spp = be %>% filter(gbif_id == be$gbif_id[i])
#   # mapview(spp) + mapview(occ_oor)
#   
#   # get intersection 
#   pts_int = occ_oor %>% st_intersection(spp)
#   # mapview(spp) + mapview(pts_int)
#   
#   # save if no points 
#   if( nrow(pts_int) == 0){
#     spp_no_coords_in_estab[[d]] = be[i,]
#     d = d +1
#   }else{
#     # save to file
#     saveRDS(pts_int$i, glue('data/occurences/rm_pts/in_estab_range/{be$gbif_id[i]}.rds'))
#   }
#   
#   
#   # rm temps
#   rm(spp, occ, pts_oor, occ_oor, pts_int)
#   gc()
#   
#   # print update
#   print(glue("{i} of {nrow(be)}"))
#   
# }
# 
# 
# # look at spp with no points
# View(st_drop_geometry(be[be$gbif_id %in% spp_no_coords_in_estab,]))
# mapview(be[be$gbif_id == spp_no_coords_in_estab[1],])
# mapview(be[be$gbif_id == spp_no_coords_in_estab[2],])
# mapview(be[be$gbif_id == spp_no_coords_in_estab[3],])
# mapview(be[be$gbif_id == spp_no_coords_in_estab[4],])
# mapview(be[be$gbif_id == spp_no_coords_in_estab[5],])
# mapview(be[be$gbif_id == spp_no_coords_in_estab[6],])
# mapview(be[be$gbif_id == spp_no_coords_in_estab[7],])
# 
# # save gbif ids of ones without pts, to later get mean worldclim vals
# saveRDS(spp_no_coords_in_estab, "data/occurences/rm_pts/in_estab_range/_birds_gbifid_no_pt_in_estab_range.rds")
# 
