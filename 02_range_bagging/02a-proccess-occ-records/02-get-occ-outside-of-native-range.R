library(tidyverse)
library(sf)
library(mapview)
library(glue)


# load in distribs
distribs = read_rds('data/spp_geometries/_all_spp_geoms_v2.rds')

# load in species
# check all spp in folder
gbifs_in_folder = str_remove(list.files("data/occurences/species/"), "\\.rds")


# Loop over each spp
# for(gbifId in gbifs_in_folder){
for(gbifId in gbifs_in_folder[27:length(gbifs_in_folder)]){
  
  # load spp distrib
  spp = distribs %>% filter(gbif_id == gbifId) %>% 
    st_buffer(10 * 1000) %>% 
    st_transform(crs = 4326) %>% 
    st_make_valid()
  
  # load in occ
  occ = read_rds(glue("data/occurences/species/{gbifId}.rds")) %>% 
    mutate(i = row_number())
  
  # load in rm ts
  rm_pt1 = read_rds(glue('data/occurences/rm_pts/clean/{gbifId}.rds'))
  
  # subset df
  occ2 = occ %>% filter(!i %in% rm_pt1)
  # mapview(st_geometry(occ2)) + mapview(spp)
  
  # checks what pts intersect native range
  pts_diff = occ2 %>% st_difference(spp)
  # mapview(st_geometry(pts_int)) + mapview(spp)
  
  # test = occ %>% filter(!i %in% rm_pt1 & !i %in%  pts_diff$i)
  # mapview(st_geometry(test)) + mapview(spp)
  
  
  # save
  saveRDS(pts_diff$i, glue('data/occurences/rm_pts/oor/{gbifId}.rds'))
  
  # rm temps
  rm(spp, occ, rm_pt1, occ2, pts_diff)
  gc()
  
  # print update
  print(glue("{which(gbifs_in_folder == gbifId)} of {length(gbifs_in_folder)}"))

}
