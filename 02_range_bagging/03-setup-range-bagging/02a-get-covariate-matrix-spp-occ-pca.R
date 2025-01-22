library(raster)
library(sf)
library(fasterize)
library(tidyverse)
library(glue)

source('scripts/range_bagging/03-setup-range-bagging/get_avg_rast_val_one_poly.R')

# get covariate data observations

# load in world clim pca
wc1 = raster('data/world_clim/pca/wc2.1_5m_bio_PCA_1.tif') %>% readAll()
wc2 = raster('data/world_clim/pca/wc2.1_5m_bio_PCA_2.tif') %>% readAll()
wc3 = raster('data/world_clim/pca/wc2.1_5m_bio_PCA_3.tif') %>% readAll()
wc4 = raster('data/world_clim/pca/wc2.1_5m_bio_PCA_4.tif') %>% readAll()
wc5 = raster('data/world_clim/pca/wc2.1_5m_bio_PCA_5.tif') %>% readAll()
wc6 = raster('data/world_clim/pca/wc2.1_5m_bio_PCA_6.tif') %>% readAll()
wc = stack(wc1, wc2, wc3, wc4, wc5, wc6)

gbifs_in_folder = str_remove(list.files("data/occurences/clean"), "\\.rds")

# load in estab ranges and which polys don't have occ pts
be = read_rds('data/establishments/birds/bird_estab_ranges.rds') %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

he = st_read('data/establishments/herps/herp_estab.gpkg') %>% 
  st_transform(crs = 4326) %>% 
  mutate(i = row_number(), .before = species) %>% 
  st_make_valid()

birds_ep = read_rds("data/occurences/rm_pts/in_estab_range/_birds_gbifid_n_occin_estab_polys.rds")
herps_ep = read_rds("data/occurences/rm_pts/in_estab_range/_herps_gbifid_n_occin_estab_polys.rds")

e_all = bind_rows(be, he)
ep = bind_rows(birds_ep, herps_ep)

# ---------------------------------------------------------------------------- #


# define fun to get covariate vals at cell occ
get_cov_vals = function(gbifId, wc, ep, e_all){
  
  # load in obs for a species
  occ = readRDS(glue('data/occurences/clean/{gbifId}.rds'))
  # occ_2 = distinct(occ)
  
  # get vals at cells of occ
  pca_df =
    tibble(pca1 = raster::extract(wc[[1]], occ), 
           pca2 = raster::extract(wc[[2]], occ), 
           pca3 = raster::extract(wc[[3]], occ), 
           pca4 = raster::extract(wc[[4]], occ), 
           pca5 = raster::extract(wc[[5]], occ), 
           pca6 = raster::extract(wc[[6]], occ),
           x = map_dbl(1:nrow(occ), ~st_geometry(occ)[[.x]][1]), 
           y = map_dbl(1:nrow(occ), ~st_geometry(occ)[[.x]][2])
    )
  
  # rm duplicate cells
  pca_df = pca_df %>% distinct(pca1,pca2,pca3,pca4,pca5,pca6, .keep_all = TRUE)

  
  # --------------
  
  # check if has OOR establishments polygons without occ pts in them
  check_oor_poly_no_pt = 
    ep %>% 
    filter(gbif_id == gbifId) %>% 
    filter(n_occ == 0)
  
  # if yes, get average val of raster cells in each poly
  if(nrow(check_oor_poly_no_pt) > 0){
    
    # get polys
    polys = check_oor_poly_no_pt %>% 
      left_join(e_all, by = c("i", "gbif_id")) %>% 
      st_as_sf()
    
    # get raster values around each poly
    pca_df_oor_no_pts = map_df(1:nrow(polys), 
                       ~get_avg_rast_val_one_poly(polys, .x, wc, 
                          r = raster::raster(ext = raster::extent(wc1), 
                                         crs = wc1@crs@projargs,
                                         ncols = wc1@ncols, nrows = wc1@nrows)
                          )
                       )
    
    if(nrow(pca_df_oor_no_pts) > 0){
      pca_df = bind_rows(pca_df, pca_df_oor_no_pts)
      message(glue("GBIF ID: {gbifId} added {nrow(pca_df_oor_no_pts)} polys to pca df"))
    }else{
    message(glue("GBIF ID: {gbifId} had oor polys but none added to pca df"))
    
    }
  }
  
  # rm nas
  pca_df = 
    pca_df %>% 
    filter(!is.na(pca1), !is.na(pca2), !is.na(pca3),
           !is.na(pca4), !is.na(pca5), !is.na(pca6))
  
  message(glue("GBIF ID : {gbifId} done, has {nrow(pca_df)} rows"))
  return(pca_df)
  
}



test1 = get_cov_vals("2474145", wc, ep, e_all)
test2 = get_cov_vals(gbifs_in_folder[1], wc, ep, e_all)
test3 = get_cov_vals("2429578", wc, ep, e_all)


# now for all spp
for(gbifId in gbifs_in_folder){
  temp = get_cov_vals(gbifId, wc, ep, e_all)
  saveRDS(temp, glue('data/world_clim/pca_covariate_matrix_spp_occ/{gbifId}.rds'))
  rm(temp)
  message(glue("{which(gbifId==gbifs_in_folder)} of {length(gbifs_in_folder)}"))
}




# 
# 
# # load in obs for a species
# r = read_rds('data/occurences/occ_raster/presence/2427671.rds')
# 
# 
# # get vals that overlap
# 
# ## get cells that have occ
# occ = which(r@data@values == 1)
# wc1_in_r = wc[[1]]@data@values[occ]
# wc2_in_r = wc[[2]]@data@values[occ]
# 
# test = matrix(c(wc1_in_r, wc2_in_r), ncol = 2, nrow = length(occ), byrow = F)