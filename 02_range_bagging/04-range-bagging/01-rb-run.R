library(raster)
library(tidyverse)
library(glue)
library(sf)
library(mapview)
library(spatialsample) #install.packages("spatialsample")


# ---------------------------------------------------------------------------- #


# load in all funs
source('scripts/range_bagging/04-range-bagging/rb_funs.R')

# load in data

## aus raster
r = read_rds('data/world_clim/template_aus_raster.rds')

## aus cov data
aus = read_rds('data/world_clim/pca_covariate_matrix_aus.rds')
aus_mat_all =   aus %>% as.matrix()
aus_mat = aus_mat_all[ complete.cases(aus_mat_all[ , -1]) , -1 ]
rownames(aus_mat) = aus_mat_all[ complete.cases(aus_mat_all[ , -1]) , 1 ]

## gbif ids & spp names
gbif_ids = list.files('data/world_clim/pca_covariate_matrix_spp_occ/') %>% 
  str_remove('\\.rds')

spp = read_rds('data/occurences/df_spp_occ_syns_2.rds')

# sum(gbif_ids %in% spp$gbif_id) == length(gbif_ids)

## define tuning grid
tuning_grid = expand_grid(d = c(1), # 1,
                          p = seq(0.1,1,0.1),
                          v = seq(200,2000,200))

# ---------------------------------------------------------------------------- #

# loop over spp
for(i in 1:length(gbif_ids)){
# for(i in 23:length(gbif_ids)){
# for(i in 35){
    
  # print update
  message(glue("Start {i} of {length(gbif_ids)}: {spp$species[spp$gbif_id  == gbif_ids[i]]}"))
  
  
  # -------------------------------------------------------------------------- #
  
  # load in species data
  spp_data_sf = read_in_spp(gbif_ids[i])
  # mapview(spp_data_sf)
  message(glue("{nrow(spp_data_sf)} records"))
  
  
  # -------------------------------------------------------------------------- #
  
  
  # break if < 50 points
  if(nrow(spp_data_sf) > 50){
    
  # -------------------------------------------------------------------------- #

      # set up a folder in data/range_bagging to save each intermediate step
  output_dir = glue("data/range_bagging/pca/{gbif_ids[i]}")
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } 
  
  
  # -------------------------------------------------------------------------- #
  
  # Run spatial CV, 5 fold for < 1000 pts, 10 fold for >
  
  ## get n folds for cv
  n_folds = if_else(nrow(spp_data_sf) >= 1000, 10, 5)
  
  ## run cv
  tuning_preds = spatial_cv(n_folds, spp_data_sf, tuning_grid)
  
  ## save
  saveRDS(tuning_preds, glue("data/range_bagging/pca/{gbif_ids[i]}/tuning_preds.rds"))
  
  # -------------------------------------------------------------------------- #
  
  
  # process tuning results
  df_tuning_results = process_cv_tuning(tuning_preds)
  saveRDS(df_tuning_results, glue("data/range_bagging/pca/{gbif_ids[i]}/df_tuning_results.rds"))
  
  ## choose winner
  tune_winner = df_tuning_results %>% arrange(-macro_mean_pred) %>% slice(1)
  
  # -------------------------------------------------------------------------- #  
  
  # run full model and pred to australia
  r_aus_pred = run_full_and_pred_aus(spp_data_sf, 
                                     spp_name = spp$species[spp$gbif_id == i],
                                    d = tune_winner$d[1], 
                                    p = tune_winner$p[1],
                                    v = tune_winner$v[1],
                                    r_template = r, 
                                    aus_mat = aus_mat)
  
  plot(r_aus_pred, main = spp$species[spp$gbif_id == gbif_ids[i]] )
  saveRDS(r_aus_pred,  glue("data/range_bagging/pca/{gbif_ids[i]}/r_aus_pred.rds"))
  
  # -------------------------------------------------------------------------- #  
  overlap = 0.8
  r_aus_pred_rc = reclass_preds(r_aus_pred, overlap)
  plot(r_aus_pred_rc, main = spp$species[spp$gbif_id == gbif_ids[i]] )
  saveRDS(r_aus_pred_rc,  glue("data/range_bagging/pca/{gbif_ids[i]}/r_aus_pred_rc.rds"))
  
  # -------------------------------------------------------------------------- #  
  
  # rm temps
  rm(spp_data_sf, n_folds, tuning_preds, df_tuning_results, r_aus_pred, r_aus_pred_rc)
  
  # end if
  }else{
    rm(spp_data_sf)
  }

}



