
library(raster)
library(terra)
library(tidyverse)
library(glue)
library(sf)
library(mapview)
library(spatialsample) 



# load in rb fun
source('scripts/range_bagging/04-range-bagging/rb.R')


# read in spp data
read_in_spp = function(gbif_id){
  test_spp = read_rds(glue('data/world_clim/pca_covariate_matrix_spp_occ/{gbif_id}.rds'))
  test_spp = test_spp[ complete.cases(test_spp), ]
  test_spp_sf = st_as_sf(test_spp, coords=c("x","y"), crs=4326)
  return(test_spp_sf)
}



# Run spatial CV
spatial_cv = function(n_folds, spp_data_sf, tuning_grid){
  
  
  # Set up N-fold spatial CV
  spatial_cv = spatial_clustering_cv(spp_data_sf, v = n_folds)
  # autoplot(spatial_cv)
  
  
  # Run n-fold CV w/tuning
  
  # # define tuning grid and inits
  # tuning_grid = expand_grid(d = 1, 
  #                           p = seq(0.1,1,0.1), 
  #                           v = seq(400,2000,400))
  tuning_preds = list() 
  
  # loop over tuning grid
  for(i in 1:nrow(tuning_grid)){
    
    fold_preds = list()
    
    # loop over n-folds
    for(fold in 1:nrow(spatial_cv)){
      
      # prep data
      data_temp = spatial_cv$splits[[fold]]$data
      
      ## training set
      train_id_temp =  spatial_cv$splits[[fold]]$in_id
      train_temp = data_temp[train_id_temp , ] %>% 
        st_drop_geometry() %>% 
        as.matrix()
      
      ## test set
      all_ids_temp = 1:nrow(data_temp)
      test_id_temp = all_ids_temp[! all_ids_temp %in% train_id_temp]
      test_temp = data_temp[test_id_temp , ] %>% 
        st_drop_geometry() %>% 
        as.matrix()
      
      # Train RB
      rb_temp = rb(train_temp, 
                   d = tuning_grid$d[i], 
                   p = tuning_grid$p[i], 
                   v = tuning_grid$v[i])
      
      # Test RB
      rb_test_temp = rb.test(rb_temp, test_temp)
      
      # save preds
      fold_preds[[fold]] = tibble(fold = fold, preds = rb_test_temp)
      
      
      # # print update
      # message(glue("fold {fold} of {n_folds} done."))
      
      #rm temps
      rm(data_temp, train_id_temp, train_temp, all_ids_temp, 
         test_id_temp, test_temp,
         rb_temp, rb_test_temp)
      
    }
    
    
    # store preds with tuning params
    tuning_preds[[i]] = tibble(d = tuning_grid$d[i], 
                               p = tuning_grid$p[i], 
                               v = tuning_grid$v[i],  
                               bind_rows(fold_preds))
    
    # print
    message(glue('tuning model {i} of {nrow(tuning_grid)}'))
    
  }
  
  # return
  return(tuning_preds)
  
}


# process tuning results
process_cv_tuning = function(tuning_preds){
  
  df_tuning_results1 = 
    tuning_preds %>% 
    bind_rows() %>% 
    group_by(d, p, v, fold) %>% #, fold
    summarise(mean_pred = mean(preds),
              sd_pred = sd(preds))  %>% 
    ungroup() %>%
    group_by(d, p, v) %>%
    summarise(macro_mean_pred = mean(mean_pred), macro_sd_pred = mean(sd_pred)) 
  
  df_tuning_results2 = 
    tuning_preds %>% 
    bind_rows() %>% 
    group_by(d, p, v) %>% #, fold
    summarise(mirco_mean_pred = mean(preds),
              micro_sd_pred = sd(preds))
  
  df_tuning_results = 
    df_tuning_results1 %>% 
    left_join(df_tuning_results2, by = c("d", "p", "v")) %>% 
    ungroup()
  
  return(df_tuning_results)
  
}


# process tuning results, with prediction threshold
process_cv_tuning2 = function(tuning_preds){
  
  df_tuning_results1 = 
    tuning_preds %>% 
    bind_rows() %>% 
    mutate(pt = if_else(preds >= 0.95, 1, 0)) %>% 
    group_by(d, p, v, fold) %>% #, fold
    summarise(acc_pred = sum(pt)/n(),
              sd_pred = sd(pt))  %>% 
    ungroup() %>%
    group_by(d, p, v) %>%
    summarise(macro_mean_acc_pred = mean(acc_pred), 
              macro_sd_pred = mean(sd_pred)) 
  
  df_tuning_results2 = 
    tuning_preds %>% 
    bind_rows() %>% 
    mutate(pt = if_else(preds >= 0.95, 1, 0)) %>% 
    group_by(d, p, v) %>% #, fold
    summarise(mirco_mean_acc_pred = sum(pt)/n(),
              micro_sd_pred = sd(pt))
  
  df_tuning_results = 
    df_tuning_results1 %>% 
    left_join(df_tuning_results2, by = c("d", "p", "v")) %>% 
    ungroup()
  
  return(df_tuning_results)
  
}


# run full model and pred to australia

## function
run_full_and_pred_aus = function(spp_data_sf, 
                                 spp_name,
                                 d, p, v,
                                 r_template, aus_mat){
  
  # run model on full data and then pred on aus
  rb_full = rb(x = (spp_data_sf %>% st_drop_geometry() %>% as.matrix()), 
               d = d,
               p = p,
               v = v)
  preds_aus = rb.test(rb_full, aus_mat)
  
  # attach to raster
  ## add pred values to ind
  pred = tibble( pred = preds_aus, id = as.integer(rownames(aus_mat)) )
  
  ## add back to raster
  template = tibble(id = 1:length(r_template@data@values)) %>% 
    left_join(pred, by = "id")
  
  r_template@data@values = template$pred
  r_template@data@names = paste0(paste0(spp_name, collapse = "_"), "_range_bagging_suitability")
  r_template@file@name = paste0(paste0(spp_name, collapse = "_"), "_range_bagging_suitability")
  r_template@data@min = min(r_template@data@values, na.rm = T)
  r_template@data@max = max(r_template@data@values, na.rm = T)
  
  return(r_template)
  
}


# reclassify aus preds based on threshold
reclass_preds = function(predsR, threshold){
  
  # reclassify
  m <- c(0, threshold, 0,  threshold, 1, 1) # should select threshold based on prop of cells categorized corrently
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  r3 = reclassify(predsR, rclmat)
  return(r3)
  
}
