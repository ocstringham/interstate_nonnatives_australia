


# get avg raster values around one poly
get_avg_rast_val_one_poly = function(polys, index_of_poly, wc, r){
  
  ## convert to raster
  test_poly = polys[index_of_poly,]
  test_raster = fasterize(test_poly, r)
  
  ## get vals
  test_cells_not_na = which(!is.na(test_raster@data@values))
  
  # if cell values found
  if(length(test_cells_not_na) > 0){
    
    test_wc1_in_r = wc[[1]]@data@values[test_cells_not_na]
    test_wc2_in_r = wc[[2]]@data@values[test_cells_not_na]
    test_wc3_in_r = wc[[3]]@data@values[test_cells_not_na]
    test_wc4_in_r = wc[[4]]@data@values[test_cells_not_na]
    test_wc5_in_r = wc[[5]]@data@values[test_cells_not_na]
    test_wc6_in_r = wc[[6]]@data@values[test_cells_not_na]
    
    ## get average
    test_wc1_avg = mean(test_wc1_in_r, na.rm = TRUE)
    test_wc2_avg = mean(test_wc2_in_r, na.rm = TRUE)
    test_wc3_avg = mean(test_wc3_in_r, na.rm = TRUE)
    test_wc4_avg = mean(test_wc4_in_r, na.rm = TRUE)
    test_wc5_avg = mean(test_wc5_in_r, na.rm = TRUE)
    test_wc6_avg = mean(test_wc6_in_r, na.rm = TRUE)
    
    ## get x, y
    test_xy = st_centroid(test_poly) %>% st_geometry()
    test_x = test_xy[[1]][1]
    test_y = test_xy[[1]][2]
    
    # create df
    test_pca_oor_no_pt_df = 
      tibble(pca1 = test_wc1_avg, pca2 = test_wc2_avg, 
             pca3 = test_wc3_avg, pca4 = test_wc4_avg, 
             pca5 = test_wc5_avg, pca6 = test_wc6_avg,
             x = test_x, y = test_y)
    
  # if no cell values (bc poly too small), take centroid
  }else{
    
    # get centroid
    test_centroid = st_centroid(test_poly)
    test_centroid_sp = as_Spatial(test_centroid)
    
    # get raster values at point
    test_pca_oor_no_pt_df = 
      tibble(pca1 = raster::extract(wc[[1]], test_centroid_sp), 
             pca2 = raster::extract(wc[[2]], test_centroid_sp), 
             pca3 = raster::extract(wc[[3]], test_centroid_sp), 
             pca4 = raster::extract(wc[[4]], test_centroid_sp), 
             pca5 = raster::extract(wc[[5]], test_centroid_sp), 
             pca6 = raster::extract(wc[[6]], test_centroid_sp),
             x = st_geometry(test_centroid)[[1]][1], 
             y = st_geometry(test_centroid)[[1]][2]
             )
  }
  

  return(test_pca_oor_no_pt_df)
  
}

