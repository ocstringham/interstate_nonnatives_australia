library(raster)
library(tidyverse)
library(glue)
library(maptools)

# get covariate data observations for Australia

# load in world clim pca
wc1 = raster('data/world_clim/pca/wc2.1_5m_bio_PCA_1.tif') %>% readAll()
wc2 = raster('data/world_clim/pca/wc2.1_5m_bio_PCA_2.tif') %>% readAll()
wc3 = raster('data/world_clim/pca/wc2.1_5m_bio_PCA_3.tif') %>% readAll()
wc4 = raster('data/world_clim/pca/wc2.1_5m_bio_PCA_4.tif') %>% readAll()
wc5 = raster('data/world_clim/pca/wc2.1_5m_bio_PCA_5.tif') %>% readAll()
wc6 = raster('data/world_clim/pca/wc2.1_5m_bio_PCA_6.tif') %>% readAll()
wc_all = stack(wc1, wc2, wc3, wc4, wc5, wc6)

# clip stack to australia
data(wrld_simpl)
SPDF <- subset(wrld_simpl, NAME=="Australia")
plot(SPDF)
extent(SPDF)
mapview(SPDF)

## crop and mask
wc_all2 <- crop(wc_all, c(112, 154, -44, -10))
wc_all3 <- mask(wc_all2, SPDF)

wc_all3

plot(wc_all3[[6]], colNA = "gray")




## get vals for each pca raster
wc1_in_r = wc_all3[[1]]@data@values
wc2_in_r = wc_all3[[2]]@data@values
wc3_in_r = wc_all3[[3]]@data@values
wc4_in_r = wc_all3[[4]]@data@values
wc5_in_r = wc_all3[[5]]@data@values
wc6_in_r = wc_all3[[6]]@data@values

# cov_mat = matrix(c(wc1_in_r, wc2_in_r, wc3_in_r, 
#                    wc4_in_r, wc5_in_r, wc6_in_r), 
#                  ncol = 6, 
#                  nrow = length(wc1_in_r), 
#                  byrow = F)

df = tibble(pc1 = wc1_in_r,
            pc2 = wc2_in_r,
            pc3 = wc3_in_r,
            pc4 = wc4_in_r,
            pc5 = wc5_in_r,
            pc6 = wc6_in_r) %>% 
  mutate(id = row_number(), .before = pc1)

# matrix
cov_mat = 
  df %>% 
  select(-id) %>% 
  as.matrix()


# save
saveRDS(df, 'data/world_clim/pca_covariate_matrix_aus.rds')


# create a blank raster map to populate
r = wc_all3[[1]]
plot(r)
r@data@values = rep(NA, length(r@data@values))
plot(r)
saveRDS(r, 'data/world_clim/template_aus_raster.rds')
