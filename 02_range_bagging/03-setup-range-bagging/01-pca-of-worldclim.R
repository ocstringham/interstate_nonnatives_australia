

library(sf)
library(raster)
library(RStoolbox)
library(tidyverse)


# load in rasters
r_files = list.files("data/world_clim/wc2.1_5m_bio", full.names = T)
r_list = map(r_files, ~raster(.x))

# make raster stack
r = raster::stack(r_list)

# pca
r_pca = rasterPCA(r, spca = TRUE, maskCheck=FALSE) #  nSamples = 1e6

summary(r_pca$model)
loadings(r_pca$model)

r_pca$map$PC1@data
r_pca$map[[1:4]]
plot(r_pca$map[[1:4]])
zoom(r_pca$map[[1]], drawExtent())



# # Save
# writeRaster(r_pca$map[[1:6]], 
#             filename='~/../Downloads/wc2.1_5m_bio_PCA.tif', 
#             format="GTiff", 
#             bylayer=TRUE,
#             overwrite=TRUE,
#             options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

# saveRDS(r_pca, "~/../Downloads/wc2.1_5m_bio_PCA.rds")
# r_pca = read_rds("~/../Downloads/wc2.1_5m_bio_PCA.rds")
# test = raster('~/../Downloads/wc2.1_5m_bio_PCA.tif')
# r_pca = raster("data/world_clim/wc2.1_5m_bio_PCA.rds")
