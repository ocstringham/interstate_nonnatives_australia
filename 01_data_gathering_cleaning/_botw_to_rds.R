library(sf)

# read in botw
botw = st_read("C:/data/spp_distributions/botw_2020/BOTW/BOTW.gdb")

# save as rds
saveRDS(botw, "C:/data/spp_distributions/botw_2020/BOTW/botw_sf.rds")
