# Script to resolve synonym names because by the time I first ran the analysis to writing up the paper some species have split into mulitple species 
# I did this after running the workflow once and then reran everything again

library(tidyverse)
library(sf)
library(mapview)


# load in geoms
distribs = read_rds('data/spp_geometries/_all_spp_geoms_v1.rds')
View(distribs %>% st_drop_geometry())

reptiles = read_rds('data/spp_geometries/reptiles_subset.rds')
View(reptiles %>% st_drop_geometry())

## look at 2 problem spp

## 1. Antaresia childreni & A. stimsoni
prob1 = reptiles %>% 
  filter(Binomial %in% c("Antaresia childreni", "Antaresia stimsoni"))
mapview(prob1)



## 2. Chelodina oblonga & C. collei
prob2 = reptiles %>% 
  filter(Binomial %in% c("Chelodina oblonga", "Chelodina colliei"))
mapview(prob2)



# merge them
# distribs[1, ncol(distribs)]

poly1 = prob1 %>% summarise() %>% st_transform(3112) #%>% st_cast("GEOMETRY")
mapview(st_geometry(poly1))

poly2 = prob2 %>% summarise()  %>% st_transform(3112) #%>% st_cast("GEOMETRY")
mapview(poly2)


# replace them
distribs2 = distribs
distribs2$geometry[distribs2$species == "Antaresia childreni"] = st_geometry(poly1)
distribs2$geometry[distribs2$species == "Chelodina oblonga"] = st_geometry(poly2)
distribs2

distribs2 %>% 
  filter(species == "Antaresia childreni") %>% 
  mapview()

distribs2 %>% 
  filter(species == "Chelodina oblonga") %>% 
  mapview()

distribs2 %>% 
  # filter(species == "Cacatua galerita") %>%
  slice(100) %>% 
  mapview()



# Add in Nephrurus levis
prob3 =  reptiles %>% 
  filter(Binomial %in% c("Nephrurus levis"))
mapview(prob3)

poly3 = prob3 %>% st_transform(3112)

distribs3 = distribs2 %>% 
  bind_rows(tibble(gbif_id = "2447717", species = "Nephrurus levis",
                   class = "Reptilia", geometry = st_geometry(poly3)) %>% 
              st_as_sf()) %>% 
  filter(!is.na(species))


distribs3 %>% 
  filter(species == "Nephrurus levis") %>% 
  mapview()

distribs3 %>% 
  # filter(species == "Cacatua galerita") %>%
  slice(100) %>% 
  mapview()



# save
saveRDS(distribs3, 'data/spp_geometries/_all_spp_geoms_v2.rds')
