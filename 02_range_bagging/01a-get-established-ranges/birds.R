library(sf)
library(tidyverse)
library(mapview)
library(rgbif)
library(glue)

# species from website
df = readRDS('data/_final_gt_with_oor.rds')
spp = df %>% 
  filter(gbif_rank == "species") %>% # ?
  distinct(species, class, gbif_id) %>% arrange(class) %>% filter(class=="Aves")

# gavia
gavia = read.csv('data/establishments/GAVIA/GAVIA_main_data_table.csv')


# get names of established birds
gavia %>% count(StatusCat)
gavia %>% count(RangeMap)
gavia_names = 
  gavia %>% 
  filter(StatusCat %in% c("Established", "Extirpated", "Breeding")) %>%
  # filter(RangeMap == "Mapped") %>%
  count(Binomial) %>% 
  left_join(gavia %>% distinct(Binomial, CommonName))


# match to gt spp
match1 = gavia_names %>% 
  filter(Binomial %in% spp$species)


# get synonyms to match

# test around
# taxize::gbif_name_usage(name=spp$species[1]) -> test
# test = bind_rows(test$results)
# 
# test2 = name_usage(key = spp$gbif_id[3], data = "synonyms")

syns_df = map2_df(spp$gbif_id, 1:nrow(spp), 
                  function(x, y){
  temp = name_usage(x,  data = "synonyms")
  if("data" %in% names(temp)){
    if(nrow(temp$data) > 0){
          temp2 =
      temp$data %>% 
      filter(nameType == "SCIENTIFIC") %>% 
      select(canonicalName) %>% 
      mutate(species = spp$species[y])
    return(temp2)
    }
  }
})

# syns = 
#   syns_df %>% 
#   filter(canonicalName != species)

# try to match syns
match2 = gavia_names %>% 
  filter(Binomial %in% syns_df$canonicalName)


# Join back in all matches w/gbif name
all_matches = 
  match1 %>% 
  bind_rows(match2) %>% 
  left_join(spp %>% 
            mutate(Binomial = 
               ifelse(species == "Eolophus roseicapilla", 
                      "Cacatua roseicapilla", species))) %>% 
  select(-gbif_id)


# See which records in gavia were mapped, which ones werent
matches_gavia = 
  gavia %>% 
  filter(StatusCat %in% c("Established", "Extirpated", "Breeding")) %>%
  filter(Binomial %in% all_matches$Binomial)

matches_mapped = 
  matches_gavia %>% 
  filter(RangeMap == "Mapped") %>% 
  arrange(Binomial, CountryName, AreaName1, AreaName2)

matches_not_mapped = 
  matches_gavia %>% 
  filter(RangeMap != "Mapped")


# Get ranges
test = st_read(glue("data/establishments/GAVIA/GAVIA_rangemaps/GAVIA_", 
                    {str_replace(all_matches$Binomial[1], " ", "_")}, ".shp"))

test = st_read(("data/establishments/GAVIA/GAVIA_rangemaps/GAVIA_Cacatua_galerita.shp")) %>% 
  st_transform(crs = 4326)

test2 = test %>% st_cast("MULTIPOLYGON") %>% st_cast("POLYGON")
mapview(test2)

## fix some names first because range file names errors
gavia_mapped = 
  matches_mapped %>% 
  distinct(Binomial) %>% 
  mutate(Binomial = case_when(Binomial == "Trichoglossus haematodus" ~ "Trichoglossus haemotodus",
                              Binomial == "Coturnix ypsilophora" ~ "Coturnix ypsilphora",
                              Binomial == "Stagonopleura guttata" ~ "Stagnopleura guttata",
                              TRUE ~ Binomial)) %>% 
  pull()

eranges = map_df(gavia_mapped, 
                 function(x){
                   st_read(glue("data/establishments/GAVIA/GAVIA_rangemaps/GAVIA_", 
                                {str_replace(x, " ", "_")}, ".shp")) %>% 
                     st_transform(crs = 4326) %>% 
                     st_cast("MULTIPOLYGON") %>% 
                     st_cast("POLYGON") %>% 
                     mutate(i = row_number(), .before = SpeciesID)
                   }
                 )
View(eranges %>% st_drop_geometry())

mapview(eranges, zcol = "Binomial")

## add in gbif name
eranges2 = 
  eranges %>% 
  left_join(spp %>% 
              mutate(Binomial = 
                       ifelse(species == "Eolophus roseicapilla", 
                              "Cacatua roseicapilla", species)))


# Save all

## estab ranges
saveRDS(eranges2, "data/establishments/birds/bird_estab_ranges.rds")

## gavia records
saveRDS(matches_mapped, "data/establishments/birds/bird_gavia_records.rds")
