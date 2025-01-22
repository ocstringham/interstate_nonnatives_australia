library(sf)
library(tidyverse)
library(glue)
library(scales)
library(mapview)
library(patchwork)

## state borders
aus_states = st_read("data/ne_50m_admin_1_states_provinces/ne_50m_admin_1_states_provinces.shp")
aus_states = aus_states %>% 
  filter(admin == "Australia") %>% 
  st_transform(crs = 3112)

# #load in suburbs geometries
# suburbs = read_rds('data/suburb_geometries/website_suburb_geometries.rds')
# suburbs = st_transform(suburbs, crs = 3112)


## load ads data
df = readRDS('data/_final_gt_with_oor.rds')

## load in geometries
distribs = read_rds('data/spp_geometries/_all_spp_geoms_v2.rds')


# load in habitat suitability
habitat = read_rds('data/range_bagging/summary_spp_traded_suitable.rds')


source("scripts/data_summary/plot_one_spp_results_fun.R")

gbifId = habitat$gbif_id[86]
spp = habitat$species[86]

one_spp_plot(gbifId = gbifId, spp = spp, ads_df = df, distribs = distribs, 
             habitat = habitat, palette = "Oranges")

# plot all
for(i in 1:nrow(habitat)){
  
  gbifId = habitat$gbif_id[i]
  spp = habitat$species[i]
  
  if(habitat$class[i] == "Aves") pallete = "Oranges"
  if(habitat$class[i] == "Reptilia") pallete = "Purples"
  if(habitat$class[i] == "Amphibia") pallete = "Greens"
  
  one_spp_plot(gbifId = gbifId, spp = spp, ads_df = df, distribs = distribs, 
               habitat = habitat, palette = pallete)
  
  # save
  ggsave(plot = last_plot(), 
         filename = glue("plots/single_spp/{habitat$class[i]}_{str_replace(spp, ' ', '_')}.png"))
  
  rm(gbifId, spp, pallete)
  
}



# # gbif_ids = list.files('data/range_bagging/pca/')
# gbifId = habitat$gbif_id[40]
# spp = habitat$species[40]
# 
# # get actual suitability
# suitability = read_rds(glue('data/range_bagging/pca/{gbifId}/sf_results.rds'))
# mapview(suitability)
# 
# 
# # plot
# 
# 
# # subset ads by species & out of range
# df_sub = df %>% 
#   filter(species == spp) %>% 
#   filter(outside_range == 1)
# 
# # spatial join ads to suburbs
# ads_sub = df_sub %>% 
#   count(SSC_CODE16, SSC_NAME16) %>% 
#   left_join(suburbs, by = "SSC_CODE16") %>% 
#   st_sf()
# 
# 
# # get spp range
# geom_sub = distribs %>% 
#   # mutate(species = {{spp_col}}) %>% 
#   # left_join(geom_key, by = "species2") %>% 
#   filter(species == spp)
# 
# ## intersect with states
# geom_sub = geom_sub %>% 
#   st_intersection(aus_states)
# 
# 
# # create grid
# grid1 = aus_states %>% st_make_grid(c(2e5, 2e5), what = "polygons", square = FALSE)
# grid_sf = grid1 %>% st_sf() %>% mutate(grid_id = row_number(), .before=geometry)
# grid_sf = st_transform(grid_sf, crs = 3112)
# 
# 
# # count number of ads in each grid cell
# suburbs_int = 
#   ads_sub %>% 
#   st_join(grid_sf) %>% 
#   st_drop_geometry() %>% 
#   group_by(grid_id) %>% 
#   summarise(n = sum(n, na.rm = TRUE))
# 
# 
# # join cell count back to grid geom 
# grid_vals = 
#   grid_sf %>% 
#   left_join(suburbs_int, by = "grid_id") %>% 
#   filter(!is.na(n), n > 0)
# 
# 
# # states where suitable
# states_temp = habitat %>% filter(species == spp) 
# if(!is.na(states_temp$states[1])){
#   states = states_temp %>% pull(states) %>% str_split(", ") %>% unlist()
# }else{
#   states = NA
# }
# 
# suitable = aus_states %>% filter(name %in% states)
# 
# # get n ads outside
# n_out = sum(grid_vals$n, na.rm = TRUE)
# 
# # get family name
# fam = df %>%
#   filter(species == spp) %>%
#   distinct(family) %>%
#   pull(family) %>%
#   `[`(1)
# 
# # Get order name
# ord = df %>%
#   filter(species == spp) %>%
#   distinct(order) %>%
#   pull(order) %>%
#   `[`(1)
# 
# # get class name
# tax_class = df %>%
#   filter(species == spp) %>%
#   distinct(class) %>%
#   pull(class) %>%
#   `[`(1)
# 
# 
# # plot
# palette = "Oranges"
# range_fill = "gray50"
# 
# p = 
#   aus_states %>% 
#   ggplot() + 
#   geom_sf(color = NA, fill = "gray90") + 
#   geom_sf(data = suitable, color = NA, fill = RColorBrewer::brewer.pal(3, palette)[2]) +
#   geom_sf(data = suitability %>% summarise(),
#           color = "black", fill = "black", alpha = 1) +
#   # geom_sf(color = "gray30", alpha = 0.7) + 
#   geom_sf(data = geom_sub, fill = range_fill, color = NA, alpha = 0.5) +
#   geom_sf(color = "white", fill = NA, linewidth = 0.5) +
#   # scale_fill_continuous() + 
#   scale_fill_fermenter(direction=1, palette = palette, breaks = pretty_breaks(4)) +
#   labs(fill = "", title = spp,
#        # subtitle = common_name,
#        subtitle = glue("{fam} | {ord} \nn = {comma(n_out)}")
#   ) + # fill = "n ads outside\nof range", 
#   # guides(fill = "none") + 
#   theme_void() +
#   theme(legend.position = c(0.35,0.1),
#         legend.title = element_text(vjust = 1.25, hjust = 1),
#         legend.direction = "horizontal",
#         legend.key.width = unit(0.5, "cm"),
#         legend.key.height = unit(0.4, "cm"),
#         plot.title = element_text(vjust = 0, hjust = 0.5, face = "italic"),
#         plot.subtitle = element_text(vjust = 0, hjust = 0.5),
#         panel.background = element_rect(fill = "white", color = "white"), 
#         plot.background = element_rect(fill = "white", color = "white"))
# 
# p
# 
# ggsave(plot = last_plot(), filename = "plots/one-spp.png")
