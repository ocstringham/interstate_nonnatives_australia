library(sf)
library(tidyverse)
library(mapview)
library(patchwork)
# library(rnaturalearth)


# state borders
aus_states = st_read("data/ne_50m_admin_1_states_provinces/ne_50m_admin_1_states_provinces.shp")
aus_states = aus_states %>% 
  filter(admin == "Australia") %>% 
  st_transform(crs = 3112)


# load data
df = readRDS('data/_final_gt_with_oor.rds')

## load in spp geometries
distribs = read_rds('data/spp_geometries/_all_spp_geoms_v2.rds')

# clip to aus
distribs_clip = distribs %>% st_intersection(aus_states %>% summarise() %>% st_geometry())


distribs$species[! distribs$species %in% distribs_clip$species]
distribs %>% 
  filter(species == "Varanus timorensis") %>% 
  mapview()


# # check n species in each
# length(unique(amphibs_geom_native$binomial)) == nrow(amphibs_geom_native)


# count overlapping polygons


## create grid
grid1 = aus_states %>% st_make_grid(c(1.2e5, 1.2e5), what = "polygons", square = FALSE)
grid_sf = grid1 %>% st_sf() %>% mutate(grid_id = row_number(), .before=geometry)
grid_sf = st_transform(grid_sf, crs = 3112)


count_spp = function(spp_geom, spp_col){
  
  # intersect distribs with grid
  test = grid_sf %>% 
    st_intersection(spp_geom)
  
  # summarize by n species in each grid
  test2 = test %>% 
    st_drop_geometry() %>% 
    group_by(grid_id) %>% 
    summarise(n = n_distinct({{spp_col}}))
  
  # join back to grid poly and only take cells in aus
  test3 = grid_sf %>% 
    left_join(test2, by = "grid_id") %>% 
    filter(!is.na(n)) %>% 
    st_filter(aus_states, .pred = st_intersects)
  
  return(test3)
}



# takes a bit
amphibs_count = count_spp(distribs_clip %>% filter(class=="Amphibia"), species)
bird_count = count_spp(distribs_clip %>% filter(class=="Aves"), species)
reptile_count = count_spp(distribs_clip %>% filter(class=="Reptilia"), species)


plot_heatmap = function(geom, title, pal = 7, fill_lab = ""){
  
  p = 
    geom %>% 
    ggplot(aes(fill = n)) + 
    geom_sf(data = aus_states, color = NA, fill = "gray90") + 
    geom_sf(color = "white", alpha = 1) + 
    geom_sf(data = aus_states, color = "gray50", fill = NA, linewidth = 0.5) + 
    scale_fill_fermenter(direction=1, palette = pal #, breaks = scales::pretty_breaks(3)
                         ) + 
    labs(fill = fill_lab, title = title) + 
    theme_void() +
    theme(plot.title = element_text(vjust = 0, hjust = 0.5), 
          legend.position = c(0.3,0.1),
          legend.title = element_text(vjust = 1.5, hjust = 1),
          legend.direction = "horizontal",
          legend.key.width = unit(0.5, "cm"),
          legend.key.height = unit(0.3, "cm"),
          panel.background = element_rect(fill = "white", color = "white"))
  
  return(p)
}


p1 = plot_heatmap(amphibs_count, "Amphibians", "Greens", "n\nspecies"); p1
p2 = plot_heatmap(bird_count, "Birds", "Oranges"); p2
p3 = plot_heatmap(reptile_count, "Reptiles", "Purples"); p3


p1 + p2 + p3 + patchwork::plot_annotation(tag_levels = "a")


ggsave(plot = last_plot(), filename = "plots/maps_distribs.png", 
       dpi = 300, width = 16, units = "cm", scale = 1.1)


# # plot
# bird_count %>% 
#   ggplot(aes(fill = n)) + 
#   geom_sf(data = aus_states, color = NA, fill = "gray90") + 
#   geom_sf(color = "white", alpha = 0.9) + 
#   geom_sf(data = aus_states, color = "gray50", fill = NA) + 
#   scale_fill_fermenter(direction=1, palette = 7, 
#                        # breaks = c(200, 400, 600),
#                        # labels = c("200", "400", "600")
#   ) + 
#   labs(fill = "n species", title = "Amphibians") + 
#   theme_void() +
#   theme(legend.position = c(0.35,0.1),
#         legend.title = element_text(vjust = 0.9, hjust = 1),
#         legend.direction = "horizontal",
#         legend.key.width = unit(1.0, "cm"),
#         panel.background = element_rect(fill = "white", color = "white"))





# test = grid_sf %>% st_intersection(amphibs_geom_native)
# test2 = test %>% st_drop_geometry() %>% 
#   group_by(grid_id) %>% summarise(n = n_distinct(binomial))
# test3 = grid_sf %>% left_join(test2) %>% 
#   filter(!is.na(n)) %>% 
#   st_filter(aus_states, .pred = st_intersects)
