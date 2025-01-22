library(sf)
library(tidyverse)
library(mapview)
library(rnaturalearth)

# load data
df = readRDS('data/_final_gt_with_oor.rds')
suitable = read_rds('data/range_bagging/summary_spp_traded_suitable.rds')


## load in suburbs geometries
suburbs = read_rds('data/suburb_geometries/website_suburb_geometries.rds')
suburbs = st_transform(suburbs, crs = 3112)


# summarize by suburb OOR ads
df2 = 
  df %>% 
  filter(outside_range == 1) %>% 
  count(SSC_CODE16, SSC_NAME16)

# add into geom
suburbs = suburbs %>% left_join(df2)


# create a hex grid

aus = rnaturalearth::ne_countries(scale = 110, country = "Australia", 
                                  returnclass = "sf") %>% 
                      st_transform(crs = 3112)
aus_states = st_read("data/ne_50m_admin_1_states_provinces/ne_50m_admin_1_states_provinces.shp")
aus_states = aus_states %>% 
  filter(admin == "Australia") %>% 
  st_transform(crs = 3112)

grid1 = aus %>% st_make_grid(c(1.2e5, 1.2e5), what = "polygons", square = FALSE)
# grid1 = suburbs %>% st_make_grid(c(0.8e5, 0.8e5), what = "polygons")
grid_sf = grid1 %>% st_sf() %>% mutate(grid_id = row_number(), .before=geometry)
grid_sf = st_transform(grid_sf, crs = 3112)

# intersect + add # ads
suburbs_int = 
  suburbs %>% 
  st_join(grid_sf) %>% 
  st_drop_geometry() %>% 
  group_by(grid_id) %>% 
  summarise(n = sum(n, na.rm = TRUE))


# join back in 
grid_vals = 
  grid_sf %>% 
  left_join(suburbs_int) %>% 
  filter(!is.na(n), n > 0)

# map
mapview(grid_vals, zcol = "n")


# ggplot, can add in capital cities? + states
grid_vals %>% 
  ggplot(aes(fill = n)) + 
  geom_sf(data = aus_states, color = NA, fill = "gray90") + 
  geom_sf(color = "white", alpha = 1) + 
  geom_sf(data = aus_states, color = "gray30", fill = NA, linewidth = 0.25) + 
  scale_fill_fermenter(direction=1, palette = "Reds", 
                       breaks = c(10, 100, 500),
                       # labels = c("200", "400", "600")
                       ) + 
  labs(fill = "n ads\noutside of range") + 
  theme_void() +
  theme(legend.position = c(0.35,0.1),
        legend.title = element_text(vjust = 1.25, hjust = 1),
        legend.direction = "horizontal",
        legend.key.width = unit(0.8, "cm"),
        panel.background = element_rect(fill = "white", color = "white"))

ggsave(plot = last_plot(), filename = "plots/maps_oor.png", 
       dpi = 900, width = 6, height = 4, units = "in")


# ---------------------------------------------------------------------------- #

# Nstates

states = tibble( state = 
  suitable %>% 
  filter(n_states_traded_oor_state_suitable_oor > 0) %>% 
  pull(states) %>% 
  str_split(", ") %>% 
  unlist() %>% 
  str_trim()
  ) %>% 
  count(state)

states_sf = aus_states %>% 
  left_join(states, by = c("name" = "state")) %>% 
  select(name, n) %>% 
  filter(name != "Jervis Bay Territory")


m1 = states_sf %>% 
  ggplot() + 
  geom_sf(aes(fill = n), color = "white") + 
  geom_sf_label(aes(label=n), alpha = 0.8) + 
  # scale_fill_viridis_b() + 
  scale_fill_fermenter(direction=1, palette = "Reds", 
                       breaks = c(10, 20, 30),
  ) +
  labs(fill = "No.\nspecies") + 
  theme_void()
m1

ggsave(plot = last_plot(), filename = "plots/map_state_nspp.png", 
       dpi = 300, width = 6, height = 4, units = "in")



# N states by taxa

states_taxa = suitable %>% 
  filter(n_states_traded_oor_state_suitable_oor > 0) %>% 
  separate_rows(states, sep = ", ") %>% 
  count(states, class)

states_taxa_sf = aus_states %>% 
  left_join(states_taxa, by = c("name" = "states")) %>% 
  select(name, class, n) %>% 
  filter(name != "Jervis Bay Territory") %>% 
  complete()

m2 = 
  states_taxa_sf %>% 
  ggplot() + 
  geom_sf(data = aus_states, color = "white", fill = "gray90") + 
  geom_sf(aes(fill = n), color = "white") + 
  geom_sf_label(aes(label=n), alpha = 0.8) + 
  # scale_fill_viridis_b() + 
  scale_fill_fermenter(direction=1, palette = "Reds", 
                       breaks = c(10, 20, 30),
  ) +
  labs(fill = "No.\nspecies") + 
  theme_void() + 
  facet_grid(~class)
m2

ggsave(plot = last_plot(), filename = "plots/map_state_nspp_class.png", 
       dpi = 300, units = "in")
