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

## load ads data
df = readRDS('data/_final_gt_with_oor.rds')

## load in geometries
distribs = read_rds('data/spp_geometries/_all_spp_geoms_v2.rds')
View(st_drop_geometry(distribs))


# load in habitat suitability
habitat = read_rds('data/range_bagging/summary_spp_traded_suitable.rds')

source("scripts/data_summary/plot_one_spp_results_fun.R")


# get candidate spp
line_x = 0.65
pa1 = one_spp_plot(2427671, "Litoria fallax", df, distribs, habitat, "Greens", line_x); pa1
pa2 = one_spp_plot(2427694, "Ranoidea caerulea", df, distribs, habitat, "Greens", line_x); pa2

pb1 = one_spp_plot(2479795, "Neopsephotus bourkii", df, distribs, habitat, "Oranges", line_x); pb1
pb2 = one_spp_plot(2479686, "Trichoglossus haematodus", df, distribs, habitat, "Oranges", line_x); pb2

pr1 = one_spp_plot(9473401, "Gowidon longirostris", df, distribs, habitat, "Purples", line_x); pr1
pr2 = one_spp_plot(2462536, "Tiliqua nigrolutea", df, distribs, habitat, "Purples", line_x); pr2

(pa1 + pa2) / (pb1 + pb2) / (pr1 + pr2)

ggsave(plot = last_plot(), filename = "plots/spp-panel.png", 
       dpi = 300, scale = 1.25) # width = 16, units = "cm", 
