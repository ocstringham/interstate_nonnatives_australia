library(tidyverse)
library(scales)
library(patchwork)
library(officer)
library(flextable)

# load data
df = readRDS('data/_final_gt_with_oor.rds')
suitable = read_rds('data/range_bagging/summary_spp_traded_suitable.rds')


# popular species ... have to merge common name back in bc not unique
t1 = 
  df %>% 
  group_by(species, class) %>% 
  summarise(n = sum(outside_range, na.rm = TRUE),
            n_w_loc = sum(!is.na(outside_range)),
            p_out = n/n_w_loc) %>% 
  arrange(-n) %>% 
  ungroup() %>% 
  left_join(df %>% filter(gbif_rank == "species") %>% 
              distinct(species, .keep_all = T) %>% 
              select(species, common_name), 
            by = "species") %>% 
  mutate(common_name = ifelse(species == "Nephrurus levis", 
                              "Common knob-tailed gecko", common_name)) %>% 
  left_join(suitable %>% 
              select(species, n_states = n_states_traded_oor_state_suitable_oor)) %>% 
  top_n(n = 20, wt = n) %>%
  mutate(p_out = round(p_out, 2)) %>% 
  select(Species = species, 
         "Common name" = common_name, 
         Class = class,
         "Number of ads OOR" = n,
         "Number of suitable states OOR" = n_states)  %>% 
  flextable() %>%
  # align(j = c(5), align = "right") %>%
  bold(part = "header") %>%
  # set_table_properties(layout = "autofit") %>%
  width(j = 1, width = 1.75) %>%
  width(j = 2, width = 1.8) %>%
  # width(j = 5, width = 1.25) %>%
  italic(i = NULL, j = 1, part = "body") %>%
  font(fontname = "Calibri", part = "all") %>% 
  fontsize(size = 11, part = "all")
t1

save_as_docx(t1, path = "plots/top_sp.docx")
  

# Popular families
fam1 = 
  df %>% 
  group_by(family) %>% 
  summarise(n_spp = n_distinct(species, na.rm = TRUE),
            n = sum(outside_range, na.rm = TRUE),
            # n_sd = sd(sum(outside_range, na.rm = TRUE), na.rm = TRUE),
            n_w_loc = sum(!is.na(outside_range)),
            p_out = n/n_w_loc) %>% 
  arrange(-n) %>% 
  ungroup() %>% 
  top_n(n = 20, wt = n)
fam1

# get average by species
fam2 = 
  df %>% 
  filter(!is.na(species)) %>% 
  group_by(family, species) %>% 
  summarise(n = sum(outside_range, na.rm = TRUE),
            n_w_loc = sum(!is.na(outside_range)),
            p_out = n/n_w_loc) %>% 
  ungroup() %>% 
  group_by(family) %>% 
  summarise( # n_spp = n_distinct(species, na.rm = TRUE),
            n_avg = mean(n, na.rm = TRUE),
            n_med = median(n, na.rm = TRUE),
            # n_sd = sd(n, na.rm = TRUE),
            # n_w_loc_avg = mean(n_w_loc, na.rm = TRUE),
            # n_w_loc_sd = sd(n_w_loc, na.rm = TRUE),
            p_out_avg = mean(p_out, na.rm = TRUE),
            # p_out_sd = sd(p_out, na.rm = TRUE)
            )
fam2

# nspecies traded outside
fam3 =  
  df %>% 
  filter(!is.na(species)) %>% 
  filter(outside_range == 1) %>% 
  group_by(family) %>% 
  summarise(n_spp_oor = n_distinct(species))
fam3

# nspp suitable
fam4 = 
  df %>% 
  filter(!is.na(species)) %>% 
  filter(species %in% (suitable %>% 
                         filter(n_states_traded_oor_state_suitable_oor > 0) %>% 
                         pull(species))) %>% 
  group_by(family) %>% 
  summarise(n_spp_suitable = n_distinct(species))
fam4


# Table: Family, n_spp, n_out_total, 
t2 = 
  fam1 %>% 
  left_join(fam2) %>% 
  arrange(-n_w_loc) %>% 
  select(-n, -p_out) %>% 
  left_join(fam3) %>% 
  left_join(fam4) %>% 
  mutate(n_avg = round(n_avg, 1),
         p_out_avg = round(p_out_avg, 2)) %>% 
  top_n(10, n_w_loc) %>% 
  arrange(-n_spp) %>% 
  select(family, n_spp, n_spp_oor, n_spp_suitable, n_avg, everything()) %>% 
  rename(Family = family,
         `Number of species traded` = n_spp,
         # `Number of ads OOR` = n_w_loc,
         `Number of species traded OOR` = n_spp_oor,
         `Number of species traded suitable` = n_spp_suitable,
         `Avg. number of ads/species OOR` = n_avg) %>% 
         # `Avg. proportion of ads outside range/species` = p_out_avg) %>% 
  select(-n_w_loc, -p_out_avg, -n_med) %>% 
  flextable() %>%
  bold(part = "header") %>%
  # set_table_properties(layout = "autofit") %>%
  # width(j = 2, width = 1.8) %>%
  # width(j = 5, width = 1.25) %>%
  # italic(i = NULL, j = 1, part = "body") %>%
  font(fontname = "Calibri", part = "all") %>% 
  fontsize(size = 11, part = "all")
t2

save_as_docx(t2, path = "plots/top_fams.docx")


# ----------------------------------------------------------------------------- #

# # get species stats
# spp = 
#   df %>% 
#   group_by(species, class) %>% 
#   summarise(n = sum(outside_range, na.rm = TRUE),
#             n_w_loc = sum(!is.na(outside_range)),
#             p_out = n/n_w_loc) %>% 
#   arrange(-n) %>% 
#   ungroup() %>% 
#   left_join(df %>% filter(gbif_rank == "species") %>% 
#               distinct(species, .keep_all = T) %>% 
#               select(species, common_name), 
#             by = "species") 
# spp
# 
# 
# # n spp, n ads
# df %>%
#   filter(!is.na(SSC_NAME16)) %>%
#   summarise(n_row = n(),
#             n_ads = n_distinct(classifieds_unique_listing_id),
#             n_spp = n_distinct(species, na.rm = TRUE),
#             n_taxa = n_distinct(gbif_name))
# 
# # n spp, n ads outside
# df %>%
#   filter(!is.na(SSC_NAME16)) %>%
#   filter(outside_range == 1) %>% 
#   summarise(n_row = n(),
#             n_ads = n_distinct(classifieds_unique_listing_id),
#             n_spp = n_distinct(species, na.rm = TRUE),
#             n_taxa = n_distinct(gbif_name))
# 
# # prop outside
# df %>% 
#   group_by(species, class) %>% 
#   summarise(n = sum(outside_range, na.rm = TRUE),
#             n_w_loc = sum(!is.na(outside_range)),
#             p_out = n/n_w_loc) %>% 
#   ungroup() %>% 
#   count(p_out) %>% 
#   filter(p_out %in% c(0,1))
# 
# # n spp, n ads ... No location
# df %>%
#   filter(is.na(SSC_NAME16)) %>%
#   summarise(n_row = n(),
#             n_ads = n_distinct(classifieds_unique_listing_id),
#             n_spp = n_distinct(species, na.rm = TRUE),
#             n_taxa = n_distinct(gbif_name))
# 
# # nspp suitable
# suitable %>% 
#   # filter(n_states_traded_oor_state_suitable_oor > 0) %>% 
#   summarise(n_spp_tot = n(),
#             n_spp_suit = sum(n_states_traded_oor_state_suitable_oor > 0),
#             max_n = max(n_states_traded_oor_state_suitable_oor), 
#             avg_n = mean(n_states_traded_oor_state_suitable_oor))
# table(suitable$n_states_traded_oor_state_suitable_oor)
# 
# # by class
# s_class = 
#   df %>%
#   filter(!is.na(SSC_NAME16)) %>%
#   group_by(class) %>% 
#   summarise(n_row = n(),
#             n_ads = n_distinct(classifieds_unique_listing_id),
#             n_spp = n_distinct(species, na.rm = TRUE),
#             n_taxa = n_distinct(gbif_name))
# s_class
# 
# # bar plot
# p1 = 
#   s_class %>% 
#   ggplot(aes(x = class, y = n_row, color = class, fill = class)) + 
#   geom_bar(stat = "identity", linewidth = 1.05) + 
#   scale_y_continuous(breaks = pretty_breaks(), labels = comma,
#                      expand = c(0, 0), limits = c(0, 6200)) + 
#   scale_fill_manual(values = c("Amphibia" = "#1b9e77", "Reptilia" = "#7570b3",
#                                "Aves" = "#d95f02")) +
#   scale_color_manual(values = c("Amphibia" = "#188e6b", "Reptilia" = "#6964a1",
#                                 "Aves" = "#c35501")) +
#   labs(y = "Number of ads") + 
#   guides(color = "none", fill = "none") +
#   theme_bw() + 
#   theme(axis.title.x = element_blank(),
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank()) 
# p1
# 
# p2 = 
#   s_class %>% 
#   # mutate(class = fct_relevel(class, c(""))) %>% 
#   ggplot(aes(x = class, y = n_spp, color = class, fill = class)) + 
#   geom_bar(stat = "identity", linewidth = 1.05) + 
#   scale_y_continuous(breaks = pretty_breaks(), labels = comma,
#                      expand = c(0, 0), limits = c(0, 100)) + 
#   scale_fill_manual(values = c("Amphibia" = "#1b9e77", "Reptilia" = "#7570b3",
#                                "Aves" = "#d95f02")) +
#   scale_color_manual(values = c("Amphibia" = "#188e6b", "Reptilia" = "#6964a1",
#                                 "Aves" = "#c35501")) +
#   labs(y = "Number of species") + 
#   guides(color = "none", fill = "none") +
#   theme_bw() + 
#   theme(axis.title.x = element_blank(),
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank()) 
# p2
# 
# 
# p3 = 
#   # plot histogram
#   df %>% 
#   group_by(species, class) %>% 
#   summarise(n = sum(outside_range, na.rm = TRUE),
#             n_w_loc = sum(!is.na(outside_range)),
#             p_out = n/n_w_loc) %>% 
#   ggplot(aes(x = p_out, fill = class, color = class)) +
#   geom_histogram(binwidth = 0.1, linewidth = 1.05) + #, color = "grey"
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) +
#   scale_x_continuous(breaks = scales::pretty_breaks()) + 
#   labs(y = 'Number of species', x = 'Proportion of advertisements\noutside of native range',
#        color = 'taxa') + 
#   scale_fill_manual(values = c("Amphibia" = "#1b9e77", "Reptilia" = "#7570b3",
#                                "Aves" = "#d95f02")) +
#   scale_color_manual(values = c("Amphibia" = "#188e6b", "Reptilia" = "#6964a1",
#                                 "Aves" = "#c35501")) +
#   guides(color = "none") + 
#   theme_bw() + 
#   theme(legend.title = element_blank(), legend.position = "top") + 
#   NULL
# 
# 
# (p1 + p2) / p3 + plot_annotation(tag_levels = "a")
# 
# ggsave(plot = last_plot(), filename = "plots/nads_nsp.png", dpi = 300,
#        width = 5.1, height = 4.2, units = "in")