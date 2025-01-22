library(tidyverse)
library(scales)
library(patchwork)
# library(officer)
# library(flextable)

# load data
df = readRDS('data/_final_gt_with_oor.rds')

#  n ads total
df %>% 
  # filter*
  summarise(n())

# total n spp, n ads
df %>% 
  filter(!is.na(outside_range)) %>% 
  # group_by(outside_range) %>% 
  summarise(n = n(),
            # p = n/nrow(df),
            n_spp = n_distinct(species, na.rm = TRUE))

# n spp, n ads OOR
df %>% 
  filter(!is.na(outside_range)) %>% 
  group_by(outside_range) %>%
  summarise(n = n(),
            p = n/nrow(df),
            n_spp = n_distinct(species, na.rm = TRUE)) %>% 
  filter(outside_range == 1)


# n spp, n ads by class 
dfs_class = 
  df %>% 
  filter(!is.na(outside_range)) %>% 
  group_by(outside_range, class) %>% 
  filter(outside_range == 1) %>% 
  summarise(n = n(),
            # p = n/n(), # needs to be class total to make sense
            n_spp = n_distinct(species, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(
    df %>% 
      filter(!is.na(outside_range)) %>% 
      group_by(class) %>% 
      summarise(n_all = n(),
                n_spp_all = n_distinct(species, na.rm = TRUE)) %>% 
      ungroup(),
    by = c("class")
  ) %>% 
  mutate(p_n = n/n_all,
         p_spp = n_spp/n_spp_all) %>% 
  select(-outside_range) %>% 
  select(class, n_all, n_oor = n, p_n, n_spp_all, n_spp_oor = n_spp, p_spp)
dfs_class



# n ads and proportion ads OOR by spp
dfs_oor_spp =
  df %>% 
  # mutate(taxa = fct_relevel(taxa, c("amphibians", "reptiles", "birds"))) %>% 
  group_by(species, class) %>% 
  summarise(n_oor = sum(outside_range, na.rm = TRUE),
            n_w_loc = sum(!is.na(outside_range)),
            p_out = n_oor/n_w_loc) %>% 
  ungroup() 

# median/average prop per species
dfs_oor_spp %>% summarise(median(p_out), mean(p_out))

# how many spp with 0/1 prop
dfs_oor_spp %>% 
  count(p_out) %>% 
  arrange(-p_out) %>% 
  filter(p_out %in% c(0,1))






# ---------------------------------------------------------------------------- #

# Figures

# bar plot n ads OOR
p1 = 
  dfs_class %>% 
  ggplot(aes(x = class, y = n_oor, color = class, fill = class)) + 
  geom_bar(stat = "identity", linewidth = 1.05) + 
  geom_text(aes(label = glue("{round(p_n, 2)*100}%")), vjust= -0.5, size = 2.75) +
  scale_y_continuous(breaks = pretty_breaks(), labels = comma,
                     expand = c(0, 0), limits = c(0, 2400)) + 
  scale_fill_manual(values = c("Amphibia" = "#1b9e77", "Reptilia" = "#7570b3",
                               "Aves" = "#d95f02")) +
  scale_color_manual(values = c("Amphibia" = "#188e6b", "Reptilia" = "#6964a1",
                                "Aves" = "#c35501")) +
  labs(y = "Number of ads\noutside of native range") + 
  guides(color = "none", fill = "none") +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank()) 
p1

# bar plot n spp OOR
p2 = 
  dfs_class %>% 
  ggplot(aes(x = class, y = n_spp_oor, color = class, fill = class)) + 
  geom_bar(stat = "identity", linewidth = 1.05) + 
  geom_text(aes(label = glue("{round(p_spp, 2)*100}%")), vjust= -0.5, size = 2.75) +
  scale_y_continuous(breaks = pretty_breaks(3), labels = comma,
                     expand = c(0, 0), limits = c(0, 80)) + 
  scale_fill_manual(values = c("Amphibia" = "#1b9e77", "Reptilia" = "#7570b3",
                               "Aves" = "#d95f02")) +
  scale_color_manual(values = c("Amphibia" = "#188e6b", "Reptilia" = "#6964a1",
                                "Aves" = "#c35501")) +
  labs(y = "Number of species traded\noutside of native range") + 
  guides(color = "none", fill = "none") +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank()) 
p2


# histogram prop ads oor by spp
p3 = 
  dfs_oor_spp %>% 
  ggplot(aes(x = p_out, fill = class, color = class)) +
  geom_histogram(binwidth = 0.1, linewidth = 1.05) + #, color = "grey"
  scale_y_continuous(expand = c(0, 0), limits = c(0, 65)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) + 
  labs(y = 'Number of species', x = 'Proportion of ads outside of native range',
       color = 'taxa') + 
  scale_fill_manual(values = c("Amphibia" = "#1b9e77", "Reptilia" = "#7570b3",
                               "Aves" = "#d95f02")) +
  scale_color_manual(values = c("Amphibia" = "#188e6b", "Reptilia" = "#6964a1",
                                "Aves" = "#c35501")) +
  guides(color = "none") + 
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "top") + 
  NULL

p3


(p1 + p2) / p3 + plot_annotation(tag_levels = "a")

ggsave(plot = last_plot(), filename = "plots/nads_nsp_oor.png", dpi = 300,
       width = 6.5, height = 4.2, units = "in", scale = 1.1)



# ---------------------------------------------------------------------------- #




# Scatter plot n ads vs n ads OOR by spp
dfs_oor_spp %>% 
  ggplot(aes(x = n_w_loc, y = n_oor, 
             fill = class, color = class)) + 
  geom_point(aes(), size = 2, alpha = 0.5) +
  # geom_jitter(size = 2, alpha = 0.5, height = 0.05) +
  # geom_hex() + 
  scale_fill_manual(values = c("Amphibia" = "#1b9e77", "Reptilia" = "#7570b3",
                               "Aves" = "#d95f02")) +
  scale_color_manual(values = c("Amphibia" = "#188e6b", "Reptilia" = "#6964a1",
                                "Aves" = "#c35501")) +
  labs(x = "Number of ads", y = "Number of ads outside of native range") + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = 'log1p') +
  guides(color = "none", fill = "none") +
  facet_wrap(~class, scales = "free") +
  theme_bw() + 
  NULL

ggsave(plot = last_plot(), filename = 'plots/nout_ntot.png', dpi = 300,
       height = 3.3, width = 8.4, units = "in")



# ---------------------------------------------------------------------------- #

# # by class?
# df %>% 
#   group_by(species, class) %>% 
#   summarise(n = sum(outside_range, na.rm = TRUE),
#             n_w_loc = sum(!is.na(outside_range)),
#             p_out = n/n_w_loc) %>% 
#   filter(n > 0) %>% 
#   ungroup() %>% 
#   summarise(n_row = sum(n),
#             p_row = n_row/sum(n_w_loc),
#             n_spp = n_distinct(species, na.rm = TRUE))
# 
# 
# df %>% 
#   group_by(species, common_name, class) %>% 
#   summarise(n = sum(outside_range, na.rm = TRUE),
#             n_w_loc = sum(!is.na(outside_range)),
#             p_out = n/n_w_loc) %>% 
#   filter(n > 0) %>% 
#   ungroup() %>% 
#   summarise(n_row = sum(n),
#             p_row = n_row/sum(n_w_loc),
#             n_spp = n_distinct(species, na.rm = TRUE))
# 
# # n spp, n ads ... No location
# df %>%
#   filter(is.na(SSC_NAME16)) %>%
#   summarise(n_row = n(),
#             n_ads = n_distinct(classifieds_unique_listing_id),
#             n_spp = n_distinct(species, na.rm = TRUE),
#             n_taxa = n_distinct(gbif_name))
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
# 
# # by species with no outside
# s_spp = 
#   df %>%
#   filter(!is.na(SSC_NAME16)) %>%
#   group_by(species) %>% 
#   filter(sum(outside_range) == 0) %>% 
#   summarise(n_row = n(),
#             n_ads = n_distinct(classifieds_unique_listing_id))
# s_spp

