library(tidyverse)
library(scales)
library(patchwork)
library(officer)
library(flextable)

# load data
df = readRDS('data/_final_gt_with_oor.rds')
suitable = read_rds('data/range_bagging/summary_spp_traded_suitable.rds')

# : species, taxa, n ads, n ads out, evaled, rb restuls, states
# get species

df1 = df %>% 
  filter(!is.na(species)) %>% 
  group_by(species, family, order, class) %>% 
  summarise(n_ads = sum(!is.na(SSC_CODE16)),
            n_oor = sum(outside_range, na.rm = TRUE)) %>% 
  ungroup()

df2 = 
  df1 %>% 
  left_join(suitable %>% 
              select(-class, -gbif_id) %>% 
              mutate(evaluated = 1, .before = species),
            by = "species") %>% 
  mutate(evaluated = ifelse(is.na(evaluated), 0, evaluated)) %>% 
  rename(n_states = n_states_traded_oor_state_suitable_oor) %>% 
  select(species, n_ads:states, everything())


# Export
write.csv(df2, "data/data_for_appendix/species.csv", row.names = F)


# Get full df for export
df3 = df %>% 
  group_by(classifieds_unique_listing_id) %>% 
  mutate(ad_id = cur_group_id(), .before = page) %>% 
  ungroup() %>% 
  select(-classifieds_unique_listing_id, -page, -row,-price_per_unit, 
         -price_includes_other_items, -hand_raised, -wild_caught, -morph,
         -quantity, -quantity_unspecified) %>% 
  arrange(ad_id)

# export
write.csv(df3, 'data/data_for_appendix/ads.csv', row.names = F)


# spp to flextable
t1 =
  df2 %>% 
  # mutate(evaluated = ifelse(evaluated == 0, "No", "Yes")) %>% 
  rename("Number of ads" = n_ads,
         "Number of ads outside range" = n_oor, 
         "Out of range suitability evaluated?" = evaluated,
         "Number of state(s) suitable" = n_states,
         "State(s) suitable" = states) %>% 
  flextable() %>%
  # align(j = c(5), align = "right") %>%
  bold(part = "header") %>%
  set_table_properties(layout = "autofit") %>%
  # width(j = 1, width = 1.75) %>%
  # width(j = 2, width = 1.8) %>%
  # width(j = 5, width = 1.25) %>%
  italic(i = NULL, j = 1, part = "body") %>%
  font(fontname = "Calibri", part = "all") %>% 
  fontsize(size = 11, part = "all")
t1

save_as_docx(t1, path = "plots/all_sp.docx")  
