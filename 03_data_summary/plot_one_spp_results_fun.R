# to fun
one_spp_plot = function(gbifId, spp, ads_df, distribs, 
                        habitat, 
                        palette, line_x = 1){
  
  # subset ads by species & out of range
  df_sub = ads_df %>% 
    filter(species == spp) %>% 
    filter(outside_range == 1)
  
  # # spatial join ads to suburbs
  # ads_sub = df_sub %>% 
  #   count(SSC_CODE16, SSC_NAME16) %>% 
  #   left_join(suburbs, by = "SSC_CODE16") %>% 
  #   st_sf()
  
  
  # get spp range
  geom_sub = distribs %>% 
    # mutate(species = {{spp_col}}) %>% 
    # left_join(geom_key, by = "species2") %>% 
    filter(species == spp)
  
  ## intersect with states
  geom_sub = geom_sub %>% 
    st_intersection(aus_states)
  
  # get actual suitability
  suitability = read_rds(glue('data/range_bagging/pca/{gbifId}/sf_results.rds'))
  
  # get full suitability
  full_suitability = read_rds(glue('data/range_bagging/pca/{gbifId}/r_aus_pred_rc.rds'))
  full_suitability_sp = sf::st_as_sf(stars::st_as_stars(full_suitability), 
                                     as_points = FALSE, merge = TRUE) %>% 
    filter(.[[1]] == 1) %>% 
    summarize()
  # mapview(full_suitability_sp)
  
  
  # states where suitable
  states_temp = habitat %>% filter(species == spp) 
  if(!is.na(states_temp$states[1])){
    states = states_temp %>% pull(states) %>% str_split(", ") %>% unlist()
  }else{
    states = NA
  }
  
  suitable = aus_states %>% filter(name %in% states)
  
  
  # get n outside
  n_out = nrow(df_sub)
  
  # get family name
  fam = df %>%
    filter(species == spp) %>%
    distinct(family) %>%
    pull(family) %>%
    `[`(1)
  
  # Get order name
  ord = df %>%
    filter(species == spp) %>%
    distinct(order) %>%
    pull(order) %>%
    `[`(1)
  
  # get class name
  tax_class = df %>%
    filter(species == spp) %>%
    distinct(class) %>%
    pull(class) %>%
    `[`(1)
  
  
  p = 
    ggplot() + 
    # state fill
    geom_sf(data= aus_states, color = NA, fill = "gray90") +
    # spp full suitability
    geom_sf(data = full_suitability_sp, color = NA, fill = "gray40", alpha = 1) +
    # spp subset suitability
    geom_sf(data = suitability,
            color = NA, fill = "black", alpha = 1) +
    # spp native range fill
    geom_sf(data = geom_sub %>% summarize(), 
            fill = "gray50", color = NA, alpha = 0.7, 
            linewidth = 0.75*line_x, linetype = "dashed") +
    # state borders
    geom_sf(data = aus_states,
            color = "white", fill = NA, linewidth = 0.5*line_x, alpha = 1) +
    geom_sf(data = aus_states %>% summarize(),
            color = "white", fill = NA, linewidth = 0.5*line_x, alpha = 1) +
    # spp native range border
    geom_sf(data = geom_sub %>% summarize(),
            fill = NA, color = "gray25", alpha = 1,
            linewidth = 0.5*line_x) + # , linetype = "dashed"
    # state suitability
    geom_sf(data = suitable, 
            fill = NA, color = RColorBrewer::brewer.pal(3, palette)[2],
            linewidth = 0.6*line_x) +
    
    scale_fill_fermenter(direction=1, palette = palette, breaks = pretty_breaks(4)) +
    labs(fill = "", title = spp,
         # subtitle = common_name,
         subtitle = glue("{fam} | {ord}") #  \nn = {comma(n_out)}
    ) + # fill = "n ads outside\nof range", 
    # guides(fill = "none") + 
    theme_void() +
    theme(legend.position = c(0.35,0.1),
          legend.title = element_text(vjust = 1.25, hjust = 1),
          legend.direction = "horizontal",
          legend.key.width = unit(0.5, "cm"),
          legend.key.height = unit(0.4, "cm"),
          plot.title = element_text(vjust = 0, hjust = 0.5, face = "italic", size = 5.5), #, size = 4
          plot.subtitle = element_text(vjust = 0, hjust = 0.5, size = 4.5), #, size = 3.5
          panel.background = element_rect(fill = "white", color = "white"), 
          plot.background = element_rect(fill = "white", color = "white"))
  p
  
  return(p)
  
}