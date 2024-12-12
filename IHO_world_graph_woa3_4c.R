# IHO_graph
library(dplyr)
library(ggplot2)
library(viridis)
library(cowplot)
library(mregions)
theme_set(theme_cowplot())
theme_update(plot.title = element_text(hjust = 0.5))

# install mregions2
# install.packages("mregions2", repos = "https://ropensci.r-universe.dev")
library(mregions2)
# list marine regions products
mrp_list


## load iho sea areas
iho_mr <- mrp_get("iho")

# file exported from \\fs\shared\datac\Projects\LifeWatch\WP2_TaxonomicBB\WOA_3\data\all_OBIS_data_20241004.accdb
iho_table_woa3 <- read.csv(file = "20241004_leen_stats.txt", sep = ",")


# summarise the table according to IHO regions
iho_table_woa3_summary <- iho_table_woa3 %>%
  group_by(NAME, MRGID, depthzone) %>%
  summarise(
    spec = n_distinct(AphiaID),
    rec = sum(records, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = depthzone, values_from = c(spec, rec), values_fill = 0)


# calculate total number of species in each IHO area
iho_table_woa3_summary <- iho_table_woa3_summary %>%
  rowwise() %>%
  mutate(spec_all = sum(c_across(starts_with("spec_")), na.rm = TRUE),
         rec_all = sum(c_across(starts_with("rec_")), na.rm = TRUE)) 
# THIS IS WRONG! I CAN'T JUST SUM SPECIES LISTS!
# I HAVE TO TAKE UNIQUE VALUES!


# rename columns
iho_table_woa3_summary <- iho_table_woa3_summary %>%
  rename(spec_0_200m = 'spec_0-200m',
         spec_1000_11000m = 'spec_1000-11000m',
         spec_200_1000m = 'spec_200-1000m',
         rec_0_200m = 'rec_0-200m',
         rec_200_1000m = 'rec_200-1000m',
         rec_1000_11000m = 'rec_1000-11000m') %>%
  mutate(spec_max = max(spec_0_200m, spec_1000_11000m, spec_200_1000m, na.rm = TRUE))

# change name of Japan Sea in iho_table_order (used for the labels of the graph)
# note that this is only in the iho_table_order, NOT in iho
levels(iho_table_woa3_summary$NAME)[levels(iho_table_woa3_summary$NAME) == 'Japan Sea'] <-
  'Waters between Japan and the Korean peninsula'

# sort dataframe based on total number of species in IHO
iho_table_woa3_summary <- iho_table_woa3_summary %>%
  arrange(spec_max) # sort your dataframe

# reorder the name
iho_table_woa3_summary$NAME <- factor(iho_table_woa3_summary$NAME,
                                   unique(iho_table_woa3_summary$NAME))

# remove line Waters between Japan and the Korean peninsula'
iho_table_woa3_summary <- iho_table_woa3_summary[!(iho_table_woa3_summary$NAME) == "Waters between Japan and the Korean peninsula",]

# max(iho_table_woa3_summary$spec_all)
# [1] 27817

# max(c(iho$spec_0_200m, iho$spec_200_1000m, iho$spec_1000_11000m), na.rm = TRUE)
# [1] 13908

# plot data
p1 <- ggplot(data = iho_table_woa3_summary) +
  geom_segment(aes(x = 0, xend = spec_max, y = NAME, yend = NAME), color="grey") +
  #geom_point(aes(x = spec_all, y = NAME, col = 'c1')) +
  geom_point(aes(x = spec_0_200m, y = NAME, col = 'c1')) +
  geom_point(aes(x = spec_200_1000m, y = NAME, col = 'c2')) +
  geom_point(aes(x = spec_1000_11000m, y = NAME, col = 'c3')) +
  labs(#title = 'Number of Marine Invertabrate Benthic species',
       x = 'number of species',
       y = '') +
  scale_color_manual('Depth Category',
                     values = c("c1" = viridis(4)[1],
                                "c2" = viridis(4)[2],
                                "c3" = viridis(4)[3]),
                                #"c4" = viridis(4)[4]),
                      labels = c("c1" = '< 200 m',
                                 "c2" = '200 - 1000 m',
                                 "c3" = '> 1000 m')
  ) +
  scale_x_continuous(breaks = seq(0,14000,4000),
                     limits = c(0, max(iho_table_woa3_summary$spec_0_200m)))
  # draw_label('IHO areas by MarineRegions.org; species information by WoRMS;
  #            species distributions by OBIS; bathymetry by EMODnet Bathymetry and GEBCO',
  #            x = 13000, y = 2, colour = 'black', size = 12)

legend <- get_legend(p1)



### draw maps
# add attributes to sf dataframe
iho <- merge(x = iho_mr,
             y = iho_table_woa3_summary,
             by.x = 'mrgid',
             by.y = 'MRGID',
             all.x = TRUE)

iho_nogeom <- sf::st_drop_geometry(iho)
View(iho_nogeom)

#### PLOTTING NUMBER OF SPECIES per IHO ####

# max(c(iho$spec_0_200m, iho$spec_200_1000m, iho$spec_1000_11000m), na.rm = TRUE)
# [1] 13908


  columns <- c('spec_s200m','spec_200.1000m', 'spec_l1000m')
  titles <- c('< 200 m', '200 - 1000 m', '> 1000 m')
  # somehow st_crs("ESRI:54030") doesn't work anymore
  prj <- 'PROJCS["World_Robinson",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],PROJECTION["Robinson"],PARAMETER["longitude_of_center",0],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["ESRI","54030"]]'
  
  
ps200 <- ggplot(iho) +
    geom_sf(aes(fill = spec_0_200m), size = 0.01) +
    coord_sf(crs='ESRI:54030') +
    scale_fill_viridis_c('# species', option = "plasma", limits = c(0,14000)) +
    ggtitle('< 200 m') +
    theme(panel.grid.major = element_line(colour = 'transparent')) +
    coord_sf(crs = st_crs(prj))
  # save_plot(paste0('species_depth_', columns[i],'.pdf'),
  #           p,
  #           base_aspect_ratio = 2)

p200_1000 <- ggplot(iho) +
  geom_sf(aes(fill = spec_200_1000m), size = 0.01) +
  coord_sf(crs='ESRI:54030') +
  scale_fill_viridis_c('# species', option = "plasma", limits = c(0,14000)) +
  ggtitle('200 - 1000 m') +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf(crs = st_crs(prj))
# save_plot(paste0('species_depth_', columns[i],'.pdf'),
#           p,
#           base_aspect_ratio = 2)

pl1000 <- ggplot(iho) +
  geom_sf(aes(fill = spec_1000_11000m), size = 0.01) +
  coord_sf(crs='ESRI:54030') +
  scale_fill_viridis_c('# species', option = "plasma", limits = c(0,14000)) +
  ggtitle('> 1000 m') +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf(crs = st_crs(prj))
# save_plot(paste0('species_depth_', columns[i],'.pdf'),
#           p,
#           base_aspect_ratio = 2)



iho_world_graph <- ggdraw() +
  draw_plot(plot = p1 + theme(legend.position = "none"),
            x = 0.05,
            y = 0,
            width = 0.95,
            height = 1) +
  draw_plot(ps200,
             x = 0.5,
             y = 0.5,
             width = 0.4,
             height = 0.4) +
  draw_plot(p200_1000,
             x = 0.5,
             y = 0.3,
             width = 0.4,
             height = 0.4) +
  draw_plot(pl1000,
             x = 0.5,
             y = 0.1,
             width = 0.4,
             height = 0.4) +
  draw_grob(legend,
            x = 0.65,
            y = -.05,
            width = 0.4,
            height = 0.4)

# save_plot('IHO_world_graph.pdf',
#           iho_world_graph,
#           base_height = 15,
#           base_width = 16.5)


save_plot('IHO_world_graph.png',
          iho_world_graph,
          base_height = 15,
          base_width = 16.5)


# # export to ppt
# library(officer)
# library(rvg)
#
# editable_graph <- dml(ggobj = iho_world_graph)
#
# read_pptx('template.pptx') %>%
#   add_slide() %>%
#   ph_with(value = editable_graph,
#           location = ph_location_fullsize()) %>%
#   print(target = 'IHO_world_graph.pptx')
