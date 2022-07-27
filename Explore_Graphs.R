##################################################################
## WG Salmon migration data explore graphs
##
## Matt Brachmann (PhDMattyB)
##
## 2022-06-06
##
##################################################################

## Load the data manipulation work horse
library(tidyverse)

WG_df = read_csv('WG_df_metadata_cleaned.csv')

theme_set(theme_bw())

# Maps package ---------------------------------------------------------------------
library(sf)
library(rnaturalearth)
library(patchwork)

world = ne_countries(scale = "medium", returnclass = "sf")
class(world)

plot_data = WG_df %>% 
  na.omit()

plot_data = WG_df %>% 
  dplyr::select(-collection.x, 
                -log_likelihood, 
                -z_score,
                -n_non_miss_loci, 
                -n_miss_loci) %>% 
  na.omit(Lat,
          Long) 
# plot_data %>% 
#   filter(Lat <= 10.0)

plot_80s = plot_data %>% 
  filter(year %in% c('1983', 
                     '1984'))
#plot a sub region
lat_min_80 = min(plot_80s$Lat)
lat_max_80 = max(plot_80s$Lat)
long_min_80 = min(plot_80s$Long)
long_max_80 = max(plot_80s$Long)

study_range_80s = ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(long_min_80 - 2, 
                    long_max_80 + 2), 
           ylim = c(lat_min_80 - 2, 
                    lat_max_80 + 2), 
           expand = FALSE) +
  labs(title = 'B)')+
  geom_point(data = plot_80s, 
             aes(x = Long, 
                 y = Lat), 
             size = 2, 
             col = '#3a86ff')+
  theme(panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_text(size = 12))

plot_data %>% 
  distinct(year)

plot_90s = plot_data %>% 
  filter(year %in% c('1996', 
                     '1997', 
                     '1998')) %>% 
  filter(Lat > 30.0)

#plot a sub region
lat_min_90 = min(plot_90s$Lat)
lat_max_90 = max(plot_90s$Lat)
long_min_90 = min(plot_90s$Long)
long_max_90 = max(plot_90s$Long)

study_range_90s = ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(long_min_90 - 2, 
                    long_max_90 + 2), 
           ylim = c(lat_min_90 - 2, 
                    lat_max_90 + 2), 
           expand = FALSE) +
  labs(title = 'C)')+
  geom_point(data = plot_90s, 
             aes(x = Long, 
                 y = Lat), 
             size = 2, 
             col = '#ff006e')+
  theme(panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_text(size = 12))


plot_2000 = plot_data %>% 
  filter(year %in% c('2017, 
                      2018,
                     2019', 
                     '2020', 
                     '2021')) %>% 
  filter(Lat > 30.0)

#plot a sub region
lat_min_2000 = min(plot_2000$Lat)
lat_max_2000 = max(plot_2000$Lat)
long_min_2000 = min(plot_2000$Long)
long_max_2000 = max(plot_2000$Long)

study_range_2000s = ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(long_min_2000 - 2, 
                    long_max_2000 + 2), 
           ylim = c(lat_min_2000 - 2, 
                    lat_max_2000 + 2), 
           expand = FALSE) +
  labs(title = 'D)')+
  geom_point(data = plot_2000, 
             aes(x = Long, 
                 y = Lat), 
             size = 2, 
             col = '#8338ec')+
  theme(panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_text(size = 12))

combo_loc_plots = study_range_80s|study_range_90s|study_range_2000s

ggsave(file = '~/Salmond_Migration_Paper/Figures/Study_Locations_Overtime.tiff', 
       plot = combo_loc_plots, 
       dpi = 'retina', 
       units = 'cm', 
       width = 30, 
       height = 20)



whole_dataset = plot_data %>% 
  filter(Lat > 30.0)

lat_min = min(whole_dataset$Lat)
lat_max = max(whole_dataset$Lat)
long_min = min(whole_dataset$Long)
long_max = max(whole_dataset$Long)

study_range = ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(long_min - 2, 
                    long_max + 2), 
           ylim = c(lat_min - 2, 
                    lat_max + 2), 
           expand = FALSE) +
  labs(title = 'A)')+
  geom_point(data = whole_dataset, 
             aes(x = Long, 
                 y = Lat), 
             size = 2, 
             col = '#fb5607')+
  theme(panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_text(size = 12))


ggsave(file = '~/Salmond_Migration_Paper/Figures/Study_Locations_whole_period.tiff', 
       plot = study_range, 
       dpi = 'retina', 
       units = 'cm', 
       width = 20, 
       height = 10)

# Change in proportion per time  ------------------------------------------

WG_df %>% 
  distinct(year) %>% 
  arrange()

WG_df_year = mutate(.data = WG_df,
                        year_fixed = as.factor(case_when(
                          year == '1983' ~ '1980',
                          year == '1984' ~ '1980',
                          year == '1996' ~ '1990',
                          year == '1997' ~ '1990',
                          year == '1998' ~ '1990',
                          year == '2019' ~ '2019',
                          year == '2020' ~ '2020',
                          year == '2021' ~ '2021',
                          year == '2017' ~ '2017',
                          year == '2018' ~ '2018')))

WG_df_year = WG_df_year %>% 
  mutate(WG_fishery = 'Greenland')

WG_prop = WG_df_year %>% 
  group_by(year_fixed,
           WG_fishery,
           # mixture_collection,
           origin_id) %>% 
  summarise(n = n())%>%
  mutate(freq = n / sum(n))

WG_col = c('#023047', 
           '#e76f51')

prop_origin = ggplot(data = WG_prop, 
       aes(x = year_fixed, 
           y = freq))+
  geom_bar(position = 'stack', 
           stat = 'identity', 
           aes(col = origin_id, 
               fill = origin_id))+
  scale_color_manual(values = WG_col)+
  scale_fill_manual(values = WG_col)+
  labs(y = 'Proportion of origin')+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        panel.grid = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 12))

ggsave('~/Salmond_Migration_Paper/Figures/Proportion_EU_vs_NA.tiff', 
       plot = prop_origin, 
       dpi = 'retina', 
       units = 'cm', 
       height = 10, 
       width = 15)


# Distance to Greenland ---------------------------------------------------
install.packages('marmap')
library(marmap)

WG_df_metadata
WG_df_year %>% 
  View()

