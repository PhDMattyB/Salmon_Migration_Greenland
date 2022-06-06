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
library(maps)
library(scatterpie)

pops = map_data('world') %>% 
  filter(region %in% c('Canada',
                       'Greenland', 
                       'Norway', 
                       'Iceland', 
                       'UK', 
                       'state'))%>% 
  as_tibble()

maine = 
  map_data('state') %>%
  filter(region == 'maine') %>% 
  as_tibble()

mapping_data = bind_rows(pops, 
                         maine)


ggplot(mapping_data) +
  geom_map(data = mapping_data, 
           map = mapping_data, 
           aes(x = long, 
               y = lat, 
               map_id = region), 
           col = 'white', 
           fill = 'black')+
  labs(x = 'Longitude', 
       y = 'Latitude')+
  # scale_fill_manual(values = map_palette)+
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12))+
  geom_scatterpie(data = spread_data, 
                  aes(x = Longitude, 
                      y = Latitude, 
                      group = Population), 
                  pie_scale = 0.75, 
                  cols = colnames(spread_data[,c(6:8)]))+
  coord_fixed()

# leaflet package ---------------------------------------------------------
library(leaflet)

## This plots all rivers where fish were collected
Sites = WG_df %>%
  dplyr::select(repunit,
                River,
                Lat,
                Long) %>%
  as.data.frame()

map = leaflet()
map = addTiles(map)
map = addMarkers(map, 
                 lng = Sites$Long, 
                 lat = Sites$Lat, 
                 popup = Sites$repunit)


## get an average lat and long for the reporting groups

mean_repunit = WG_df %>% 
  dplyr::select(year, 
                repunit, 
                River, 
                Lat, 
                Long) %>% 
  group_by(repunit) %>% 
  na.omit() %>% 
  summarise(mean_lat = mean(Lat), 
            mean_long = mean(Long), 
            median_lat = median(Lat), 
            median_long = median(Long))

map = leaflet()
map = addTiles(map)
map = addMarkers(map, 
                 lng = mean_repunit$median_lat, 
                 lat = mean_repunit$median_long, 
                 popup = mean_repunit$repunit)

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
