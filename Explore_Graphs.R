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


# world_clim enviro data ---------------------------------------------------------
library(raster)

setwd('~/Salmond_Migration_Paper/Worldclim_data/')

latlong_generator = function(data){
  LatLong = data %>%
    dplyr::select(Long,
                  Lat) %>%
    # arrange(Lat) %>%
    as.data.frame()
  
  Lat = as.numeric(LatLong$Lat)
  Long = as.numeric(LatLong$Long)
  
  LatLong = bind_cols(Long, Lat) %>%
    rename(Long = ...1,
           Lat = ...2) 
  # %>%
  #   na.omit()
}
Data_per_decade = function(List_of_files){
  raster_image = lapply(List_of_files, raster::raster) 
  print('Gathering raster data')
  extraction = lapply(raster_image, extract, y = coords)
  print('Extracting environmental data')
  bound = bind_cols(extraction)
  print('Making a large data set')
  decade_mean = apply(bound, 1, mean) %>% 
    as_tibble()
  print('Calculating mean')
  return(decade_mean)
}


## The code below is for multiple .tif files
setwd('~/Salmond_Migration_Paper/Worldclim_data/Precipitation_1980/')
coords = WG_df_metadata %>% 
  filter(year %in% c('1983', 
                     '1984')) %>%
  na.omit() %>% 
  latlong_generator()
precip_files_80s = list.files(pattern = "*.tif") 
mean_precip_80s = Data_per_decade(precip_files_80s)

setwd('~/Salmond_Migration_Paper/Worldclim_data/Max_Temp_1980/')
coords = WG_df_metadata %>% 
  filter(year %in% c('1983', 
                     '1984')) %>%
  na.omit() %>% 
  latlong_generator()
max_temp_files_80s = list.files(pattern = "*.tif") 
mean_max_temp_80s = Data_per_decade(max_temp_files_80s)

Environment_data_1980 = bind_cols(coords, 
                                  mean_precip_80s, 
                                  mean_max_temp_80s) %>% 
  rename(Precipitation = value...3, 
         Max_temp = value...4)

Data_1980s = WG_df_metadata %>%
  filter(year %in% c('1983', 
                     '1984')) %>% 
  na.omit() %>% 
  dplyr::select(indiv, 
                repunit, 
                mixture_collection,
                origin_id)

Final_1980_Data = bind_cols(Data_1980s, 
                            Environment_data_1980) 

write_csv(Final_1980_Data, 
          file = '~/Salmond_Migration_Paper/Worldclim_data/Worldclim_1980_Data.csv')


setwd('~/Salmond_Migration_Paper/Worldclim_data/Precipitation_1990/')
coords = WG_df_metadata %>% 
    filter(year %in% c('1996', 
                       '1997', 
                       '1998')) %>%
  na.omit() %>% 
  latlong_generator()
precip_files_90s = list.files(pattern = '*tif')
mean_precip_90s = Data_per_decade(precip_files_90s)

setwd('~/Salmond_Migration_Paper/Worldclim_data/Max_Temp_1990/')
coords = WG_df_metadata %>% 
  filter(year %in% c('1996', 
                     '1997', 
                     '1998')) %>%
  na.omit() %>% 
  latlong_generator()
max_temp_files_90s = list.files(pattern = "*.tif") 
mean_max_temp_90s = Data_per_decade(max_temp_files_90s)

Environment_data_1990 = bind_cols(coords, 
                                  mean_precip_90s, 
                                  mean_max_temp_90s) %>% 
  rename(Precipitation = value...3, 
         Max_temp = value...4)

Data_1990s = WG_df_metadata %>%
  filter(year %in% c('1996', 
                     '1997', 
                     '1998')) %>% 
  na.omit() %>%
  dplyr::select(indiv, 
                repunit, 
                mixture_collection,
                origin_id) 

Final_1990_Data = bind_cols(Data_1990s, 
                            Environment_data_1990) 

write_csv(Final_1990_Data, 
          file = '~/Salmond_Migration_Paper/Worldclim_data/Worldclim_1990_Data.csv')


setwd('~/Salmond_Migration_Paper/Worldclim_data/Precipitation_Upper_2000/')
coords = WG_df_metadata %>% 
  filter(year %in% c('2017', 
                     '2018', 
                     '2019', 
                     '2020', 
                     '2021')) %>%
  na.omit() %>% 
  latlong_generator()
precip_files_2000s = list.files(pattern = '*tif')
mean_precip_2000s = Data_per_decade(precip_files_2000s)

setwd('~/Salmond_Migration_Paper/Worldclim_data/Max_Temp_1990/')
coords = WG_df_metadata %>% 
  filter(year %in% c('2017', 
                     '2018', 
                     '2019', 
                     '2020', 
                     '2021')) %>%
  na.omit() %>% 
  latlong_generator()
max_temp_files_2000s = list.files(pattern = "*.tif") 
mean_max_temp_2000s = Data_per_decade(max_temp_files_2000s)

Environment_data_2000 = bind_cols(coords, 
                                  mean_precip_2000s, 
                                  mean_max_temp_2000s) %>% 
  rename(Precipitation = value...3, 
         Max_temp = value...4)

Data_Upper_2000 = WG_df_metadata %>%
  filter(year %in% c('2017', 
                     '2018', 
                     '2019', 
                     '2020', 
                     '2021')) %>% 
  na.omit() %>%
  dplyr::select(indiv, 
                repunit, 
                mixture_collection,
                origin_id) 

Final_Upper_2000_Data = bind_cols(Data_Upper_2000, 
                            Environment_data_2000)

write_csv(Final_Upper_2000_Data, 
          file = '~/Salmond_Migration_Paper/Worldclim_data/Worldclim_Upper_2000_Data.csv')


# Wordclim historical bioclimatic data ------------------------------------
## Not going to work... averaged from the 70s-2000
# 
# library(raster)
# 
# setwd('~/Salmond_Migration_Paper/Worldclim_data/Historical_bioclimatic_data/')
# 
# # precip_01 = raster::raster("wc2.1_30s_prec_01.tif") 
# ## If we do it this way be have to come up with an average
# ## for each year for each variable. That's going to be a lot of
# ## mindless coding and function creating. 
# bio1 = raster::raster('wc2.1_30s_bio_1.tif')
# bio2 = raster::raster('wc2.1_30s_bio_2.tif')
# bio3 = raster::raster('wc2.1_30s_bio_3.tif')
# bio4 = raster::raster('wc2.1_30s_bio_4.tif')
# bio5 = raster::raster('wc2.1_30s_bio_5.tif')
# bio6 = raster::raster('wc2.1_30s_bio_6.tif')
# bio7 = raster::raster('wc2.1_30s_bio_7.tif')
# bio8 = raster::raster('wc2.1_30s_bio_8.tif')
# bio9 = raster::raster('wc2.1_30s_bio_9.tif')
# bio10 = raster::raster('wc2.1_30s_bio_10.tif')
# bio11 = raster::raster('wc2.1_30s_bio_11.tif')
# bio12 = raster::raster('wc2.1_30s_bio_12.tif')
# bio13 = raster::raster('wc2.1_30s_bio_13.tif')
# bio14 = raster::raster('wc2.1_30s_bio_14.tif')
# bio15 = raster::raster('wc2.1_30s_bio_15.tif')
# bio16 = raster::raster('wc2.1_30s_bio_16.tif')
# bio17 = raster::raster('wc2.1_30s_bio_17.tif')
# bio18 = raster::raster('wc2.1_30s_bio_18.tif')
# bio19 = raster::raster('wc2.1_30s_bio_19.tif')
# 
# 
# data_1980s = WG_df_metadata %>% 
#   filter(year %in% c('1983', 
#                      '1984')) %>% 
#   na.omit()
# 
# latlong_generator = function(data){
#   LatLong = data_1980s %>% 
#     dplyr::select(Long, 
#                   Lat) %>% 
#     # arrange(Lat) %>%
#     as.data.frame() 
#   
#   Lat = as.numeric(LatLong$Lat)
#   Long = as.numeric(LatLong$Long)
#   
#   LatLong = bind_cols(Long, Lat) %>% 
#     rename(Long = ...1, 
#            Lat = ...2) %>% 
#     na.omit()
# }
# 
# coords = latlong_generator(data_1980s)
# 
# bio1_data = raster::extract(bio1, coords) %>% 
#   as_tibble() %>% 
#   rename(bio1 = value) 
# bio2_data = raster::extract(bio2, coords) %>% 
#   as_tibble() %>% 
#   rename(bio2 = value) 
# bio3_data = raster::extract(bio3, coords) %>% 
#   as_tibble() %>% 
#   rename(bio3 = value) 
# bio4_data = raster::extract(bio4, coords) %>% 
#   as_tibble() %>% 
#   rename(bio4 = value) 
# bio5_data = raster::extract(bio5, coords) %>% 
#   as_tibble() %>% 
#   rename(bio5 = value) 
# bio6_data = raster::extract(bio6, coords) %>% 
#   as_tibble() %>% 
#   rename(bio6 = value) 
# bio7_data = raster::extract(bio7, coords) %>% 
#   as_tibble() %>% 
#   rename(bio7 = value) 
# bio8_data = raster::extract(bio8, coords) %>% 
#   as_tibble() %>% 
#   rename(bio8 = value) 
# bio9_data = raster::extract(bio9, coords) %>% 
#   as_tibble() %>% 
#   rename(bio9 = value) 
# bio10_data = raster::extract(bio10, coords) %>% 
#   as_tibble() %>% 
#   rename(bio10 = value)
# bio11_data = raster::extract(bio11, coords) %>% 
#   as_tibble() %>% 
#   rename(bio11 = value)
# bio12_data = raster::extract(bio12, coords) %>% 
#   as_tibble() %>% 
#   rename(bio12 = value)
# bio13_data = raster::extract(bio13, coords) %>% 
#   as_tibble() %>% 
#   rename(bio13 = value)
# bio14_data = raster::extract(bio14, coords) %>% 
#   as_tibble() %>% 
#   rename(bio14 = value)
# bio15_data = raster::extract(bio15, coords) %>% 
#   as_tibble() %>% 
#   rename(bio15 = value)
# bio16_data = raster::extract(bio16, coords) %>% 
#   as_tibble() %>% 
#   rename(bio16 = value)
# bio17_data = raster::extract(bio17, coords) %>% 
#   as_tibble() %>% 
#   rename(bio17 = value)
# bio18_data = raster::extract(bio18, coords) %>% 
#   as_tibble() %>% 
#   rename(bio18 = value)
# bio19_data = raster::extract(bio19, coords) %>% 
#   as_tibble() %>% 
#   rename(bio19 = value)
# 
# bioclim_data = bind_cols(bio1_data, 
#                          bio2_data, 
#                          bio3_data, 
#                          bio4_data, 
#                          bio5_data, 
#                          bio6_data, 
#                          bio7_data, 
#                          bio8_data, 
#                          bio9_data, 
#                          bio10_data, 
#                          bio11_data, 
#                          bio12_data, 
#                          bio13_data, 
#                          bio14_data, 
#                          bio15_data,
#                          bio16_data, 
#                          bio17_data, 
#                          bio18_data, 
#                          bio19_data)
# 
# bioclim_data = bind_cols(data_1980s, 
#           bio1_80s_extract) 

# magnetic field data -----------------------------------------------------



# Linear models per year ------------------------------------------



# Linear mixed effect model(s) --------------------------------------------



