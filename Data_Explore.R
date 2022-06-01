##################################################################
## Salmon migration data exploration
##
## Matt Brachmann (PhDMattyB)
##
## 2022-06-01
##
##################################################################

## Load the data manipulation work horse
library(tidyverse)


setwd('~/Salmond_Migration_Paper/')

Sal_80_90 = read_csv('WG1980_1990_indivAssign_byRegion.csv') %>% 
  dplyr::select(-repunit.y, 
         -mixture_collection.y) %>% 
  separate(col = 'mixture_collection.x', 
           into = c('year', 
                    'mixture_collection'), 
           sep = '_') %>% 
  arrange(year) %>% 
  dplyr::select(-ID) %>% 
  rename(repunit = repunit.x)

dim(Sal_80_90)


Sal_2017 = read_csv('2017_2018_WG_individualAssignments5reps.csv') %>%
  dplyr::select(indiv.1, 
                mixture_collection.1, 
                repunit.1, 
                rep_pofz.1, 
                agreement, 
                # lowZ, 
                mean_pofz) %>% 
  filter(agreement == 'yes') %>% 
  dplyr::select(-agreement) %>% 
  rename(indiv = indiv.1, 
         mixture_collection = mixture_collection.1, 
         repunit = repunit.1, 
         rep_pofz = rep_pofz.1) %>% 
  separate(col = 'mixture_collection', 
           into = c('year', 
                    'mixture_collection'), 
           sep = '_') %>% 
  arrange(year) %>% 
  rename(PofZ = mean_pofz) %>%
  mutate(collection = NA) %>% 
  dplyr::select(indiv, 
                rep_pofz, 
                year, 
                mixture_collection, 
                repunit, 
                collection,
                PofZ)

dim(Sal_2017)


Sal_2019 = read_csv('WG2019_indivAssign_byRegion.csv') %>% 
  dplyr::select(-repunit.y, 
                -mixture_collection.y) %>% 
  separate(col = 'mixture_collection.x', 
           into = c('year', 
                    'mixture_collection'), 
           sep = '_') %>% 
  arrange(year) %>% 
  dplyr::select(-...1) %>% 
  rename(repunit = repunit.x)

dim(Sal_2019)


Sal_2020 = read_csv('WG2020_indivAssign_byRegion.csv')%>% 
  dplyr::select(-repunit.y, 
                -mixture_collection.y) %>% 
  separate(col = 'mixture_collection.x', 
           into = c('year', 
                    'mixture_collection'), 
           sep = '_') %>% 
  arrange(year) %>% 
  dplyr::select(-...1) %>% 
  rename(repunit = repunit.x)


Sal_2021 = read_csv('WG2021_indivAssign_byRegion.csv')%>% 
  dplyr::select(-repunit.y, 
                -mixture_collection.y) %>% 
  separate(col = 'mixture_collection.x', 
           into = c('year', 
                    'mixture_collection'), 
           sep = '_') %>% 
  arrange(year) %>% 
  dplyr::select(-...1) %>% 
  rename(repunit = repunit.x)


clean_data = full_join(Sal_80_90, 
           Sal_2019)

clean_data = full_join(clean_data, 
                       Sal_2020)

clean_data = full_join(clean_data, 
                       Sal_2021)


final_data = bind_rows(clean_data, 
                       Sal_2017) 

final_data %>% 
  write_csv('WG_clean_dataframe.csv')
