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
  arrange(year)

dim(Sal_2017)
