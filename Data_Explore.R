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


# Data cleaning -----------------------------------------------------------

## we have 5 csv files all with the same data and duplicated cols
## if we just create a year column they can be joined together
## tidydata for the win folks

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



# Look at the data --------------------------------------------------------

## Now we actually need to look at the data to figure
## out what's going on 
## Check out Bradbury 2016 and 2021 to figure out what 
## the columns actually mean

final_data = read_csv('WG_clean_dataframe.csv') %>% 
  dplyr::select(indiv, 
                year, 
                repunit, 
                rep_pofz, 
                mixture_collection, 
                collection, 
                PofZ, 
                everything())


final_data %>% 
  distinct(mixture_collection)

## check supplementary table 1 in Bradbury et al., 2021 paper

## collection is the river of origin - fine scale

## reporting unit (repunit) is a broader group that the rivers
## fit into.

## mixture collection is where they were located in the 
## west greeland fishery, I'm pretty sure

final_data %>% 
  # distinct(collection) %>% 
  filter(repunit == 'BRI') %>% 
  # filter(collection %in% c('KUN', 
  #                          'TOR', 
  #                          'VIN'))
  arrange(collection) %>% 
  distinct(collection)

final_data %>% 
  # group_by(year)
  distinct(year)

final_data %>% 
  group_by(year, 
           mixture_collection) %>% 
  View()


final_data %>% 
  # group_by(repunit)
  distinct(repunit)


# Merging metadata --------------------------------------------------------

final_data = read_csv('WG_clean_dataframe.csv') %>% 
  dplyr::select(indiv, 
                year, 
                repunit, 
                rep_pofz, 
                mixture_collection, 
                collection, 
                PofZ, 
                everything())

dim(final_data)
metadata1 = read_csv('WG_river_group_metadata.csv') %>% 
  rename(repunit = `Reporting Group`, 
         collection = Code)

WG_df_metadata = left_join(final_data, 
          metadata1) %>% 
  select(-`Data Source`)

dim(WG_df_metadata)

## Don't really need the metadata2 file
## the mixture collection data is all from Greenland
# metadata2 = read_csv('WG_Greenland_Fishery_metadata.csv') %>% 
#   rename(mixture_collection = Location_code)


# WG_df_metadata %>%
#   distinct(repunit) %>% 
#   arrange(repunit) %>% 
#   View()

WG_df_metadata = mutate(.data = WG_df_metadata,
              origin_id = as.factor(case_when(
                repunit == 'ANT' ~ 'North America',
                repunit == 'BPN' ~ 'North America', 
                repunit == 'BRI' ~ 'Europe', 
                repunit == 'ENS' ~ 'North America',
                repunit == 'FRN' ~ 'Europe',
                repunit == 'FTB' ~ 'North America',
                repunit == 'GAS' ~ 'North America',
                repunit == 'GL' ~ 'Europe',
                repunit == 'GUL' ~ 'North America',
                repunit == 'ICE' ~ 'Europe',
                repunit == 'LAS' ~ 'North America',
                repunit == 'LAC' ~ 'North America',
                repunit == 'MEL' ~ 'North America',
                repunit == 'NF1' ~ 'North America',
                repunit == 'NF2' ~ 'North America',
                repunit == 'NNF' ~ 'North America',
                repunit == 'QLS' ~ 'North America',
                repunit == 'QUE' ~ 'North America',
                repunit == 'SJR' ~ 'North America',
                repunit == 'SNO' ~ 'Europe',
                repunit == 'SPN' ~ 'Europe',
                repunit == 'UNG' ~ 'North America',
                repunit == 'USA' ~ 'North America',
                repunit == 'WNF' ~ 'North America')))

## We've got a finalized data set!!
WG_df_metadata %>% 
  write_csv('WG_df_metadata_cleaned.csv')

# -------------------------------------------------------------------------


