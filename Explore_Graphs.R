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


# Map ---------------------------------------------------------------------


# Proportion origin --------------------------------------------------------

WG_df %>% 
  group_by(mixture_collection, 
           origin_id) %>% 
  summarise(n = n())%>%
  mutate(freq = n / sum(n)) 

# Change in proportion per time  ------------------------------------------

## need a case_when function to group the 80's and 90's years into
## two distinct groups... the 80's and 90's

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

WG_df_year %>% 
  group_by(year_fixed,
           WG_fishery,
           # mixture_collection,
           origin_id) %>% 
  summarise(n = n())%>%
  mutate(freq = n / sum(n)) %>% 
  View()
