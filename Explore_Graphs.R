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
  group_by(year,
           mixture_collection,
           origin_id) %>% 
  summarise(n = n())%>%
  mutate(freq = n / sum(n)) %>% 
  View()
