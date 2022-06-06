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
# Map ---------------------------------------------------------------------



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

ggplot(data = WG_prop, 
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

