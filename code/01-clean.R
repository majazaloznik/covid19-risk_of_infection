# load libraries
library(readr)
library(dplyr)
library(tidyr)

# import data
download.file("https://pxweb.stat.si:443/SiStatData/sq/958", 
"data/2020-slo_population_age_by_region.csv")

pop <- read_csv("data/2020-slo_population_age_by_region.csv", 
                locale = locale(encoding = "latin2"),
                skip = 1)

# clean up and aggregate age groups 
pop %>% 
  select(-c(1)) %>% 
  rename("region" = !!names(.[1])) %>% 
  gather(-c(1), key=colname, value=value) %>% 
  mutate(group = c(rep("0-4", 12),
                   rep("5-14", 24),
                   rep("15-24", 24),
                   rep("25-34", 24),
                   rep("35-44", 24),
                   rep("45-54", 24),
                   rep("55-64", 24),
                   rep("65-74", 24),
                   rep("75-84", 24),
                   rep("85+", 48)),
         sets = c(rep(1, 12), rep(1:2, each = 12, 8), rep(1:4, each = 12))) %>% 
  group_by(group) %>% 
  select(-colname) %>% 
  spread( key = sets, value = value) %>% 
  rowwise() %>% 
  mutate(value = sum( `1`, `2`,`3`,`4`, na.rm = TRUE))  %>% 
  select(region, group, value) %>% 
  spread(key = group, value = value) %>% 
  relocate(`5-14`, .after = `0-4`) -> pop
  

head(pop)