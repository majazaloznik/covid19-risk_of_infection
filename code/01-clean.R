###########################################################
# load libraries
###########################################################
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)

###########################################################
## import data
###########################################################

# population sizes from SUR
download.file("https://pxweb.stat.si:443/SiStatData/sq/958", 
 "data/2020-slo_population_age_by_region.csv")

pop <- read_csv("data/2020-slo_population_age_by_region.csv", 
                locale = locale(encoding = "Windows-1250"),
                skip = 1)

# median infections model data from Žiga
download.file(paste0("https://fiz.fmf.uni-lj.si/~zaplotnikz/",
"korona/2020_11_07/slo_pandemic_model_output_2020_11_07.csv"),
"data/slo_pandemic_model_output_2020_11_07.csv")

n.inf <- read_csv("data/slo_pandemic_model_output_2020_11_07.csv",
                  col_types = 
                    cols_only('date' = col_date("%Y-%m-%d"), 
                              'infectious_median' = col_double())) 

# sledilnik confirmed cases data
download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/stats.csv"),
                     "data/stats.csv")

confirmed.cases <- read_csv("data/stats.csv")

###########################################################
## clean upa
###########################################################
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
  select(-colname) %>% 
  spread( key = sets, value = value) %>% 
  rowwise() %>% 
  mutate(value = sum( `1`, `2`,`3`,`4`, na.rm = TRUE))  %>% 
  select(region, group, value) %>% 
  spread(key = group, value = value) %>% 
  relocate(`5-14`, .after = `0-4`) -> pop
  

# clean, decumulate, and join
confirmed.cases %>% 
  select(date, starts_with("age"), -contains("male"), -contains("female")) %>% 
  mutate_if(is.numeric, function(x) c(x[1], diff(x))) %>% 
  rename_if(is.numeric, ~(sub("[.]todate$", "", .))) %>% 
  full_join(n.inf) %>% 
  rename("infect.med" = "infectious_median") -> data
