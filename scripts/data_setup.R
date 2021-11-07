################################################
# setup
################################################

set.seed(1337)

# -- header -- #

setwd('~/Github/KapitanyBokkHarangozoSolteszRacz2022/')

library(tidyverse) # fake sql and pipes
library(magrittr) # self pipe
library(glue) # for pythonesque string handling

# -- read-in -- #

d = read_csv('data/psychosis_linguistic_IDs.csv') # data
id = read_tsv('data/pairs.tsv') # matched id-s

# -- wrangling -- #

# make id table long
id %<>% 
  mutate(pair_id = glue('pair {1:n()}')) %>% 
  rename('sample_type' = type) %>% 
  pivot_longer(-c(sample_type,pair_id),names_to = 'treatment_type', values_to = 'id')

# build tidier columns, match up with id pair info
d %<>% 
  mutate(
    id = as.character(Azonosito),
    Group = as.character(Group),
    group_type = case_when(
      Group == 0 ~ 'control',
      Group == 1 ~ 'schizophrenia',
      Group == 2 ~ 'SIPD'
    ),
    schizophrenia = group_type == 'schizophrenia',
    SIPD = group_type == 'SIPD',
    Woman = Gender == 2
  ) %>% 
  select(-Azonosito,-Gender)
  
d = left_join(id,d)

# -- checks -- #

distinct(d,sample_type,treatment_type,group_type,schizophrenia,SIPD)
count(d,sample_type,treatment_type,group_type,schizophrenia,SIPD)

# -- write-out -- #

write_tsv(d, 'data/data_tidy.tsv')
