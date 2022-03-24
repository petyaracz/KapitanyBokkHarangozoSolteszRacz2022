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

# d = read_csv('data/psychosis_linguistic_IDs.csv') # data
d = read_csv('data/psychosis_linguistic_revised4.csv')
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
    Woman = Gender == 2,
    substance_lifetime_type = case_when(
      Substance == 0 ~ 'nothing',
      Substance == 1 ~ 'alcohol',
      Substance == 5 ~ 'multiple'
    ),
    substance_most_frequent_type = case_when(
      Substance_n1 == 0 ~ 'nothing',
      Substance_n1 == 1 ~ 'alcohol',
      Substance_n1 == 2 ~ 'cannabis',
      Substance_n1 == 3 ~ 'cocaine',
      Substance_n1 == 4 ~ 'mdma',
      Substance_n1 == 5 ~ 'multiple'
    ),
    last_use_type = case_when(
      Last_use == 0 ~ 'never',
      Last_use == 1 ~ '12+ months',
      Last_use == 2 ~ '1+ month',
      Last_use == 3 ~ '1week-1month',
      Last_use == 4 ~ 'within week'
    ) %>% fct_reorder(Last_use),
    frequency_type = case_when(
      Frequency == 0 ~ 'never',
      Frequency == 1 ~ 'not in 12 months',
      Frequency == 2 ~ 'few occasions a year',
      Frequency == 3 ~ 'once a month',
      Frequency == 4 ~ '1+ a month',
      Frequency == 5 ~ 'once a week',
      Frequency == 6 ~ '1+ a week',
      Frequency == 7 ~ 'daily'
    ) %>% fct_reorder(Frequency),
    medication_type = case_when(
      Med_type == 1 ~ 'benzodiazepines',
      Med_type == 2 ~ 'antipsychotics',
      Med_type == 3 ~ 'both',
      Med_type == 4 ~ 'other'
    )
  ) %>% 
  select(-Azonosito,-Gender)
  
d = left_join(id,d)

# -- checks -- #

distinct(d,sample_type,treatment_type,group_type,schizophrenia,SIPD)
count(d,sample_type,treatment_type,group_type,schizophrenia,SIPD)

# -- write-out -- #

write_tsv(d, 'data/data_tidy.tsv')
