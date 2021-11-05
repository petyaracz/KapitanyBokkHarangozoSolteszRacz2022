################################################
# vis
################################################

set.seed(1337)

# -- header -- #

setwd('~/Github/KapitanyRacz2022')

library(tidyverse)
library(magrittr)
library(glue)
library(ggthemes) # for some ggplot themes
library(gghalves) # for half geoms
library(patchwork) # <3

# -- read-in -- #

d = read_tsv('data/data_tidy.tsv') # see data_setup

text_vars = c(
  'Narrativity',
  'Syntac_simp',
  'Word_conc',
  'Ref_coh',
  'Lexical_dens',
  'Unique_con',
  'Determiners',
  'Dem_att',
  'Poss_2',
  'Synt_comp2',
  'Emot_sens'
)

outcome_vars = c(
  'DES_Dep',
  'DES_Abs',
  'DES_Dis',
  'OLIFE_Un',
  'OLIFE_Cog',
  'OLIFE_Int',
  'OLIFE_Imp'
)

# -- centering -- #

# scale numeric variables and then shuffle the deck
d2 = d %>% 
  mutate(
    across(where(is.double), ~ scale(., center = T, scale = T) %>% 
             as.double())
  ) %>% 
  sample_n(n())

# --- sets --- #

# diagnostic variables only
dco = d2 %>% 
  filter(!is.na(DES_Dep),!is.na(DES_Abs),!is.na(DES_Dis)) %>% 
  select(group_type,all_of(outcome_vars)) 

# text variables only
dct = d2 %>% 
  filter(id != 'ri20re') %>% # this person didn't write anything :(
  select(group_type,all_of(text_vars)) 

# --- vis --- #

### Dep. Outcomes

## correlation matrix of predictors:
dco %>% 
  select(all_of(outcome_vars)) %>% 
  cor() %>% 
  corrplot::corrplot(method = 'number')

## predictors across groups:
dco %>% 
  pivot_longer(- group_type) %>% 
  ggplot(aes(group_type, value)) +
  # geom_tufteboxplot() +
  geom_half_violin() +
  geom_half_boxplot(width = .1) +
  geom_half_dotplot() +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  facet_wrap( ~ name ) +
  ylab('scaled value') +
  xlab('group type')
ggsave('vis/des_olef_groups.pdf', width = 12, height = 6)

### Text vars

## correlation matrix of predictors:
dct %>% 
  select(all_of(text_vars)) %>% 
  cor() %>% 
  corrplot::corrplot(method = 'number')

## ~ per group:
cors = dct %>% 
  group_by(group_type) %>% 
  nest() %>% 
  mutate(
    cor = map(data, ~ cor(.)),
    corrplot = map(cor, ~ corrplot::corrplot(., method = 'number'))
  )

cors %<>% 
  mutate(
    long = map(cor, ~
                 as.data.frame(.) %>% 
                 rownames_to_column() %>% 
                 pivot_longer(-rowname) %>% 
                 rename(
                   'var1' = rowname,
                   'var2' = name,
                   'cor' = value
                 )
               ) 
  ) %>% 
  select(group_type,long) %>% 
  unnest(cols = c(long)) %>% 
  mutate(pos = cor >= 0)
  
ac = ggplot(cors, aes(group_type, cor, fill = pos)) +
  geom_col(position = 'dodge') +
  facet_wrap( ~ var1 + var2 ) +
  guides(fill = "none") +
  ylab('rate of correlation') +
  xlab('group') + 
  theme_few() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(ac, file = 'vis/all_cor.pdf', width = 12, height = 14)

## predictors across groups:
dct %>% 
  pivot_longer(- group_type) %>% 
  ggplot(aes(group_type, value)) +
  # geom_tufteboxplot() +
  geom_half_violin() +
  geom_half_boxplot(width = .1) +
  geom_half_dotplot() +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  facet_wrap( ~ name ) +
  ylab('scaled value') +
  xlab('group type')
ggsave('vis/text_var_groups.pdf', width = 12, height = 6)
