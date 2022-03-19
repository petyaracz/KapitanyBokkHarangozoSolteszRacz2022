################################################
# vis
################################################

set.seed(1337)

# -- header -- #

setwd('~/Github/KapitanyBokkHarangozoSolteszRacz2022/')

library(tidyverse)
library(magrittr)
library(glue)
library(ggthemes) # for some ggplot themes
library(gghalves) # for half geoms
library(patchwork) # <3

# -- read-in -- #

d = read_tsv('data/data_tidy.tsv') # see data_setup

d %<>%
  mutate(
    last_use_type = fct_reorder(last_use_type, Last_use),
    frequency_type = fct_reorder(frequency_type, Frequency),
    sipd_cannabis = substance_most_frequent_type == 'cannabis' & frequency_type %in% c('once a week', '1+ a week')
  )

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

med_vars = c(
  'substance_lifetime_type',
  'substance_most_frequent_type',
  'last_use_type',
  'frequency_type',
  'medication_type' 
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
  select(group_type,medication_type,substance_most_frequent_type,sipd_cannabis,all_of(text_vars)) %>% 
  rename(
    'Syntactic simplicity' = Syntac_simp,
    'Word concreteness' = Word_conc,
    'Referential cohesion' = Ref_coh,
    'Lexical density' = Lexical_dens,
    'Ratio of unique content words' = Unique_con, 
    'Ratio of determiners' = Determiners,
    'Ratio of demonstratives' = Dem_att,
    'Ratio of possessive pronouns' = Poss_2,
    'Syntactic complexity' = Synt_comp2,
    'Emotional sensitivity' = Emot_sens
  )

# med variables only
dcm = d2 %>% 
  select(group_type,all_of(med_vars))

# --- vis --- #

### medical variables

dcm %>% 
  count(group_type,substance_lifetime_type) %>% 
  pivot_wider(names_from = substance_lifetime_type, values_from = n, values_fill = 0)

mtables = dcm %>% 
  pivot_longer(-group_type) %>% 
  group_by(name) %>% 
  nest() %>%
  mutate(
    table = map(data, ~ 
                  count(.,group_type,value) %>% 
                  pivot_wider(names_from = value, values_from = n, values_fill = 0)
                )
  )

mtables$table %>% knitr::kable()

### Dep. Outcomes

## correlation matrix of predictors:
dco %>% 
  select(all_of(outcome_vars)) %>% 
  cor() %>% 
  corrplot::corrplot(method = 'number')

## pca of predictors
dco_pca = dco %>% 
  select(all_of(outcome_vars)) %>% 
  prcomp()
  
p1 = factoextra::fviz_eig(dco_pca)
p2 = factoextra::fviz_pca_var(dco_pca)
p1 + p2
ggsave('vis/depression_pca.pdf', width = 12, height = 6)

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
  select(-group_type,-medication_type,-substance_most_frequent_type,-sipd_cannabis) %>% 
  cor() %>% 
  corrplot::corrplot(method = 'number')

## pca of predictors
dct_pca = dct %>% 
  select(-group_type,-medication_type,-substance_most_frequent_type,-sipd_cannabis) %>% 
  prcomp()

p3 = factoextra::fviz_eig(dct_pca)
p4 = factoextra::fviz_pca_var(dct_pca)
p3 + p4
ggsave('vis/text_pca.pdf', width = 12, height = 6)

## ~ per group:
cors = dct %>% 
  group_by(group_type,medication_type,substance_most_frequent_type) %>% 
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
  pivot_longer(- c(group_type,medication_type,substance_most_frequent_type,sipd_cannabis)) %>% 
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
  facet_wrap( ~ name, ncol = 3) +
  ylab('scaled value') +
  xlab('group type')
ggsave('vis/text_var_groups.pdf', width = 12, height = 12)

### Medication and substance use

## substance freq
d %>% 
  filter(substance_most_frequent_type %in% c('cannabis','alcohol')) %>% 
  select(group_type,substance_most_frequent_type,frequency_type) %>% 
  ggplot(aes(frequency_type)) +
  geom_bar() +
  facet_wrap( ~ substance_most_frequent_type + group_type, ncol = 3) +
  scale_y_continuous(breaks = seq(1,8,2)) +
  theme_few() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank())

ggsave('vis/substance_freq.pdf', width = 6, height = 6)

d %>% 
  filter(group_type == 'SIPD') %>% 
  count(substance_most_frequent_type,frequency_type)

## predictors across cannabis in sipd
dct %>% 
  filter(group_type == 'SIPD') %>% 
  pivot_longer(- c(group_type,medication_type,substance_most_frequent_type,sipd_cannabis)) %>%
  mutate(sipd_cannabis2 = ifelse(sipd_cannabis, 'infrequent/\nno use', 'frequent use')) %>% 
  ggplot(aes(sipd_cannabis2, value)) +
  # geom_tufteboxplot() +
  geom_half_violin() +
  geom_half_boxplot(width = .1) +
  geom_half_dotplot() +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  facet_wrap( ~ name, ncol = 3) +
  ylab('scaled value') +
  xlab('cannabis use') +
  ggtitle('Text variables across cannabis use in SIPD group')

ggsave('vis/text_var_sipd.pdf', width = 12, height = 12)

## medication use
d %>% 
  mutate(medication_type2 = ifelse(is.na(medication_type), 'none', medication_type) %>% 
           fct_relevel('antipsychotics','benzodiazepines','both','other','none')) %>% 
  ggplot(aes(medication_type2)) +
  geom_bar() +
  facet_wrap( ~ group_type, ncol = 3) +
  scale_y_continuous(breaks = seq(1,8,2)) +
  theme_few() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank())

ggsave('vis/medication_freq.pdf', width = 6, height = 6)

## sipd: medication and substance use

d %>% 
  filter(group_type == 'SIPD') %>% 
  count(medication_type,substance_most_frequent_type) %>% 
  pivot_wider(names_from = substance_most_frequent_type, values_from =  n, values_fill = 0) %>% knitr::kable(format = 'simple')

## predictors across medication in sipd
dct %>% 
  filter(group_type == 'SIPD', medication_type %in% c('antipsychotics','benzodiazepines')) %>% 
  pivot_longer(- c(group_type,medication_type,substance_most_frequent_type)) %>% 
  ggplot(aes(medication_type, value)) +
  # geom_tufteboxplot() +
  geom_half_violin() +
  geom_half_boxplot(width = .1) +
  geom_half_dotplot() +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  facet_wrap( ~ name, ncol = 3) +
  ylab('scaled value') +
  xlab('medication type') +
  ggtitle('Text variables across medication type in SIPD group')

ggsave('vis/text_var_sipd2.pdf', width = 12, height = 12)

