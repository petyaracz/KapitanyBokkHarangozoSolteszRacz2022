################################################
# gbm
################################################

set.seed(1337)

# -- header -- #

setwd('~/Github/KapitanyBokkHarangozoSolteszRacz2022/')

library(tidyverse)
library(magrittr)
library(glue)
library(ggthemes)
library(patchwork)
library(h2o) # to use h2o
# don't panic:
# https://docs.h2o.ai/h2o/latest-stable/h2o-docs/index.html

# -- read-in -- #

d = read_tsv('data/data_tidy.tsv')

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

# -- centering -- #

# centering numeric variables
d2 = d %>% 
  filter(id != 'ri20re') %>% 
  select(id, sample_type, treatment_type, group_type, all_of(text_vars)) %>% 
  mutate(
    across(where(is.double), ~ scale(., center = T, scale = T) %>% 
             as.double())
    ) %>% 
  sample_n(n())

# -- subsetting -- #

# schizophrenia group vs matched controls, sipd group vs matched controls, no controls
schizo = filter(d2, sample_type == 'schizophrenia') %>% 
  select(-sample_type,-treatment_type,-id)
sipd = filter(d2, sample_type == 'SIPD') %>% 
  select(-sample_type,-treatment_type,-id)
treatment = filter(d2, treatment_type == 'test') %>% 
  select(-sample_type,-treatment_type,-id)

# -- glm -- #

# spin up h2o
h2o.init(nthreads=4)

## setup

# load datato h2o
dat1 = as.h2o(schizo)
dat2 = as.h2o(sipd)
dat3 = as.h2o(treatment)

# h2o wants the outcome as a factor
dat1$group_type = as.factor(dat1$group_type)
dat2$group_type = as.factor(dat2$group_type)
dat3$group_type = as.factor(dat3$group_type)

# defining x and y
response = 'group_type'
predictors = text_vars

## comparison random forests

# how well do bagging methods do? let's fit random forests on all three datasets
rf1 = h2o.randomForest(
  x = predictors, 
  y = response,
  training_frame = dat1,
  nfolds = 4,
  ntrees = 200,
  stopping_rounds = 2,
  score_each_iteration = T
) # parameters were made up

rf2 = h2o.randomForest(
  x = predictors, 
  y = response,
  training_frame = dat2,
  nfolds = 4,
  ntrees = 200,
  stopping_rounds = 2,
  score_each_iteration = T
)

rf3 = h2o.randomForest(
  x = predictors, 
  y = response,
  training_frame = dat3,
  nfolds = 4,
  ntrees = 200,
  stopping_rounds = 2,
  score_each_iteration = T
)

# check accuracy
h2o.auc(h2o.performance(rf1, xval = TRUE)) # .86
h2o.auc(h2o.performance(rf2, xval = TRUE)) # .73
h2o.auc(h2o.performance(rf3, xval = TRUE)) # .53

# save model
h2o.saveModel(rf1, 'models')
h2o.saveModel(rf2, 'models')
h2o.saveModel(rf3, 'models')

## hyperparameters

# we pick four hyperparameters and 4-5 values. 
# largely following links under
# https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/gbm.html
# https://docs.h2o.ai/h2o/latest-stable/h2o-docs/grid-search.html

# we define hyperparameters in the grid:
gbm_params1 = list(
  learn_rate = c(0.01,0.03,0.05,0.1),
  max_depth = c(1,3,5,7,9),
  sample_rate = c(0.2,0.4,0.6,0.8,1),
  col_sample_rate = c(0.2,0.4,0.6,0.8,1)
  )

# we do a grid search for each model: 1-2-3
gbm_grid1 = h2o.grid("gbm", 
                      x = predictors, 
                      y = response,
                      grid_id = "gbm_grid1",
                      training_frame = dat1,
                      nfolds = 4,
                      ntrees = 100,
                      seed = 1,
                      hyper_params = gbm_params1
                     )

gbm_grid2 = h2o.grid("gbm", 
                     x = predictors, 
                     y = response,
                     grid_id = "gbm_grid2",
                     training_frame = dat2,
                     nfolds = 4,
                     ntrees = 100,
                     seed = 1,
                     hyper_params = gbm_params1
)

gbm_grid3 = h2o.grid("gbm", 
                     x = predictors, 
                     y = response,
                     grid_id = "gbm_grid3",
                     training_frame = dat3,
                     nfolds = 4,
                     ntrees = 100,
                     seed = 1,
                     hyper_params = gbm_params1
)

# pick the best model from each grid
gbm_gridperf1 = h2o.getGrid(grid_id = "gbm_grid1",
                            sort_by = "auc",
                            decreasing = TRUE)
gbm_gridperf2 = h2o.getGrid(grid_id = "gbm_grid2",
                            sort_by = "auc",
                            decreasing = TRUE)
gbm_gridperf3 = h2o.getGrid(grid_id = "gbm_grid3",
                            sort_by = "auc",
                            decreasing = TRUE)

## best settings

# col_sample_rate learn_rate max_depth sample_rate           model_ids     auc
# 1         0.60000    0.10000   1.00000     0.40000 gbm_grid1_model_235 0.84375
# 1         0.20000    0.05000   1.00000     1.00000 gbm_grid2_model_492 0.85547
# 1         0.20000    0.10000   3.00000     0.20000 gbm_grid3_model_117 0.60417

# note: best models either about match even w/ ranfom forest fit on same data or improve on it

fit1 = h2o.getModel(gbm_gridperf1@model_ids[[1]])
fit2 = h2o.getModel(gbm_gridperf2@model_ids[[1]])
fit3 = h2o.getModel(gbm_gridperf3@model_ids[[1]])

# save models
h2o.saveModel(fit1, 'models')
h2o.saveModel(fit2, 'models')
h2o.saveModel(fit3, 'models')

# check performance (again)
h2o.auc(h2o.performance(fit1, xval = TRUE)) # for cv
h2o.auc(h2o.performance(fit2, xval = TRUE)) # for cv
h2o.auc(h2o.performance(fit3, xval = TRUE)) # for cv

# check variable importance
h2o.varimp(fit1)
h2o.varimp(fit2)
h2o.varimp(fit3)

########################################################################

path1 = '~/Github/KapitanyBokkHarangozoSolteszRacz2022/models/gbm_grid1_model_235'
path2 = '~/Github/KapitanyBokkHarangozoSolteszRacz2022/models/gbm_grid2_model_492'
path3 = '~/Github/KapitanyBokkHarangozoSolteszRacz2022/models/gbm_grid3_model_117'

fit1 = h2o.loadModel(normalizePath(path1))
fit2 = h2o.loadModel(normalizePath(path2))
fit3 = h2o.loadModel(normalizePath(path3))

# -- vis -- #

# build three plots of variable importance in % for the three models, with 
# variables ordered across importance
p1 = h2o.varimp(fit1) %>% 
  as_tibble() %>% 
  mutate(
    variable2 = case_when(
      variable == 'Narrativity' ~ 'Narrativity',
      variable == 'Syntac_simp' ~ 'Syntactic simplicity',
      variable == 'Word_conc' ~ 'Word concreteness',
      variable == 'Ref_coh' ~ 'Referential cohesion',
      variable == 'Lexical_dens' ~ 'Lexical density',
      variable == 'Unique_con' ~ 'Ratio of unique content words',
      variable == 'Determiners' ~ 'Ratio of determiners',
      variable == 'Dem_att' ~ 'Ratio of demonstratives',
      variable == 'Poss_2' ~ 'Ratio of possessive pronouns',
      variable == 'Synt_comp2' ~ 'Syntactic complexity',
      variable == 'Emot_sens' ~ 'Emotional sensitivity'
    ) %>% fct_reorder(percentage)
  ) %>% 
  ggplot(aes(variable2,percentage*100)) +
  geom_col() +
  theme_few() +
  xlab('text variable') +
  ylab('variable percentage') +
  ylim(0,60) +
  coord_flip() +
  ggtitle('Control / Schizophrenia ~\ntext variables')

p2 = h2o.varimp(fit2) %>% 
  as_tibble() %>% 
  mutate(
    variable2 = case_when(
      variable == 'Narrativity' ~ 'Narrativity',
      variable == 'Syntac_simp' ~ 'Syntactic simplicity',
      variable == 'Word_conc' ~ 'Word concreteness',
      variable == 'Ref_coh' ~ 'Referential cohesion',
      variable == 'Lexical_dens' ~ 'Lexical density',
      variable == 'Unique_con' ~ 'Ratio of unique content words',
      variable == 'Determiners' ~ 'Ratio of determiners',
      variable == 'Dem_att' ~ 'Ratio of demonstratives',
      variable == 'Poss_2' ~ 'Ratio of possessive pronouns',
      variable == 'Synt_comp2' ~ 'Syntactic complexity',
      variable == 'Emot_sens' ~ 'Emotional sensitivity'
    ) %>% fct_reorder(percentage)
  ) %>% 
  ggplot(aes(variable2,percentage*100)) +
  geom_col() +
  theme_few() +
  xlab('') +
  ylab('variable percentage') +
  ylim(0,60) +
  coord_flip() +
  ggtitle('Control / SIPD ~\ntext variables')

p3 = h2o.varimp(fit3) %>% 
  as_tibble() %>% 
  mutate(
    variable2 = case_when(
      variable == 'Narrativity' ~ 'Narrativity',
      variable == 'Syntac_simp' ~ 'Syntactic simplicity',
      variable == 'Word_conc' ~ 'Word concreteness',
      variable == 'Ref_coh' ~ 'Referential cohesion',
      variable == 'Lexical_dens' ~ 'Lexical density',
      variable == 'Unique_con' ~ 'Ratio of unique content words',
      variable == 'Determiners' ~ 'Ratio of determiners',
      variable == 'Dem_att' ~ 'Ratio of demonstratives',
      variable == 'Poss_2' ~ 'Ratio of possessive pronouns',
      variable == 'Synt_comp2' ~ 'Syntactic complexity',
      variable == 'Emot_sens' ~ 'Emotional sensitivity'
    ) %>% fct_reorder(percentage)
  ) %>% 
  ggplot(aes(variable2,percentage*100)) +
  geom_col() +
  theme_few() +
  xlab('') +
  ylab('variable percentage') +
  ylim(0,60) +
  coord_flip() +
  ggtitle('SIPD / Schizophrenia ~\ntext variables')

# build a largely superfluous plot that shows the auc-s as three columns
best_model_auc = 
  tibble(
    model = c('Control / Schizophrenia', 'Control / SIPD', 'SIPD / Schizophrenia'),
    auc = c(0.84375,0.8554688,0.641667)
  )

p4 = best_model_auc %>% 
  ggplot(aes(model,auc)) +
  geom_col() +
  theme_few() +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1), name = 'area under the curve') 
  

# patchwork the four plots together
( p1 + p2 + p3 ) / ( p4 + plot_spacer() + plot_spacer())
