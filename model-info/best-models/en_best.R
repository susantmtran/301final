library(tidyverse)
library(tidymodels)
library(ranger)
library(tictoc)


oscar_recipe <- readRDS("model-info/oscar_recipe.rds")
oscar_train <- readRDS("data/processed/oscar_train.rds")
oscar_folds <- readRDS("data/processed/oscar_folds.rds")

set.seed(101)

# log forest
en_model <- logistic_reg(mixture = tune(),
                         penalty = tune()) %>%
  set_engine("glmnet")

en_params <- parameters(en_model)

en_grid <- grid_regular(en_params, levels = 5)

en_workflow <- workflow() %>%
  add_model(en_model) %>%
  add_recipe(oscar_recipe)

tic("EN Model")

en_tuned <- en_workflow %>% 
  tune_grid(oscar_folds, grid = en_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
en_model_time <- tic.log(format = TRUE)

# Write out results & workflow
write_rds(en_tuned, "model-info/tuned/en_tuned_best.rds")



