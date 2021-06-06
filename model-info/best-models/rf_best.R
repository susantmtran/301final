library(tidyverse)
library(tidymodels)
library(ranger)
library(tictoc)



oscar_recipe <- readRDS("model-info/oscar_recipe.rds")
oscar_train <- readRDS("data/processed/oscar_train.rds")
oscar_folds <- readRDS("data/processed/oscar_folds.rds")

set.seed(101)

# random forest
rf_model <- rand_forest(mode = "classification",
                        mtry = tune(),
                        min_n = tune()) %>%
  set_engine("ranger")

rf_params <- parameters(rf_model) %>%
  update(mtry = mtry(c(1, 10)))

rf_grid <- grid_regular(rf_params, levels = 5)

rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(oscar_recipe)

tic("RF Model")

rf_tuned <- rf_workflow %>% 
  tune_grid(oscar_folds, grid = rf_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
rf_model_time <- tic.log(format = TRUE)

# Write out results & workflow
write_rds(rf_tuned, "model-info/tuned/rf_tuned_best.rds")
