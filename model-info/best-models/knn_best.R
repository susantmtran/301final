library(tidyverse)
library(tidymodels)
library(ranger)
library(tictoc)
library(kknn)



oscar_recipe <- readRDS("model-info/oscar_recipe.rds")
oscar_train <- readRDS("data/processed/oscar_train.rds")
oscar_folds <- readRDS("data/processed/oscar_folds.rds")

set.seed(101)

# model

nn_model <- nearest_neighbor(mode = "classification", 
                             neighbors = tune()) %>%
  set_engine("kknn")

nn_params <- parameters(nn_model) 
nn_grid <- grid_regular(nn_params, levels = 5)

nn_workflow <- workflow() %>%
  add_model(nn_model) %>%
  add_recipe(oscar_recipe)

tic("NN Model")

nn_tuned <- nn_workflow %>%
  tune_grid(oscar_folds, grid = nn_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
nn_model_time <- tic.log(format = TRUE)

# Write out results & workflow
write_rds(nn_tuned, "model-info/tuned/nn_tuned_best.rds")