library(tidyverse)
library(tidymodels)
library(ranger)
library(tictoc)

oscar_recipe <- readRDS("model-info/oscar_recipe.rds")
oscar_train <- readRDS("data/processed/oscar_train.rds")
oscar_folds <- readRDS("data/processed/oscar_folds.rds")

set.seed(101)

# random forest
mlp_model <- mlp(mode = "classification",
                 hidden_units = tune(),
                 penalty = tune()) %>%
  set_engine("nnet")

mlp_params <- parameters(mlp_model)

mlp_grid <- grid_regular(mlp_params, levels = 5)

mlp_workflow <- workflow() %>%
  add_model(mlp_model) %>%
  add_recipe(oscar_recipe)

tic("MLP Model")

mlp_tuned <- mlp_workflow %>% 
  tune_grid(oscar_folds, grid = mlp_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
mlp_model_time <- tic.log(format = TRUE)

# Write out results & workflow
write_rds(mlp_tuned, "model-info/tuned/mlp_tuned_best.rds")