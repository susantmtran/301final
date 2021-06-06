library(tidyverse)
library(tidymodels)
library(ranger)
library(tictoc)


oscar_recipe <- readRDS("model-info/oscar_recipe.rds")
oscar_train <- readRDS("data/processed/oscar_train.rds")
oscar_folds <- readRDS("data/processed/oscar_folds.rds")

set.seed(101)

# boosted tree
bt_model <- boost_tree(mode = "classification",
                       mtry = tune(),
                       min_n = tune(),
                       learn_rate = tune()) %>%
  set_engine("xgboost")

bt_params <- parameters(bt_model) %>%
  update(mtry = mtry(c(1, 10)),
         learn_rate = learn_rate(range = c(-5, -0.2)))

bt_grid <- grid_regular(bt_params, levels = 5)

bt_workflow <- workflow() %>%
  add_model(bt_model) %>%
  add_recipe(oscar_recipe)

tic("BT Model")

bt_tuned <- bt_workflow %>% 
  tune_grid(oscar_folds, grid = bt_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
bt_model_time <- tic.log(format = TRUE)

# Write out results & workflow
write_rds(bt_tuned, "model-info/tuned/bt_tuned_best.rds")
