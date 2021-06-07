#### rf_model_fit ####
# load-packages -----------------------------------------------------------
library(tidymodels)
library(tidyverse)


# load-objects -----------------------------------------------------------
oscar_recipe <- readRDS("model-info/oscar_recipe.rds")
oscar_train <- readRDS("data/processed/oscar_train.rds")
oscar_test <- readRDS("data/processed/oscar_test.rds")
oscar_folds <- readRDS("data/processed/oscar_folds.rds")
rf_tuned <- read_rds("model-info/tuned/rf_tuned_best.rds")

set.seed(101)


# define-model ------------------------------------------------------------
rf_model <- rand_forest(mode = "classification",
                        mtry = tune(),
                        min_n = tune()) %>%
  set_engine("ranger")


# define-tuning-grid ------------------------------------------------------
rf_params <- parameters(rf_model) %>%
  update(mtry = mtry(c(1, 10)))

rf_grid <- grid_regular(rf_params, levels = 5)


# workflow ----------------------------------------------------------------
rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(oscar_recipe)


# tuned-workflow ----------------------------------------------------------------
rf_workflow_tuned <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tuned, metric = "accuracy"))


# fit ---------------------------------------------------------------------
rf_fit_results <- fit(rf_workflow_tuned, oscar_train)
rf_fit_results
write_rds(rf_fit_results, "model-info/rf_best_model_training_fit.rds")


# test-set-performance ----------------------------------------------------
rf_fit_results %>% 
  predict(oscar_test) %>% 
  bind_cols(truth = oscar_test$Oscar_nominated) %>% 
  accuracy(truth = truth, estimate = .pred_class)


# ROC-AUC -----------------------------------------------------------------
rf_fit_results %>% 
  predict(new_data = oscar_test, type = "prob") %>% 
  bind_cols(truth = oscar_test$Oscar_nominated) %>% 
  roc_curve(truth = truth, .pred_0) %>% 
  autoplot()

rf_accuracy <- rf_fit_results %>% 
  predict(oscar_test) %>% 
  bind_cols(truth = oscar_test$Oscar_nominated) %>% 
  accuracy(truth = truth, estimate = .pred_class) 

rf_fit_results %>% 
  predict(new_data = oscar_test, type = "prob") %>% 
  bind_cols(truth = oscar_test$Oscar_nominated) %>% 
  roc_auc(truth = truth, .pred_0) %>% # roc-auc
  bind_rows(rf_accuracy) %>% # elastic net accuracy (stored)
  mutate(
    Metric = .metric, # rename
    Estimate = .estimate
  ) %>% 
  select(Metric, Estimate) # select



