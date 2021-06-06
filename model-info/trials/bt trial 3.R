library(tidyverse)
library(tidymodels)
library(xgboost)
library(tictoc)

potential_data <- read_csv("data/BigML_Dataset_5f50a62c2fb31c516d000176.csv")

# Oscar nominated: 0 is No nomination and 1 is one or more nominations
potential_data$Oscar_nominated <- ifelse(potential_data$Oscar_nominated == 0, 0, 
                                         ifelse(potential_data$Oscar_nominated != 0, 1, NA))

naniar::miss_var_summary(potential_data)
potential_data <- potential_data %>% select(!ends_with("categories"))

potential_data <- potential_data %>%
  mutate(Oscar_nominated = factor(Oscar_nominated),
         certificate = factor(certificate),
         awards_ratio = awards_wins/awards_nominations)

potential_data$awards_ratio[is.nan(potential_data$awards_ratio)] <- 0

oscar_split <- initial_split(potential_data, prop = 0.7, strata = Oscar_nominated)
oscar_train <- training(oscar_split) 
oscar_test <- testing(oscar_split)
oscar_folds <- vfold_cv(potential_data, v = 5, repeats = 3, strata = Oscar_nominated)

oscar_recipe <- recipe(
  Oscar_nominated ~ certificate + duration + rate + metascore + gross + release_date.month +
  awards_wins + awards_nominations + awards_ratio + Golden_Globes_nominated +
  Critics_Choice_nominated + BAFTA_nominated + People_Choice_nominated +
  New_York_Film_Critics_Circle_nominated +
  Los_Angeles_Film_Critics_Association_nominated, data = oscar_train) %>%
        step_impute_bag(release_date.month, metascore, gross, certificate) %>%
        step_other(all_predictors(), threshold = 0.02) %>% 
        step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
        step_normalize(all_numeric())

# no more missingness!
prep(oscar_recipe) %>%
  bake(new_data = NULL)

# random forest
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
write_rds(bt_model_time, "model-info/timing/bt_model_time3.rds")
write_rds(bt_tuned, "model-info/tuned/bt_tuned3.rds")

show_best(bt_tuned, metric = "accuracy")
