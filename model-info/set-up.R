### set-up
library(tidyverse)
library(tidymodels)
library(ranger)
library(tictoc)

set.seed(101)

oscars_data <- read_csv("data/Oscars-demographics-DFE.csv")
potential_data <- read_csv("data/BigML_Dataset_5f50a62c2fb31c516d000176.csv")

# Oscar nominated: 0 is No nomination and 1 is one or more nominations
potential_data$Oscar_nominated <- ifelse(potential_data$Oscar_nominated == 0, 0, 
                                         ifelse(potential_data$Oscar_nominated != 0, 1, NA))

potential_data <- potential_data %>%
  mutate(
    Oscar_nominated = factor(Oscar_nominated)
  )

oscar_split <- initial_split(potential_data, prop = 0.7, strata = Oscar_nominated)

oscar_train <- training(oscar_split) 
oscar_test <- testing(oscar_split)

oscar_folds <- vfold_cv(potential_data, v = 5, repeats = 3, strata = Oscar_nominated)
skimr::skim_without_charts(oscar_train)

oscar_recipe <- recipe(Oscar_nominated ~ duration + votes + rate + metascore + gross 
                       + awards_wins + awards_nominations + Golden_Globes_nominated +
                         Critics_Choice_nominated + BAFTA_nominated, data = oscar_train) %>%
  step_impute_bag(metascore, gross) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  step_normalize(all_numeric()) 

prep(oscar_recipe) %>%
  bake(new_data = NULL)

write_rds(oscar_recipe, "model-info/oscar_recipe.rds")
