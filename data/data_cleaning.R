library(tidyverse)

potential_data <- read_csv("data/BigML_Dataset_5f50a62c2fb31c516d000176.csv")

set.seed(101)

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

# save
write_rds(potential_data, "data/processed/cleaned_data_set.rds")
write_rds(oscar_split, "data/processed/oscar_split.rds")
write_rds(oscar_train, "data/processed/oscar_train.rds")
write_rds(oscar_test, "data/processed/oscar_test.rds")
write_rds(oscar_folds, "data/processed/oscar_folds.rds")



