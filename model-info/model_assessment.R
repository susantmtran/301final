library(tidyverse)
library(tidymodels)


rf_tuned <- read_rds("model-info/tuned/rf_tuned_best.rds")
nn_tuned <- read_rds("model-info/tuned/nn_tuned_best.rds")
bt_tuned <- read_rds("model-info/tuned/bt_tuned_best.rds")
mlp_tuned <- read_rds("model-info/tuned/mlp_tuned_best.rds")
en_tuned <- read_rds("model-info/tuned/en_tuned_best.rds")

tune_results <- tibble(
  model_type = c("rf_tuned", "nn_tuned", "mlp_tuned", "en_tuned", "bt_tuned"),
  tune_info = list(rf_tuned, nn_tuned, mlp_tuned, en_tuned, bt_tuned),
  assessment_info = map(tune_info, collect_metrics),
  best_model = map(tune_info, ~select_best(.x, metric = "accuracy")))

tune_results %>%
  select(model_type, assessment_info) %>%
  unnest(assessment_info) %>%
  filter(.metric == "accuracy") %>%
  group_by(model_type) %>%
  summarise(accuracy = max(mean)) %>% 
  arrange(desc(accuracy))



