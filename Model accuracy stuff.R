library(tidyverse)
library(tidymodels)

rf_tuned <- read_rds("rf_tuned.rds")
nn_tuned <- read_rds("nn_tuned.rds")
bt_tuned <- read_rds("bt_tuned.rds")
mlp_tuned <- read_rds("mlp_tuned.rds")
en_tuned <- read_rds("en_tuned.rds")

tune_results <- 
  tibble(
    model_type = c("rf_tuned", "nn_tuned", "bt_tuned", "mlp_tuned", "en_tuned"),
    tune_info = list(nn_tuned, rf_tuned, bt_tuned, mlp_tuned, en_tuned),
    assessment_info = map(tune_info, collect_metrics),
    best_model = map(tune_info, ~select_best(.x, metric = "accuracy"))
  )

tune_results %>%
  select(model_type, assessment_info) %>%
  unnest(assessment_info) %>%
  filter(.metric == "accuracy") %>%
  group_by(model_type) %>%
  summarise(Accuracy = max(mean)) %>% 
  arrange(desc(Accuracy))

  