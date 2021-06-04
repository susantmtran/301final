library(tidyverse)
library(tidymodels)

rf_tuned <- read_rds("rf_tuned.rds")
nn_tuned <- read_rds("nn_tuned.rds")

tune_results <- 
  tibble(
    model_type = c("rf_tuned", "nn_tuned"),
    tune_info = list(nn_tuned, rf_tuned),
    assessment_info = map(tune_info, collect_metrics),
    best_model = map(tune_info, ~select_best(.x, metric = "accuracy"))
  )

tune_results %>%
  select(model_type, assessment_info) %>%
  unnest(assessment_info) %>%
  filter(.metric == "accuracy") %>%
  group_by(model_type) %>%
  summarise(Accuracy = max(mean))

  