# Modeling with tidy models with the aim of predicting penguin sex (male or female)

# Making a new df with the relevant variables 
penguin_df <- penguins %>%
  filter(!is.na(sex)) %>%
  mutate(body_mass_kg = body_mass_g/1000) %>%
  select(-year, -island, -body_mass_g)

glimpse(penguin_df)
skim(penguin_df)


# Data splitting ----------------------------------------------------------
set.seed(123)
penguin_split <- initial_split(penguin_df, strata = sex)
penguin_split

penguin_train <- training(penguin_split)
penguin_test <- testing(penguin_split)

cv_folds <- vfold_cv(penguin_train)

# Logistic Regression -----------------------------------------------------
logistic_recipe <- 
  recipe(sex ~ ., data = penguin_train) %>%
  step_corr(all_numeric()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

logistic_spec <- 
  logistic_reg() %>% 
  set_engine("glm") 

logistic_workflow <- 
  workflow() %>% 
  add_recipe(logistic_recipe) %>% 
  add_model(logistic_spec)

# Evaluating
logistic_fit <-
  logistic_workflow %>% 
  last_fit(penguin_split)

logistic_fit 

logistic_fit %>% 
  collect_metrics()

logistic_fit %>% 
  pull(.predictions)

logistic_fit %>% 
  collect_predictions() %>% 
  conf_mat(truth = sex, estimate = .pred_class)
#only 4 penguins are predicted wrong

#final model using all data
final_logistic_fit <-
  logistic_workflow %>% 
  fit(penguin_df)

final_logistic_fit

# Tuning a Random Forest model -----------------------------------------------------------
library(usemodels)
use_ranger(sex~ ., data = penguin_df) #gives a template for modeling

ranger_recipe <- 
  recipe(sex ~., data = penguin_train) %>%
  step_corr(all_numeric()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

ranger_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_mode("classification") %>% 
  set_engine("ranger") 

ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

set.seed(48276)
ranger_tune <-
  tune_grid(ranger_workflow, resamples = cv_folds, grid = crossing(mtry = 1:3, min_n = c(5, 10)))

# Evaluating 
ranger_tune %>% 
  collect_metrics()

ranger_tune %>%
  select_best(metric = "roc_auc")

autoplot(ranger_tune)

rf_param <- 
  tibble(
    mtry = 1,
    min_n = 5)

final_ranger_wflow <- 
  ranger_workflow %>% 
  finalize_workflow(rf_param)

final_ranger_wflow

ranger_fit <-
  final_ranger_wflow %>% 
  last_fit(penguin_split)

ranger_fit 

ranger_fit %>% 
  collect_metrics()

ranger_fit %>% 
  pull(.predictions)

ranger_fit %>% 
  collect_predictions() %>% 
  conf_mat(truth = sex, estimate = .pred_class)

#final model using all data
final_ranger_fit <-
  final_ranger_wflow %>% 
  fit(penguin_df)

