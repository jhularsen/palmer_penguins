

# Explore the data --------------------------------------------------------
penguins %>%
  filter(!is.na(sex)) %>%
  mutate(body_mass_kg = body_mass_g/1000) %>%
  ggplot(aes(flipper_length_mm, bill_length_mm, color = sex, size = body_mass_kg)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~species)

penguins_df <- penguins %>%
  filter(!is.na(sex)) %>%
  mutate(body_mass_kg = body_mass_g/1000) %>%
  select(-year, -island, -body_mass_g)


# Build a model -----------------------------------------------------------
set.seed(123)
penguin_split <- initial_split(penguins_df, strata = sex)
penguin_train <- training(penguin_split)
penguin_test <- testing(penguin_split)

set.seed(123)
penguin_boot <- bootstraps(penguin_train)
penguin_boot

glm_spec <- logistic_reg() %>%
  set_engine("glm")

glm_spec

rf_spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_spec

# Next let’s start putting together a tidymodels workflow(), 
# a helper object to help manage modeling pipelines with pieces that fit together like Lego blocks. 
# Notice that there is no model yet: Model: None.
penguin_wf <- workflow() %>%
  add_formula(sex ~ .)

penguin_wf

# Now we can add a model, and the fit to each of the resamples. First, we can fit the logistic regression model.
glm_rs <- penguin_wf %>%
  add_model(glm_spec) %>%
  fit_resamples(
    resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE)
  )

glm_rs

# Second, we can fit the random forest model.
rf_rs <- penguin_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE)
  )

rf_rs
# We have fit each of our candidate models to our resampled training set!

# Evaluate model ----------------------------------------------------------
# Now let’s check out how we did.
collect_metrics(glm_rs)
collect_metrics(rf_rs)
# Very similar results, so lets pick the logistic regression model since its simpler!

# How are the two classes predicted?
glm_rs %>%
  conf_mat_resampled()

# We can also make an ROC curve.
glm_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()
# This ROC curve is more jagged than others you may have seen because the dataset is small

# It is finally time for us to return to the testing set. 
# Notice that we have not used the testing set yet during this whole analysis; 
# the testing set is precious and can only be used to estimate performance on new data. 
# Let’s fit one more time to the training data and evaluate on the testing data using the function last_fit().
penguin_final <- penguin_wf %>%
  add_model(glm_spec) %>%
  last_fit(penguin_split)

penguin_final
# The metrics and predictions here are on the testing data.

collect_metrics(penguin_final)

collect_predictions(penguin_final) %>%
  conf_mat(sex, .pred_class)

# The coefficients (which we can get out using tidy()) have been estimated using the training data. 
# If we use exponentiate = TRUE, we have odds ratios.
penguin_final$.workflow[[1]] %>%
  tidy(exponentiate = TRUE)
# The largest odds ratio is for bill depth, with the second largest for bill length. 
# An increase of 1 mm in bill depth corresponds to almost 4x higher odds of being male. 
# The characteristics of a penguin’s bill must be associated with their sex.

# We don’t have strong evidence that flipper length is different between male and female penguins, 
# controlling for the other measures; maybe we should explore that by changing that first plot!
penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(bill_depth_mm, bill_length_mm, color = sex, size = body_mass_g)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~species)
# Yes, the male and female penguins are much more separated now.
