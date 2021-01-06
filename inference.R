# Bootstrap 95% CI for mean body mass for gentoo penguins -----------------------------------------------------------------------

gentoo <- penguins %>% 
  filter(species == "Gentoo", !is.na(body_mass_g))

set.seed(123)

gentoo_resamples <- gentoo %>%
  rep_sample_n(size = nrow(penguins_gentoo), replace = TRUE, reps = 1000)

gentoo_means <- gentoo_resamples %>% 
  group_by(replicate) %>% 
  summarise(mean_body_mass = mean(body_mass_g))

# Bootstrap distribution (approximation of sampling distribution)
gentoo_means %>% 
  ggplot(aes(x = mean_body_mass)) +
  geom_histogram(bins = 20, color = "white", boundary = 5100) + 
  labs(x = "Resample mean", y = "Count")


# Percentile method 
quantile(gentoo_means$mean_body_mass, probs = c(0.025, 0.975))

# SE method 
x_bar <- mean(gentoo$body_mass_g)

gentoo_means %>%
  summarise(mean_of_means = mean(mean_body_mass), 
            x_bar = x_bar, 
            sd_of_means = sd(mean_body_mass), 
            ci_lower = x_bar-1.96*sd_of_means,
            ci_upper = x_bar+1.96*sd_of_means)

# Using the infer workflow
set.seed(123)

bootstrap_distribution <- gentoo %>% 
  specify(response = body_mass_g) %>% 
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") 

visualise(bootstrap_distribution) 

# Percentile method 
percentile_ci <- bootstrap_distribution %>% 
  get_ci(level = 0.95, type = "percentile")

percentile_ci  

visualize(bootstrap_distribution) + 
  shade_ci(endpoints = percentile_ci)

# SE method
standard_error_ci <- bootstrap_distribution %>% 
  get_ci(type = "se", point_estimate = x_bar)  

standard_error_ci 

visualize(bootstrap_distribution) + 
  shade_ci(endpoints = standard_error_ci)


# Hypothesis test for mean difference between Gentoo and Adelie ---------------------------------------------------------

gentoo_adelie <- penguins %>%
  filter(species %in% c("Adelie", "Gentoo"), !is.na(body_mass_g)) %>%
  droplevels()

gentoo_adelie %>% 
  ggplot(aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Dark2") + 
  theme(text = element_text(size = 14)) + 
  theme(legend.position = "none") +
  labs(x = "", y = "Body Mass (g)") 

gentoo_adelie %>% 
  group_by(species) %>% 
  summarize(n = n(), mean_body_mass = mean(body_mass_g), sd_body_mass = sd(body_mass_g))

# H0: mu_g - mu_a = 0 
set.seed(123)

null_distribution <- gentoo_adelie %>%
  specify(body_mass_g ~ species) %>%
  hypothesise(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Gentoo", "Adelie"))

null_distribution

obs_diff_means <- gentoo_adelie %>%
  specify(body_mass_g ~ species) %>%
  calculate(stat = "diff in means", order = c("Gentoo", "Adelie"))

obs_diff_means  

visualise(null_distribution, bins = 20) + 
  shade_p_value(obs_stat = obs_diff_means, direction = "both") 

null_distribution %>%
  get_p_value(obs_stat = obs_diff_means, direction = "both")
# Rejecting H0!

# We can also use a 95% CI (SE method)
set.seed(123)

bootstrap_distribution <- gentoo_adelie %>%
  specify(body_mass_g ~ species) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Gentoo", "Adelie")) 

se_ci <- bootstrap_distribution %>%
  get_ci(level = 0.95, type = "se", point_estimate = obs_diff_means)

se_ci

visualize(bootstrap_distribution) + 
  shade_confidence_interval(endpoints = se_ci)
# Note that 0 isn't included, so we can also reject the H0 here 


# Hypothesis test for mean difference between Chinstrap and Adelie ---------------------------------------------------------

chinstrap_adelie <- penguins %>%
  filter(species %in% c("Chinstrap", "Adelie"), !is.na(body_mass_g)) %>%
  droplevels()

chinstrap_adelie %>% 
  ggplot(aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Dark2") + 
  theme(text = element_text(size = 14)) + 
  theme(legend.position = "none") +
  labs(x = "", y = "Body Mass (g)") 
# Looks very similar, so I am not expecting a significant difference here

# H0: mu_c - mu_a = 0 
set.seed(123)

null_distribution <- chinstrap_adelie %>%
  specify(body_mass_g ~ species) %>%
  hypothesise(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Chinstrap", "Adelie"))

null_distribution

obs_diff_means <- chinstrap_adelie %>%
  specify(body_mass_g ~ species) %>%
  calculate(stat = "diff in means", order = c("Chinstrap", "Adelie"))

obs_diff_means

visualise(null_distribution, bins = 20) + 
  shade_p_value(obs_stat = obs_diff_means, direction = "both") 

null_distribution %>%
  get_p_value(obs_stat = obs_diff_means, direction = "both")
# Not rejecting H0!

# Can also use a 95% CI (Percentile method)
set.seed(123)

bootstrap_distribution <- chinstrap_adelie %>%
  specify(body_mass_g ~ species) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Chinstrap", "Adelie")) 

percentile_ci <- bootstrap_distribution %>% 
  get_ci(level = 0.95, type = "percentile")

percentile_ci

visualize(bootstrap_distribution) + 
  shade_confidence_interval(endpoints = percentile_ci) + 
  geom_vline(xintercept = 0, size = 2)
# We can't reject H0 since 0 is included in the CI