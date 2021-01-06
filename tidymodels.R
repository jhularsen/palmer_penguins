# Modeling with tidy models with the aim of predicting penguin sex (male or female)

# Making a new df with the relevant variables 
penguin_df <- penguins %>%
  filter(!is.na(sex)) %>%
  select(-year, island)

penguin_split <- initial_split(penguin_df)
penguin_split

