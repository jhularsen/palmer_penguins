# Packages ----------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(skimr)
library(palmerpenguins)
library(RColorBrewer)
library(infer)
theme_set(theme_minimal())

# EDA -----------------------------------------------------------

?penguins
glimpse(penguins)
skim(penguins)

# Some summary statistics
penguins %>% 
  group_by(species) %>% 
  summarise(count = n(), across(where(is.numeric), mean, na.rm = TRUE)) %>% 
  select(-year)

penguins %>% 
  group_by(species, island) %>%
  summarise(count = n()) 
# Adelie lives on every island, Chinstrap only lives on Dream and Gentoo only lives on Biscoe

# Visualisations

penguins %>%
  drop_na() %>%
  count(sex, species) %>%
  ggplot() + geom_col(aes(x = species, y = n, fill = species)) +
  geom_label(aes(x = species, y = n, label = n)) +
  scale_fill_brewer(palette = "Dark2") + 
  facet_wrap(~sex) +
  theme(legend.position = "none") + 
  labs(x = "", y = "Count")
# We observe a similar amount of male and female penguins for each species

penguins %>% 
  ggplot(aes(x = flipper_length_mm, y = bill_length_mm, color = species)) +
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE, size = 1.3) + 
  scale_color_brewer(palette = "Dark2") + 
  theme(text = element_text(size = 14)) + 
  theme(legend.position = "bottom") +
  labs(x = "Flipper Length (mm)", y = "Bill Length (mm)", color = "Penguin species", 
       title = "Bill length increases with flipper length") 

arrows <- tibble(
    x1 = c(187, 212),
    x2 = c(205, 204),
    y1 = c(6000, 3000), 
    y2 = c(5500, 3500))

arrows

penguins %>% 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE, size = 1.3) + 
  theme(text = element_text(size = 14)) + 
  theme(legend.position = "bottom") +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)", color = "Penguin species", 
       title = "Body mass increases with flipper length") +
  annotate("text", x = 180, y = 6000, label = "Gentoo penguins \n are much larger") +
  annotate("text", x = 223, y = 3000, label = "It is hard to differentiate \n Chinstrap and Adelie") +
  geom_curve(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.8,
    color = "gray20", curvature = -0.3)

penguins %>% 
  ggplot(aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Dark2") + 
  theme(text = element_text(size = 14)) + 
  theme(legend.position = "none") +
  labs(x = "", y = "Body Mass (g)", 
       title = "Gentoo penguins are bigger penguins") 

penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = flipper_length_mm, y = bill_length_mm, color = sex, size = body_mass_g)) + 
  geom_point(alpha = 0.6) + 
  facet_wrap(~ species)
  
  
