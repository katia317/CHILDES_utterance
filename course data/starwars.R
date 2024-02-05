library(tidyverse)
library(dplyr)

## Create your goal tibble to replicate

# Run this line to see what your end product should look like
sw.wrangled.goal <- read_csv("/Users/katia/Desktop/D2M/CHILDES_utterance/course data/sw-wrangled.csv") %>% 
  mutate(across(c(hair, gender, species, homeworld), factor)) # this is a quick-and-dirty fix to account for odd importing behavior

# View in console
sw.wrangled.goal 
# Examine the structure of the df and take note of data types
# Look closely at factors (you may need another function to do so) to see their levels
str(sw.wrangled.goal) 

## Use the built-in starwars dataset to replicate the tibble above in a tbl called sw.wrangled

sw.wrangled <- starwars %>% 
  mutate(
    first_name = word(name, 1),
    last_name = word(name, 2,-1),
    initials = paste0(substr(first_name, 1, 1), substr(last_name, 1, 1)),
    height_in = height * 0.3937,
    height_cm = height,
    mass = mass,
    hair = as.factor(replace_na(hair_color, "bald")),
    gender = as.factor(case_when(
      gender == "masculine" ~ "m",
      gender == "feminine" ~ "f",
      TRUE ~ NA
    )),
    species = as.factor(toupper(species)),
    homeworld = as.factor(homeworld),
    brown_hair = as.logical(replace_na(str_detect(hair, "brown"), FALSE))
  )%>%
  select(first_name, last_name, initials, height_in, height_cm, mass, hair, gender, species, homeworld, brown_hair)

sw.wrangled <- sw.wrangled %>%
  arrange(last_name, first_name, height_in) %>%
  filter(!is.na(height_cm))

write_csv(sw.wrangled,"sw.wrangled.csv")

## Check that your sw.wrangled df is identical to the goal df
all.equal(sw.wrangled.goal, sw.wrangled)

# Assignmengt 11.1

library(ggplot2)
# Assignment 11.1
# the histogram PLOT 1
sw.wrangled <- read.csv("/Users/katia/Desktop/D2M/CHILDES_utterance/course data/sw.wrangled.csv")
ggplot(sw.wrangled, aes(x = height_cm)) +
  geom_histogram(binwidth = 10) +
  labs(x = "height_cm", y = "count")

# the bar chart PLOT 2
ggplot(sw.wrangled, aes(x = reorder(hair, -table(hair)[hair]))) +
  geom_bar() +
  labs(x = "sorted_hair", y = "count")

# the point chart PLOT 3
filtered_sw <- sw.wrangled %>% filter(mass <= 300)
ggplot(filtered_sw, aes(x = height_in, y = mass)) +
  geom_point(shape = 17)
  labs(x = "height_in", y = "mass")

## Assignment 12.1

# Plot 1
sw.wrangled <- read.csv("/Users/katia/Desktop/D2M/CHILDES_utterance/course data/sw.wrangled.csv")
filtered_sw <- sw.wrangled %>% filter(mass <= 300)
ggplot(filtered_sw, aes(x = hair, y = mass, fill = hair)) +
  geom_boxplot(aes(x = reorder(hair, -table(hair)[hair])))+
  geom_point() + 
  labs(x = "Hair color(s)", y = "Mass (kg)", fill = "Colourful hair")+
  theme(panel.background = element_rect(fill = "grey90"))

# Plot 2
brown_sw <- filtered_sw %>%
  mutate(hair = ifelse(hair == "brown", "Has brown hair", "No brown hair"))

plot_brown <- ggplot(brown_sw, aes(x = mass, y = height_in)) +
  geom_smooth(method = "lm") +
  xlim(-200, 200) +
  ylim(-4, 200) +
  geom_point() +
  facet_wrap(~hair) + 
  theme_minimal() +
  labs(title = "Mass vs. height by brown-hair-havingness",
       subtitle = "A critically important analysis")
print(plot_brown)

# Plot 3 
library(tidyverse)
species_fst <- sw.wrangled %>%
  mutate(species_first_letter = substr(species, 1, 1)) %>%
  filter(!is.na(species_first_letter)) %>%
  group_by(species_first_letter, gender) %>%
  summarise(count = n(), .groups = 'drop')
plot_species <- ggplot(species_fst, aes(x = count, y = fct_rev(species_first_letter), fill = gender))+
  geom_col()+
  labs(y = "species_first_letter")
print(plot_species)
  