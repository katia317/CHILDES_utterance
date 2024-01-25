library(tidyverse)
library(dplyr)

## Create your goal tibble to replicate

# Run this line to see what your end product should look like
sw.wrangled.goal <- read_csv("/Users/katia/Desktop/D2M/course data/sw-wrangled.csv") %>% 
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
# Use any returned information about mismatches to adjust your code as needed
all.equal(sw.wrangled.goal, sw.wrangled)
