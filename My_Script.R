install.packages("tidyverse")
library(tidyverse)
install.packages("papaja")
library(papaja)
library(ggplot2)

# Assign 1+ numeric variable and 1+ string/character variable
num_v <- 22
str_v <- "Jimmy Butler"

# BONUS: Create a dataframe
Miami_Heat <- data.frame(
  Players = c("Butler", "Adebayo", "Herro", "Robinson", "Love"),
  Numbers = c(22, 13, 14, 55, 42)
)
