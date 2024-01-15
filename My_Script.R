options(repos = c(CRAN = "https://cran.rstudio.com"))
library(tidyverse)
library(papaja)
library(ggplot2)

# Assignment 1
num_v <- 22
str_v <- "Jimmy Butler"

Miami_Heat <- data.frame(
  Players = c("Butler", "Adebayo", "Herro", "Robinson", "Love"),
  Numbers = c(22, 13, 14, 55, 42)
)

# Assignment 4
# prints a greeting based on "name"
hello_world <- function(name) {
  if (name == "Dr.Dowling"){
    paste0("Good morning", "," , name, "!")
  }
  else if (name == "Voldemort") {
    paste0(name, ", Expelliarmus!")
  }
  else if (name %in% c("Monica","Phoebe","Rachel")) {
    paste0(name, ", How you doing?")
  }
  else {
    paste0("I don’t know you:(")
  }
}

