library(shiny)
library(tidyverse)

## The dataset (taken from lab01)
pokedata <- read_csv("pokemon.csv")
pokedata <- pokedata %>% select(Type_1,Type_2, Generation, Color, Egg_Group_1, Egg_Group_2, Body_Style)
