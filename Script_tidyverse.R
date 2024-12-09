library(tidyverse)

# Sélection de variables
iris %>% select(Petal.Width, Species)

# Création d'un tibble
tib1 <- iris %>% 
  filter(Species == "versicolor" | Species == "virginica") %>% 
  as.tibble()
head(tib1)
class(tib1)

# 