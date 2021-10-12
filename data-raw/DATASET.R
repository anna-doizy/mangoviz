## code to prepare dataset goes here

# Translations ------------------------------------------------------------

onglets <- read.csv("data-raw/onglets.csv", encoding = "UTF-8")
textesUI <- read.csv("data-raw/textesUI.csv", encoding = "UTF-8")
# encoding for getting rid of the R-CMD check "found non-ASCII strings" warning


usethis::use_data(onglets, textesUI, overwrite = TRUE)
devtools::document()



# Evaluation variétale ----------------------------------------------------

library(readr)
library(dplyr)
library(stringr)


variete <- read_delim("data-raw/MA02.txt", locale = locale(encoding = "ISO-8859-1", decimal_mark = ",", grouping_mark = "")) %>%
  rename(Annee = annee) %>% 
  group_by(arbre, cultivar, Annee) %>% 
  summarise(
    masse = sum(masse),
    nbfruit = sum(nbfruit)
  ) %>% 
  ungroup() %>% 
  mutate(
    masse_fruit = masse / nbfruit,
    cultivar = cultivar %>% str_to_title() %>% factor(),
    X = str_sub(arbre, end = 1) %>% factor(levels = LETTERS[11:1]),
    Y = str_sub(arbre, start = 2) %>% factor(levels = 1:17)
  ) %>% 
  tidyr::pivot_longer(masse:masse_fruit, names_to = "Mesure", values_to = "Valeur")


usethis::use_data(variete, overwrite = TRUE)
devtools::document()




