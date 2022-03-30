## code to prepare dataset goes here

# Translations ------------------------------------------------------------

textesUI <- read.csv("data-raw/textesUI.csv", encoding = "UTF-8")
# encoding for getting rid of the R-CMD check "found non-ASCII strings" warning


usethis::use_data(textesUI, overwrite = TRUE)
devtools::document()



# Evaluation variétale ----------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(forcats)


variete <- read_delim("data-raw/MA02.txt", locale = locale(encoding = "ISO-8859-1", decimal_mark = ",", grouping_mark = "")) %>%
  rename(Annee = annee) %>% 
  group_by(arbre, cultivar, Annee) %>% 
  summarise(
    masse = sum(masse/1000), # kg
    nbfruit = sum(nbfruit)
  ) %>% 
  ungroup() %>% 
  mutate(
    masse_fruit = masse / nbfruit * 1000, # g
    cultivar = cultivar %>% str_to_title() %>% factor(),
    X = str_sub(arbre, end = 1) %>% factor(levels = LETTERS[11:1]),
    Y = str_sub(arbre, start = 2) %>% factor(levels = 1:17)
  ) %>% 
  tidyr::pivot_longer(masse:masse_fruit, names_to = "Mesure", values_to = "Valeur")

taille <- read_delim("data-raw/MA05.txt", locale = locale(encoding = "ISO-8859-1", decimal_mark = ",", grouping_mark = "")) %>%
  filter(taille != "bordure") %>% 
  group_by(arbre, bloc, annee, taille) %>% 
  summarise(
    masse = sum(masse/1000, na.rm = TRUE), # kg
    nbfruit = sum(nbfruit)
  ) %>% 
  ungroup() %>% 
  mutate(
    masse_fruit = masse / nbfruit * 1000, # g
    Annee = str_sub(annee, end = 4) %>% as.numeric(),
    Taille = taille %>% fct_recode("taille_ete" = "taille02", "taille_hiver" = "taille07" ,"taille_sans" ="tem"),
    bloc = factor(bloc),
    X = str_sub(arbre, end = 1) %>% factor(levels = LETTERS[9:1]),
    Y = str_sub(arbre, start = 2) %>% factor(levels = 1:17)
  ) %>% 
  select(-annee, -taille) %>% 
  tidyr::pivot_longer(masse:masse_fruit, names_to = "Mesure", values_to = "Valeur")


usethis::use_data(variete, taille, overwrite = TRUE)
devtools::document()




