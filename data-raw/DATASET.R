## code to prepare dataset goes here

# Translations ------------------------------------------------------------

onglets <- read.csv("data-raw/onglets.csv", encoding = "UTF-8")
textesUI <- read.csv("data-raw/textesUI.csv", encoding = "UTF-8")
# encoding for getting rid of the R-CMD check "found non-ASCII strings" warning


usethis::use_data(onglets, textesUI, overwrite = TRUE)
devtools::document()


