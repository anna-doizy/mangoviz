#' Varietal essay
#'
#' @format A data frame of 2 814 x 7
#' 
#' \describe{
#'   \item{arbre}{tree identifier}
#'   \item{cultivar}{mango tree cultivar}
#'   \item{Annee}{Year of the observation}
#'   \item{X, Y}{Tree coordinates}
#'   \item{Mesure, Valeur}{Either the production in kilograms (masse) or the number of fruits (nbfruit) or the mean fruit weight in grams (masse_fruit) per tree per year of harvest}
#'   }
"variete"

#' Pruning essay
#'
#' @format A data frame of 4 860 x 8
#' 
#' \describe{
#'   \item{arbre}{tree identifier}
#'   \item{bloc}{block of trees}
#'   \item{Annee}{Year of the observation}
#'   \item{Taille}{treatment: winter pruning, summer pruning or no pruning}
#'   \item{X, Y}{Tree coordinates}
#'   \item{Mesure, Valeur}{Either the production in kilograms (masse) or the number of fruits (nbfruit) or the mean fruit weight in grams (masse_fruit) per tree per year of harvest}
#'   }
"taille"

#' Translation texts for the UI
#' 
#' @format 3 columns for identifier, french and english
"textesUI"
