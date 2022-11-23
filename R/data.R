#' Varietal essay, from 2010 to 2021
#'
#' @format A data frame of 4 824 x 7
#' 
#' \describe{
#'   \item{arbre}{tree identifier}
#'   \item{cultivar}{mango tree cultivar}
#'   \item{Annee}{Year of the observation}
#'   \item{X, Y}{Tree coordinates}
#'   \item{Mesure, Valeur}{Either the production in kilograms (masse) or the number of fruits (nbfruit) or the mean fruit weight in grams (masse_fruit) per tree per year of harvest}
#'   }
"variete"

#' Pruning essay, from 2007 to 2021
#'
#' @format A data frame of 6 063 x 8
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
#' @format 4 columns for identifier, french, english and spanish
"textesUI"

#' Data for pruning cycle graph: Mango tree steps (through 3 cycles)
#'
#' @format A data frame of 15 x 4
#' 
#' \describe{
#'   \item{Debut}{Date of the beginning of the step}
#'   \item{Fin}{Date of the end of the step}
#'   \item{Etape}{Name/id of the step}
#'   \item{Cycle}{Cycle id}
#'   }
"cycle"

