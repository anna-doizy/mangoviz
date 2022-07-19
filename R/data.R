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

#' Data for pruning cycle graph: Pruning dates
#'
#' @format A data frame of 4 x 6
#' 
#' \describe{
#'   \item{Taille}{Type of pruning}
#'   \item{Date_taille}{Pruning date}
#'   \item{Depart}{Beginning of the arrow}
#'   \item{Pointe}{End of the arrow}
#'   \item{pos_img}{Position of the shears image}
#'   \item{img}{Path to the shears image}
#'   }
"date_taille"

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

#' Data for pruning cycle graph: Date labels for the x axis
#'
#' @format A data frame of 47 x 3
#' 
#' \describe{
#'   \item{pas}{date break}
#'   \item{annee}{turn the year into a general identifier}
#'   \item{etiquette}{date label, with a month format + the year id once per year}
#'   }
"date_labels"
