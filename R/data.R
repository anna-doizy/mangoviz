#' Translation of tabs
#'
#' @format data.frame 7x4
"onglets"

#' Translation of the rest of the user interface
#'
#' @format data.frame 48x4
"textesUI"

#' Meteorological data from meteo France
#'
#' @format data.frame 497x9
#' \describe{
#'   \item{NUM_POSTE}{Station identifier}
#'   \item{DAT}{Month as a date vector (March, April and May from 2018 to 2020)}
#'   \item{TMMOY}{Monthly average of the daily mean temperatures in °C}
#'   \item{RR}{Monthly cumul of rain in mm}
#'   \item{Nom}{Name of the closest city}
#'   \item{Latitude, Longitude}{Geographical coordinates of the station}
#'   \item{Altitude}{Station elevation}
#'   \item{Mois}{Month as a character vector}
#' }
#' 
#' @source \url{https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=115&id_rubrique=38}
# "meteo"

