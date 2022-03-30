
# A FAIRE ####

# changer couleur du checkbox_year (le rouge est pour les bandes de titre) A VOIR
# changer les couleurs des var par celles de Isabelle OK MAIS PALICHON SUR LES GRAPHIQUES
# Confirmer les années de taille des arbres !!??

# vérifier soigneusement les textui

# TAILLE
# début de la taille plus marqué et visible (rectangle ?)
# "Les arbres ont été taillés tous les deux ans à partir de 2012"

# VARIETE
# ajouter possibilité d'afficher toutes les variétés ensemble (avec un switch ?)
# couleur du gradient : tester https://personal.sron.nl/~pault/#fig:scheme_iridescent : faire des comparaisons à montrer à Isabelle


# spatial :
# revoir échelle de couleurs, les mettre en classe éventuellement A VOIR
# selection de chaque cultivar
# faire un graphe spatial global


# AUTRE

# - envoyer les fichiers texte à Fred pour relecture
# - anglais presque OK + espagnol
# - mode d'emploi dans l'accueil
# - logo : attendre photo de Fred + choix de la première police
# - responsive : image width + ratio des graphiques (taille de police des labels des axes) A VOIR
# - MAJ des fiches variétales


# - garder en tete possible ajout des valeurs brix pH etc. (données au niveau du fruit)


# - PUBLICATION
# format de citation de l'appli à réfléchir
# les données dans un datapaper -> mettre le dépôt en public à ce moment
# symposium mangue oct-nov 2023 Espagne
# quand héberger dans le serveur du CIRAD ?


# FAIT

# - il faut sélectionner au moins une variété pour que le dernier graphique fonctionne OK
# - je n'ai pas encore intégré les bandes de confiance et la ligne horizontale pour le dernier graphique dans le cas où une seule variété est sélectionnée OK
# - ordre des ligne colonne du plan de parcelle à mettre comme suivi spatial OK
# - décimales dans les étiquettes du graphe 1 à enlever OK
# 1/ comp variétale : barres horizontales (repasser en ggplot ?) NON
# 2/ suivi temporel : trouver un format de sélection qui prend moins de place OK
# 3/ suivi spatial : en bas, tester de repasser en ggplot avec un facet_wrap(~ Annee) OK
# - format des nombres à checker en fonction de la langue OK
# Variables mesurées (remplacer le texte de présentation qui ne veut rien dire) OK
# mesure de la production : masse + nb de fruits Ok
# cacher le code source pour le moment et mettre le dépôt en privé OK
# - ajouter logos des institutions OK
# - Intégrer les nouveaux graphiques sans barres d'erreur OK
# moyenne au lieu de médiane OK
# couleurs tranchées et cohérentes OK
# penser aux traductions OK
# ratio des graphes OK
# nom et titre des axes, titres des graphiques OK
# VIOLIN
# surface prop. nb points OK
# si peu de points, enveler la couche violin NON
# noms des var à 45° ou en 2 lignes si besoin OK
# TEMPS
# ajouter ligne horizontale pour la moyenne globale dans le suivi temporel OK
# couleur sur les traits et les points moyenne ? OK
# relier les points moyenne OK
# - nettoyer fichier description OK
# trouver des meilleurs couleurs pour les variétés OK
# Améliorer les titres des graphiques et les sous-textes OK
# ajouter du loading pour les graphiques ? OK
# - harmoniser les pipes OK
# - erreurs du check : noms non portables pour les fiches variétales (à harmoniser avec les radio buttons correspondants) + taille de certains fichiers à diminuer (avec squash.io) OK
# Nom et contenu des onglets :
#   le verger (origine/description + photo parcelle + photo de chaque var) : mettre des radio buttons OK
#   le suivi de la production : les graphiques de suivi + changer l'input de place OK
#   bilan/synthèse : les fiches variétales qui reprennent les résultats de l'essai : OK
# intégrer les graphiques taille dans l'appli OK
# enlever le "onclick" ("bug carrés noirs") OK
# harmoniser police des graphiques avec le reste de l'app OK
# réorganisation layout onglet verger : le plan doit être suffisamment grand, penser à un espace pour la photo du verger OK



# server ####


lang <- "fr"


library(dplyr)
library(ggplot2)
library(ggiraph)
library(mangoviz)

# VARIETES ####

input <- list(
  # variete_mesure = "masse",
  variete_mesure = "nbfruit",
  # variete_mesure = "masse_fruit",
  variete_checkbox_year = 2010:2015,
  # variete_checkbox_year = 2016,
  # variete_multi_var = c("Heidi", "Irwin", "José")
  variete_multi_var = "Heidi"
)


coul_var <- c("#b94137", "#0080b4", "#008355", "#7f4ecc", "#ce7e26", "#8aa543", "#56423e", "#be4b7f", "#a5af98", "#00c1ff") %>% setNames(unique(variete$cultivar))


## Plan de la parcelle ####

variete %>% 
  distinct(X, Y, cultivar) %>%
  {ggplot(.) +
      aes(x = X, y = Y, fill = cultivar, tooltip = cultivar, data_id = cultivar) +
      geom_tile_interactive(color = "black") +
      scale_fill_manual(values = coul_var, guide = guide_legend(byrow = TRUE)) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      coord_fixed() +
      labs(x = NULL, y = NULL, fill = textesUI[textesUI$id == "cultivar", lang])} %>% 
  girafe(
    ggobj = ., height_svg = 8,
    options = list(
      opts_hover_inv(css = "opacity:0.4;"),
      opts_tooltip(use_fill = TRUE),
      opts_hover(css = "fill:black;opacity:0.8;"),
      opts_selection(type = "none")
    )
  )




## comparaison des variétés ####

if(!is.null(input$variete_checkbox_year)) { # if no selected date, no plot
  {variete %>% 
      filter(Mesure == input$variete_mesure, Annee %in% input$variete_checkbox_year, !is.na(Valeur)) %>% 
      ggplot() +
      aes(x = cultivar, y = Valeur, fill = cultivar) +
      geom_violin(alpha = 0.3, color = "transparent", scale = "count") +
      geom_jitter(alpha = 0.3, width = 0.2, height = 0) +
      geom_point(stat = "summary", fun = mean, size = 4, color = "white") +
      geom_point_interactive(
        stat = "summary", 
        fun = mean, size = 3, 
        aes(color = cultivar, tooltip = paste(..color.., round(..y.., 1), sep = "<br>"), data_id = cultivar)
      ) +
      scale_fill_manual(values = coul_var, aesthetics = c("colour", "fill")) +
      labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$variete_mesure, lang]) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  } %>% 
    girafe(
      ggobj = ., 
      options = list(
        opts_hover_inv(css = "opacity:0.4;"),
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "fill:black;"),
        opts_selection(type = "none")
      )
    )
}

## comparaison des années (suivi temporel) ####

  
if(!is.null(input$variete_multi_var)) # if no selected cultivar, no plot
{
  {if(length(input$variete_multi_var) == 1) { # if one selected cultivar
    variete %>% 
      filter(Mesure == input$variete_mesure, cultivar == input$variete_multi_var, !is.na(Valeur)) %>%
      ggplot() +
      aes(x = Annee, y = Valeur) +
      geom_line_interactive(aes(group = arbre, data_id = arbre, tooltip = arbre, hover_css = "fill:none"), alpha = 0.1) +
      geom_point_interactive(alpha = 0.3, aes(data_id = arbre, tooltip = arbre)) +
      geom_smooth_interactive(method = "lm", formula = "y~1", se = FALSE, color = "black", linetype = 2, aes(tooltip = paste(textesUI[textesUI$id == "global_mean", lang], round(..y.., 1), sep = "<br>"))) +
      geom_line(stat = "summary", fun = mean, aes(colour = cultivar)) +
      geom_point_interactive(stat = "summary", fun = mean, size = 3, aes(colour = cultivar, tooltip = paste(..color.., round(..y.., 1), sep = "<br>"))) +
      scale_color_manual(values = coul_var[input$variete_multi_var]) +
      labs(
        x = NULL, y = NULL, 
        title = textesUI[textesUI$id == input$variete_mesure, lang],
        colour = textesUI[textesUI$id == "cultivar", lang]
      )
  } else { # if several selected cultivars
    variete %>% 
      filter(Mesure == input$variete_mesure, cultivar %in% input$variete_multi_var) %>%
      group_by(Annee, cultivar) %>% 
      summarise(
        Moyenne = mean(Valeur, na.rm = TRUE)
      ) %>%
      suppressMessages() %>% # group message
      # suppressWarnings() %>% # NA & NaN values
      ggplot() +
      aes(x = Annee, y = Moyenne, colour = cultivar, tooltip = paste(cultivar, round(Moyenne, 1), sep = "<br>"), data_id = cultivar) +
      geom_line(aes(group = cultivar)) +
      geom_point_interactive() +
      scale_color_manual(values = coul_var[input$variete_multi_var]) +
      labs(
        x = NULL, y = NULL, 
        title = textesUI[textesUI$id == input$variete_mesure, lang],
        colour = textesUI[textesUI$id == "cultivar", lang]
      )
  } } %>% 
    girafe(
      ggobj = ., 
      options = list(
        opts_hover_inv(css = "opacity:0;"),
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "stroke-width:3px;"),
        opts_selection(type = "none")
      )
    )
}


## mesures à l'échelle de la parcelle ####


if(!is.null(input$variete_multi_var)) # if no selected cultivar, no plot
  variete %>% 
  filter(Mesure == input$variete_mesure) %>%
  {ggplot(.) +
      aes(x = X, y = Y, fill = Valeur, tooltip = paste(cultivar, round(Valeur, 1), sep = "<br>"), data_id = cultivar) +
      geom_tile_interactive(colour = "black") +
      scale_fill_gradientn(
        colours = c("#a40000",  "#de7500", "#ee9300", "#f78b28", "#fc843d", "#ff7e50", "#ff5d7a", "#e851aa", "#aa5fd3", "#0070e9"), 
        na.value = "transparent" # travailler encore le gradient de couleurs
      ) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      coord_fixed() +
      labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$variete_mesure, lang], fill = NULL) +
      facet_wrap(~ Annee, nrow = 1)} %>% 
  girafe(
    ggobj = ., width_svg = 16,
    options = list(
      opts_hover_inv(css = "opacity:0.2;"),
      opts_tooltip(use_stroke = TRUE),
      opts_hover(css = ""),
      opts_selection(type = "single", css = "fill:black;")
    )
  )

# TAILLE ####




coul_taille <- c(`taille_ete` = "darkgreen", `taille_hiver` = "darkblue", `taille_sans` = "darkred")

input <- list(
  # taille_mesure = "masse",
  # taille_mesure = "nbfruit",
  taille_mesure = "masse_fruit",
  taille_checkbox_year = 2011:2018,
  # taille_checkbox_year = 2016,
  # taille_multi = c("taille_ete", "taille_hiver", "taille_sans")
  taille_multi = "taille_ete"
)


# Attention la taille des arbres a commencé entre 2011 et 2012
# que signifie taille en été/en hiver par rapport à l'année de récolte ?


## plan de la parcelle ####
# représentation des bordures à valider
# expliquer les bloc
# traduire label des bloc

taille %>% 
  distinct(X, Y, arbre, bloc, Taille) %>%
  {ggplot(.) +
      aes(x = X, y = Y, fill = Taille, data_id = Taille, tooltip = arbre) +
      # geom_tile_interactive(color = "black", aes(label = arbre, tooltip = paste(..label.., textesUI[textesUI$id == "taille_ete", lang], sep = "<br>"))) + # ne marche pas
      geom_tile_interactive(color = "black") +
      scale_fill_manual(
        values = coul_taille, labels = textesUI[textesUI$id %in% levels(taille$Taille), lang] %>% setNames(levels(taille$Taille)),
        guide = guide_legend(byrow = TRUE)
      ) +
      # scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      coord_fixed() +
      labs(x = NULL, y = NULL, fill = textesUI[textesUI$id == "taille_legend", lang])} %>% 
  girafe(
    ggobj = ., 
    options = list(
      opts_hover_inv(css = "opacity:0.4;"),
      opts_tooltip(use_fill = TRUE),
      opts_hover(css = "fill:black;opacity:0.8;")
    )
  )



## comparaison des tailles ####
# déselectionner par défaut les années avant 2011 ?
# distinguer les blocs ? que mettre dans les tooltips ?

if(!is.null(input$taille_checkbox_year)) { # if no selected date, no plot
  {taille %>% 
      filter(Mesure == input$taille_mesure, Annee %in% input$taille_checkbox_year, !is.na(Valeur)) %>% 
      ggplot() +
      aes(x = Taille, y = Valeur, fill = Taille, label = arbre) +
      geom_violin(alpha = 0.3, color = "transparent", scale = "count") +
      geom_jitter_interactive(alpha = 0.3, width = 0.2, height = 0) + #, aes(tooltip = paste(..label.., round(..y.., 1), sep = "<br>"), data_id = bloc)) +
      geom_point(stat = "summary", fun = mean, size = 4, color = "white") +
      geom_point_interactive(
        stat = "summary", 
        fun = mean, size = 3, 
        aes(color = Taille, tooltip = round(..y.., 1), data_id = Taille)
      ) +
      scale_fill_manual(values = coul_taille, aesthetics = c("colour", "fill")) +
      labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$taille_mesure, lang]) +
      theme(legend.position = "none")
  } %>% 
    girafe(
      ggobj = ., 
      options = list(
        opts_hover_inv(css = "opacity:0.4;"),
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "fill:black;"),
        opts_selection(type = "none")
      )
    )
}


## comparaison des années (suivi temporel) ####
# ajouter une ligne verticale en 2011 OK
# améliorer l'échelle des années OK

if(!is.null(input$taille_multi)) { # if no selected taille, no plot
  {if(length(input$taille_multi) == 1) { # if one selected taille
    taille %>% 
      filter(Mesure == input$taille_mesure, Taille == input$taille_multi, !is.na(Valeur)) %>%
      ggplot() +
      aes(x = Annee, y = Valeur) +
      geom_vline_interactive(xintercept = 2010.5, color = "white", size = 2, aes(tooltip = textesUI[textesUI$id == "pruning_start", lang])) + # ou 2011.5 ??
      geom_line_interactive(aes(group = arbre, data_id = arbre, tooltip = arbre, hover_css = "fill:none"), alpha = 0.1) +
      geom_point_interactive(alpha = 0.3, aes(data_id = arbre, tooltip = arbre)) +
      geom_line(stat = "summary", fun = mean, aes(colour = Taille)) +
      geom_point_interactive(stat = "summary", fun = mean, size = 3, aes(colour = Taille, tooltip = paste(..color.., round(..y.., 1), sep = "<br>"))) +
      geom_smooth_interactive(method = "lm", formula = "y~1", se = FALSE, color = "black", linetype = 2, aes(tooltip = paste(textesUI[textesUI$id == "global_mean", lang], round(..y.., 1), sep = "<br>"))) +
      scale_color_manual(values = coul_taille[input$taille_multi]) +
      scale_x_continuous(breaks = seq(2008, 2018, by = 2)) +
      labs(
        x = NULL, y = NULL, 
        title = textesUI[textesUI$id == input$taille_mesure, lang],
        colour = textesUI[textesUI$id == "taille_legend", lang]
      )
  } else { # if several selected taille
    taille %>% 
      filter(Mesure == input$taille_mesure, Taille %in% input$taille_multi) %>%
      group_by(Annee, Taille) %>% 
      summarise(
        Moyenne = mean(Valeur, na.rm = TRUE)
      ) %>%
      suppressMessages() %>% # group message
      # suppressWarnings() %>% # NA & NaN values
      ggplot() +
      aes(x = Annee, y = Moyenne, colour = Taille, tooltip = paste(Taille, round(Moyenne, 1), sep = "<br>"), data_id = Taille) +
      geom_vline_interactive(xintercept = 2010.5, color = "white", size = 2, aes(tooltip = textesUI[textesUI$id == "pruning_start", lang])) + # ou 2011.5 ??
      geom_line(aes(group = Taille)) +
      geom_point_interactive() +
      scale_color_manual(values = coul_taille[input$taille_multi]) +
      scale_x_continuous(breaks = seq(2008, 2018, by = 2)) +
      labs(
        x = NULL, y = NULL, 
        title = textesUI[textesUI$id == input$taille_mesure, lang],
        colour = textesUI[textesUI$id == "taille_legend", lang]
      )
  } } %>% 
    girafe(
      ggobj = ., 
      options = list(
        opts_hover_inv(css = "opacity:0;"),
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "stroke-width:3px;"),
        opts_selection(type = "none")
      )
    )
}



## mesures à l'échelle de la parcelle ####

# traduire tooltip OK
# vérifier valeurs extrêmes de poids moyen de fruit
# une ou deux lignes ?
# comment montrer l'année où on a commencé à tailler ? -> l'ajouter dans le texte ?

taille %>% 
  filter(Mesure == input$taille_mesure) %>%
  rowwise() %>% 
  mutate(Taille_trad = textesUI[textesUI$id == Taille, lang]) %>% 
  {ggplot(.) +
      aes(x = X, y = Y, fill = Valeur, tooltip = paste(Taille_trad, round(Valeur, 1), sep = "<br>"), data_id = Taille) +
      geom_tile_interactive(colour = "black") +
      scale_fill_gradientn(
        colours = c("#a40000",  "#de7500", "#ee9300", "#f78b28", "#fc843d", "#ff7e50", "#ff5d7a", "#e851aa", "#aa5fd3", "#0070e9"), 
        na.value = "transparent" # travailler encore le gradient de couleurs
      ) +
      # scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      coord_fixed() +
      labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$variete_mesure, lang], fill = NULL) +
      facet_wrap(~ Annee, nrow = 2)} %>% 
  girafe(
    ggobj = ., width_svg = 12,
    options = list(
      opts_hover_inv(css = "opacity:0.2;"),
      opts_tooltip(use_stroke = TRUE),
      opts_hover(css = ""),
      opts_selection(type = "single", css = "fill:black;")
    )
  )











# Evaluation variétale (VIEUX) ----------------------------------------------------

variete <- read_delim("data-raw/MA02.txt", locale = locale(encoding = "ISO-8859-1", decimal_mark = ",", grouping_mark = "")) %>%
  rename(Annee = annee)


# verif
skimr::skim(variete)

ftable(Annee ~ arbre, data = cultivar)

variete %>% 
  filter(masse == 0, nbfruit != 0)
variete %>% 
  filter(masse != 0, nbfruit == 0)



# chaque arbre récolté plusieurs fois sur la même année : aggrégation des données pour avoir une valeur par arbre et par année
variete_arbre_annee <- variete %>% 
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
  )

skimr::skim(variete_arbre_annee)

ftable(Annee ~ cultivar, data = variete_arbre_annee)



variete_mesure <- variete_arbre_annee %>% 
  tidyr::pivot_longer(masse:masse_fruit, names_to = "Mesure", values_to = "Valeur")





# couleurs_var <- c()


## plan
# représentation des bordures à valider

var_plan <- variete_mesure %>% 
  distinct(X, Y, cultivar) %>%
  ggplot() +
  aes(x = X, y = Y, fill = cultivar) + # ajouter textui pour variété
  geom_tile(color = "black") +
  scale_fill_viridis_d() + # revoir les couleurs
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  coord_fixed() +
  labs(x = NULL, y = NULL)

plotly::ggplotly(var_plan)


# autre méthode avec echarts ?
# légendes ?

variete_arbre_annee %>%
  distinct(X, Y, cultivar) %>%
  mutate(bidon = 1) %>%
  tidyr::pivot_wider(names_from = cultivar, values_from = bidon) |>
  e_charts(X) |>
  e_heatmap(Y, Heidi, name = "Heidi") |>
  e_heatmap(Y, José) |>
  e_heatmap(Y, Sensation) |>
  e_tooltip()



## Récapitulons

# input

mesure <- "masse"
mesure <- "nbfruit"
mesure <- "masse_fruit"



### Bar plot : comparaison variétale




t_params <- powerTransform(variete[variete$Mesure == input$variete_mesure, "Valeur"], family = "bcnPower")

variete %>% 
  filter(Mesure == input$variete_mesure) %>%
  mutate(Valeur_t = ifelse(!is.na(Valeur), bcnPower(Valeur, lambda = t_params$lambda, gamma = t_params$gamma), NA)) %>% # tranformation de variable
  lm(Valeur_t ~ factor(Annee) * cultivar, data = .) %>%
  ref_grid(at = list(Annee = input$variete_checkbox_year %>% as.numeric())) %>% # POURQUOI ?
  update(tran = make.tran("bcnPower", c(t_params$lambda, t_params$gamma))) %>% # prise en compte de la transformation pour estimer les moyennes marginales
  emmeans("cultivar", type = "response") %>% # estimation des moyennes marginales
  as_tibble() %>%
  mutate(response = round(response, 0)) %>% 
  e_charts(cultivar) %>% 
  e_bar(response, legend = FALSE) %>%
  e_labels(position = "inside") %>% 
  e_error_bar(lower.CL, upper.CL) %>%
  e_y_axis(formatter = e_axis_formatter(locale = lang)) |>
  e_x_axis(axisLabel = list(interval = 0, rotate = 25))  |>
  suppressMessages() |> suppressWarnings()
# e_flip_coords() |> # flip axis, ne marche pas avec les barres d'erreur
# e_title("Production annuelle moyenne par arbre (kg)", "en fonction de la variété et des années de récolte sélectionnées") %>% # textui
# e_tooltip(formatter = e_tooltip_item_formatter(locale = lang, digits = 1))






# faire modèle

annees <- 2010:2015
annees <- 2016

# à voir si mettre les modèles dans une iste, pour gagenr du temps à l'affichage ?
# Attention les 3 modèles pour les 3 mesures à vérifier !

t_params <- powerTransform(variete_arbre_annee[[mesure]], family = "bcnPower")

variete_mesure %>% 
  filter(Mesure == mesure) %>% # input
  mutate(Valeur_t = ifelse(!is.na(Valeur), bcnPower(Valeur, lambda = t_params$lambda, gamma = t_params$gamma), NA)) %>% # tranformation de variable
  lm(Valeur_t ~ factor(Annee) * cultivar, data = .) %>% # modèle A VERIFIER
  ref_grid(at = list(Annee = annees)) %>% # idée : at y mettre que les années concernées ?
  update(tran = make.tran("bcnPower", c(t_params$lambda, t_params$gamma))) %>% # prise en compte de la transformation pour estimer les moyennes marginales
  emmeans("cultivar", type = "response") %>% # estimation des moyennes marginales, idée : enlever le by année sur un jeu filtré pour faire avec plusieurs années
  as_tibble() %>%
  # filter(Annee == annees) |> # si input : une seule année
  e_charts(cultivar) %>% 
  e_bar(response, legend = FALSE, name = NA) %>%
  e_error_bar(lower.CL, upper.CL) %>%
  # e_mark_line(data = list(xAxis = cultivar)) %>% # input
  e_axis(axis = "y", formatter = e_axis_formatter(locale = "fr")) |>
  e_title("Production annuelle moyenne par arbre (kg)", "en fonction de la variété et des années de récolte sélectionnées") %>% # textui
  e_tooltip() |>
  e_flip_coords()



### Heat map : suivi spatial

# input
# seulement si 1 variété, pas d'année en input
variete_heatmap <- "Heidi"

variete_mesure %>%
  filter(Mesure == mesure) |> # input
  tidyr::pivot_wider(names_from = cultivar, values_from = Valeur) |> # pour garder toutes les lignes et colonnes de la parcelle
  arrange(as.numeric(as.character(Y)), desc(X)) |>
  rename(Selec = all_of(variete_heatmap)) |> # input
  group_by(Annee)|>
  e_charts(X, reorder = FALSE, timeline = TRUE) |>
  e_grid(left = "30%") |>
  e_heatmap(Y, Selec) |>
  e_visual_map(Selec) |>
  e_title(variete) # "Production annuelle par arbre (kg) de la variété XX"



tata <- variete %>%
  filter(Mesure == input$variete_mesure) |>
  tidyr::pivot_wider(names_from = cultivar, values_from = Valeur) |> # pour garder toutes les lignes et colonnes de la parcelle
  arrange(as.numeric(as.character(Y)), desc(X)) |>
  rename(Selec = all_of(input$variete_select_var)) |>
  group_by(Annee)|>
  e_charts(X, reorder = FALSE, timeline = TRUE) |>
  e_grid(left = "30%") |>
  e_heatmap(Y, Selec) |>
  e_visual_map(Selec) # |>
# e_title(variete) # "Production annuelle par arbre (kg) de la variété XX"

toto <- lapply(unique(variete$Annee), function(an) {
  variete %>%
    filter(Mesure == input$variete_mesure, Annee == an) |>
    tidyr::pivot_wider(names_from = cultivar, values_from = Valeur) |> # pour garder toutes les lignes et colonnes de la parcelle
    arrange(as.numeric(as.character(Y)), desc(X)) |>
    rename(Selec = all_of(input$variete_select_var)) |>
    e_charts(X, reorder = FALSE) |>
    e_grid(left = "30%") |>
    e_heatmap(Y, Selec) |>
    e_visual_map(Selec) |> 
    e_title(an)
}) %>% 
  e_arrange(ids = ., rows = 2) # title



### lines :  Suivi temporel


# pourrait se mettre en eventReactive pour optimiser
t_params <- powerTransform(variete[variete$Mesure == input$variete_mesure, "Valeur"], family = "bcnPower")

variete %>% 
  filter(Mesure == input$variete_mesure) %>% # input
  mutate(Valeur_t = ifelse(!is.na(Valeur), bcnPower(Valeur, lambda = t_params$lambda, gamma = t_params$gamma), NA)) %>% # tranformation de variable
  lm(Valeur_t ~ factor(Annee) * cultivar, data = .) %>% # modèles vérifiés préalablement
  ref_grid(at = list(cultivar = input$variete_multi_var)) %>%
  update(tran = make.tran("bcnPower", c(t_params$lambda, t_params$gamma))) %>% # prise en compte de la transformation pour estimer les moyennes marginales
  emmeans("Annee", by = "cultivar", type = "response") %>% # estimation des moyennes marginales
  as_tibble() %>%
  mutate(Annee = factor(Annee)) |>
  # filter(cultivar == cultivar) |> # input
  group_by(cultivar) |>
  e_charts(Annee) %>% 
  e_line(response) %>%
  {ifelse(length(input$variete_multi_var) == 1, e_band(., lower.CL, upper.CL), .)} %>% 
  # e_band(lower.CL, upper.CL) %>% # à ajouter si une seule variété cochée
  e_axis(axis = "y", formatter = e_axis_formatter(locale = lang)) |>
  e_tooltip(trigger = "axis", formatter = e_tooltip_pointer_formatter(locale = lang, digits = 0)) #%>% 
# e_mark_line(data = list(xAxis = cultivar)) %>% # si on arrivait à mettre l'année en train d'être visualisée dans les timeline ?
# e_title(cultivar)




varietes <- "Heidi"
varietes <- c("Heidi", "Irwin", "José")


variete_mesure %>% 
  filter(Mesure == mesure) %>% # input
  mutate(Valeur_t = ifelse(!is.na(Valeur), bcnPower(Valeur, lambda = t_params$lambda, gamma = t_params$gamma), NA)) %>% # tranformation de variable
  lm(Valeur_t ~ factor(Annee) * cultivar, data = .) %>% # modèles vérifiés préalablement
  ref_grid(at = list(cultivar = varietes)) %>%
  update(tran = make.tran("bcnPower", c(t_params$lambda, t_params$gamma))) %>% # prise en compte de la transformation pour estimer les moyennes marginales
  emmeans("Annee", by = "cultivar", type = "response") %>% # estimation des moyennes marginales
  as_tibble() %>%
  mutate(Annee = factor(Annee)) |>
  # filter(cultivar == cultivar) |> # input
  group_by(cultivar) |>
  e_charts(Annee) %>% 
  e_line(response) %>%
  # e_band(lower.CL, upper.CL) %>% # à mettre si une seule variété cochée
  e_axis(axis = "y", formatter = e_axis_formatter(locale = "fr")) |>
  e_tooltip(trigger = "axis", formatter = e_tooltip_pointer_formatter(locale = "fr", digits = 0)) #%>% 
# e_mark_line(data = list(xAxis = cultivar)) %>% # si on arrivait à mettre l'année en train d'être visualisée dans les timeline ?
# e_title(cultivar)

# ajouter la ligne horizontale pour une variété cochée





## travail en cours



### spatial


# Demander de choisir la variété
cultivar <- "Heidi"

# Demander de choisir si production, nb fruits ou masse moyenne des fruits
mesure <- "masse" # ou nbfruit ou masse_fruit




# donc ordre des étapes (A FAIRE)
# pivot
# filter (pareil quelle que soit la langue ?)
# rename title only



e2 <- variete_arbre_annee %>%
  # dplyr::rename(
  #   `Production annuelle (kg)` = masse,
  #   `Nombre de fruits` = nbfruit,
  #   `Masse moyenne d'un fruit` = masse_fruit
  #   ) |>
  # tidyr::pivot_longer(c("Production annuelle (kg)", "Nombre de fruits", "Masse moyenne d'un fruit"), names_to = "Mesure", values_to = "Valeurs") |> # textui
  tidyr::pivot_longer(masse:masse_fruit, names_to = "Mesure", values_to = "Valeurs") |>
  # filter(cultivar == cultivar, Mesure == mesure) |> # input
  filter(Mesure == mesure) |> # input
  tidyr::pivot_wider(names_from = cultivar, values_from = Valeurs) |> # pour garder toutes les ligens et colonnes de la parcelle
  arrange(as.numeric(as.character(Y)) %>% desc()) |>
  rename(Selec = cultivar) |> # input
  group_by(Annee) |>
  e_charts(X, timeline = TRUE, reorder = FALSE,) |>
  e_grid(left = "30%") |>
  e_heatmap(Y, Selec) |>
  e_visual_map(Selec) |>
  e_title(cultivar) |> # textui pour title ; input
  # e_tooltip() |>
  e_connect("e1")











### temporel




#### Vérification des modèles

mesure <- "masse"
mesure <- "nbfruit"
mesure <- "masse_fruit"


t_params <- powerTransform(variete_arbre_annee[[mesure]], family = "bcnPower")

variete_filtre <- variete_mesure %>% 
  filter(Mesure == mesure) %>% # input
  mutate(Valeur_t = ifelse(!is.na(Valeur), bcnPower(Valeur, lambda = t_params$lambda, gamma = t_params$gamma), NA)) # tranformation de variable

mod_variete <-  lm(Valeur_t ~ factor(Annee) * cultivar, data = variete_filtre)


plot(mod_variete)

anova(mod_variete)
Anova(mod_variete)

variete_filtre %>% 
  modelr::add_predictions(mod_variete) %>% 
  # group_by(cultivar, Annee) %>%
  yardstick::mae(Valeur_t, pred) # rsq

broom::glance(mod_variete)

variete_filtre %>% 
  modelr::add_predictions(mod_variete) %>% 
  ggplot() +
  aes(Valeur_t, pred) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point()











#### comparaisons multiples


ref_grid(mod_var_masse)
ref_grid(mod_var_masse, cov.reduce = range)


# pour un nombre fixé de fruits (à voir si ça change avec le nb de fruits ?)

# compare var pour chaque année
comp_mass_var_annee <- emmeans(
  mod_var_masse, 
  "cultivar", 
  by = "Annee", 
  type = "response" 
  # cov.reduce = range
) #%>% plot(comparisons = TRUE)

# emmip(mod_var_masse, cultivar ~ Annee, CIs = TRUE, type = "response", cov.reduce = range)


# A FAIRE : mettre dans le même tableau les résultats des 3 mesures (masse, nbfruit et masse_fruit) pour faire choisir l'utilisateur
# highlight la variété choisie ? et lier le graphic avec e_connect()

e1 <- confint(comp_mass_var_annee) %>%
  as_tibble() %>% # pas obligatoire
  # filter(Mesure == mesure) %>%  # input
  arrange(Annee, response) |>
  group_by(Annee) %>%
  e_charts(cultivar, reorder = FALSE, timeline = TRUE, elementId = "e1") %>% 
  e_bar(response, legend = FALSE, name = NA) %>%
  e_error_bar(lower.CL, upper.CL) %>%
  e_mark_line(data = list(xAxis = cultivar)) %>% # input
  e_title("Production annuelle moyenne par arbre (kg)", "en fonction de la variété et de l'année de récolte") %>% # textui
  e_tooltip() |>
  e_y_axis(max = round(max(confint(comp_mass_var_annee)$upper.CL), digits = -4)) # pertinence ?


# a mettre ?
variete_arbre_annee %>% 
  group_by(Annee) %>%
  e_charts(cultivar, timeline = TRUE) |>
  # e_scatter(masse, symbol_size = 5) |>
  e_boxplot(masse)

variete_arbre_annee %>% 
  filter(Annee == 2015) %>% 
  group_by(cultivar) %>%
  e_charts() |>
  # e_scatter(masse, symbol_size = 5) |>
  e_boxplot(masse)



# A FAIRE : mettre dans le même tableau les résultats des 3 mesures (masse, nbfruit et masse_fruit) pour faire choisir l'utilisateur
# et lier le graphique avec e_connect() ?

# compare les années pour chaque var
comp_mass_annee_var <- emmeans(mod_var_masse, "Annee", by = "cultivar", type = "response"
                               # , cov.reduce = range
)

e3 <- confint(comp_mass_annee_var) %>%
  as_tibble() %>%
  mutate(Annee = factor(Annee)) |>
  filter(cultivar == cultivar) |> # input
  e_charts(Annee) %>% 
  e_line(response, legend = FALSE) %>%
  e_band(lower.CL, upper.CL) %>%
  e_tooltip() %>% 
  # e_mark_line(data = list(xAxis = cultivar)) %>% # si on arrivait à mettre l'année en train d'être visualisée dans les timeline ?
  e_title(cultivar) # attention pour mesure ; input




# emmip(mod_var_masse, cultivar ~ Annee, CIs = TRUE, type = "response", cov.reduce = range)
# emmip(mod_var_masse, Annee ~ cultivar, CIs = TRUE, type = "response", cov.reduce = range)
# emmip(mod_var_masse, ~ cultivar, CIs = TRUE, type = "response", cov.reduce = range) # ???
# 
# emtrends(mod_var_masse, "cultivar", by = "Annee", var = "sqrt(nbfruit)") %>% plot(comparisons = TRUE)
# emtrends(mod_var_masse, ~ cultivar, var = "nbfruit") %>% plot(comparisons = TRUE)
# correspond(ait) au poids estimé en g d'un fruit


e_arrange(e1, e_arrange(e2, e3, cols = 2))





# facteur taille (VIEUX) ####

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
    Taille = taille %>% fct_recode("taille en été" = "taille02", "taille en hiver" = "taille07" ,"sans taille" ="tem"),
    bloc = factor(bloc),
    X = str_sub(arbre, end = 1) %>% factor(levels = LETTERS[11:1]),
    Y = str_sub(arbre, start = 2) %>% factor(levels = 1:17)
  ) %>% 
  tidyr::pivot_longer(masse:masse_fruit, names_to = "Mesure", values_to = "Valeur")


coul_taille <- c(`taille en été` = "darkgreen", `taille en hiver` = "darkblue", `sans taille` = "darkred")

# plan
# représentation des bordures à valider

taille %>% 
  distinct(arbre, bloc, Taille) %>%
  ggplot() +
  aes(x = X, y = Y, fill = Taille) +
  geom_tile(color = "black") +
  scale_fill_manual(values = coul_taille) + # revoir les couleurs
  coord_fixed() +
  scale_y_discrete(drop = FALSE) +
  labs(x = NULL, y = NULL) 









# chaque arbre récolté plusieurs fois sur la même année : aggrégation des données pour avoir une valeur par arbre et par année
taille_arbre_annee <- taille %>% 
  filter(taille != "bordure") %>% 
  group_by(arbre, Taille, bloc, Annee) %>% 
  summarise(
    masse = sum(masse, na.rm = TRUE),
    nbfruit = sum(nbfruit)
  ) %>% 
  ungroup() %>% 
  mutate(
    masse_fruit = masse / nbfruit, # masse moyenne des fruits par arbre
    Taille = factor(Taille) # mise à jour des modalités
  )

skimr::skim(taille_arbre_annee)

ftable(bloc ~ Taille, data = taille_arbre_annee)
ftable(Annee ~ Taille, data = taille_arbre_annee)



# Attention la taille des arbres a commencé entre 2011 et 2012

ggplot(taille_arbre_annee) +
  aes(x = Annee, y = nbfruit, group = arbre) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Taille)
ggplot(taille_arbre_annee) +
  aes(x = Taille, y = nbfruit, color = bloc) +
  geom_point(alpha = 0.5, position = position_dodge(width = 0.2)) +
  facet_wrap(~ Annee)

ggplot(taille_arbre_annee) +
  aes(x = Annee, y = masse, group = arbre) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Taille)
ggplot(taille_arbre_annee) +
  aes(x = Taille, y = masse, color = bloc) +
  geom_point(alpha = 0.5, position = position_dodge(width = 0.2)) +
  facet_wrap(~ Annee)

ggplot(taille_arbre_annee) +
  aes(x = Annee, y = masse_fruit, group = arbre) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Taille)
ggplot(taille_arbre_annee) +
  aes(x = Taille, y = masse_fruit, color = bloc) +
  geom_point(alpha = 0.5, position = position_dodge(width = 0.2)) +
  facet_wrap(~ Annee)


ggplot(taille_arbre_annee) +
  aes(x = nbfruit, y = masse, color = Annee) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Taille)
ggplot(taille_arbre_annee) +
  aes(x = nbfruit, y = masse, color = Taille) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Annee)


# # modele masse
# 
# mod_taille_masse <- lmer(masse ~ nbfruit * factor(Annee) * Taille + (1|bloc), data = taille_arbre_annee)
# 
# ranova(mod_taille_masse) # effet bloc
# Anova(mod_taille_masse)
# 
# ref_grid(mod_taille_masse)
# 
# emmeans(mod_taille_masse, "Taille", by = "Annee") %>% plot(comparisons = TRUE)
# emmip(mod_taille_masse, Taille ~ Annee, CIs = TRUE) +
#   coord_cartesian(ylim = c(0, 100000))
# emmip(mod_taille_masse, Annee ~ Taille, CIs = TRUE)
# 
# emtrends(mod_taille_masse, "Taille", by = "Annee", var = "nbfruit") %>% plot(comparisons = TRUE)
# emtrends(mod_taille_masse, ~ Taille, var = "nbfruit") %>% plot(comparisons = TRUE)
# 
# 
# # modele nombre de fruits
# 
# mod_taille_nb <- lmer(nbfruit ~ factor(Annee) * Taille * (1|bloc), data = taille_arbre_annee)
# 
# ranova(mod_taille_nb) # effet bloc
# Anova(mod_taille_nb)
# 
# ref_grid(mod_taille_nb)
# 
# emmeans(mod_taille_nb, "Taille", by = "Annee") %>% plot(comparisons = TRUE)
# emmip(mod_taille_nb, Taille ~ Annee, CIs = TRUE) +
#   scale_colour_manual(values = couleurs_taille)
# emmip(mod_taille_nb, Annee ~ Taille, CIs = TRUE)








