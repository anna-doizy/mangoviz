# Note pour plus tard
# dashboardHeader(disable = TRUE) ?
# ou plutôt mettre l'accueil et à propos dans des boîtes et enlever mon css foireux (pour la prochaine appli plutôt !)


# ERREUR : Version de l'API graphiques incompatible
# FIX : update.packages(ask = FALSE, checkBuilt = TRUE) # attention c'est long !



# PUBLICATION

# format de citation de l'appli à réfléchir
# les données dans un datapaper -> mettre le dépôt en public à ce moment
# symposium mangue oct-nov 2023 Espagne
# quand héberger dans le serveur du CIRAD ?
# données à déposer dans dataverse CIRAD : à citer (Fred)

# Guillaume : est-ce qu'on peut mettre l'appli en open et les données en privé ? Sinon tout en privé

# mettre en doc (README) la structure des données qu'il faut, s'inspirer de data.R

# si demande du code source alors donner qu'une partie des données (quelques arbres et 2 ans et 2 modalités (taille ou var)) -> minimum fonctionnel

# garder en tete possible ajout des valeurs brix pH etc. (données au niveau du fruit)

# obj : bêta tests d'ici la fin de l'année
# textes avec traduction : d'ici le 14 juillet



# SUIVI des modifications ####


# RDV 2022-09-26

# police sans serif par défault A VERIFIER avec Isabelle
# téléchargement graphique svg ou png bonne qualité ou eps ! POUR L'INSTANT NON
# essai taille : passer les 2 première boites en layout vertical ou bien faire le même layout que pour essai variétal ? A VOIR AVEC LES TEXTES ET IMAGES DE FRED



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
# changer couleur du checkbox_year (le rouge est pour les bandes de titre) OK
# changer les couleurs des var par celles de Isabelle OK MAIS PALICHON SUR LES GRAPHIQUES
# ajouter des helpers OK
# couleur du gradient : tester https://personal.sron.nl/~pault/#fig:scheme_iridescent : OK
# ajouter possibilité d'afficher toutes les variétés ensemble (avec un switch ?) OK
# revoir échelle de couleurs, les mettre en classe éventuellement OK
# selection de chaque cultivar OK
# faire un graphe spatial global OK
# traduire tooltip OK

# 2022-07-19
# logo : attendre photo de Fred ? (choix de la première police OK)
# faire un graphique spatial global comme pour cultivar OK
# vérifier valeurs extrêmes de poids moyen de fruit OK
# Confirmer les années de taille des arbres OK
# ajouter boite modalité de taille avec calendrier des tailles des récoltes dans un graphique OK
# - mettre les sécateurs à la bonne échelle OK
# - aligner les sécateurs sur le cycle de taille OK
# - marquer le passage des années OK
# - mettre les années au niveau du milieu de l'année OK
# - remplacer les années par N, N+1, etc. OK
# - intégrer le graphique cycle des tailles dans l'appli
# 2022-07-20
# intégrer les corrections textesUI, locale d'Isabelle
# déboguer les labels du graphe taille temporel
# 2022-07-21 
# intégrer les corrections des helpfiles d'Isabelle
# 1er graphique : interaction sur les points noirs avec dataid : arbre OK
# mettre en label le nombre de fruits qui ont permis de calculer les moyennes OK
# ajouter carte réunion avec un point et les coordonnées des vergers OK
# 2022-07-26
# Ajouter les arbres de bordure sur les plans OK
# mettre une croix en F12 sur le plan de l'essai taille OK
# 2022-07-28 
# mettre le logo dans la navbar OK
# passer en selectInput pour tout les graphes temp et spatial plutôt qu'en radio OK
# Améliorer couleur du bouton "tout sélectionner" OK
# 2022-08-02
# Mise à jour des noms des Font Awesome icons (+2 issues sur github) OK
# Responsiveness : régler l'emplacement des éléments de l'ui et la taille du texte des graphiques à partir d'une résolution d'écran de 1280 x 720 (recommandé par les dev ; ctrl + maj + M, vue adaptative dans mozilla)

# 2022-10-11
# présentation : inverser axe I -> A pour les 2 essais OK
# plan satelite et mise à jour des coordonnées OK
# pour le graphe cycles de tailles :
# - remplacer N0 N1 par Année 0 année 1 et les mois par leur initiales ? OK
# - un petit trait vertical sous 01 ? OK
# - ajouter mention taille en été/en hiver au-dessus des sécateurs OK
# changer les couleurs de taille (plus doux) Ok
# préciser le calcul des visualisations globales (moyenne) dans le titre du bouton : "moyenne sur l'ensemble des années" au lieu de "globale" OK
# mettre une autre palette de couleurs pour les graphiques globaux pour montrer les différences d'échelle OK
# taille 2eme graphique : pointillés trait vertical entre 2011 & 2012 pour indiquer le début de la taille. OK Ajouter mention dans l'aide OK

# 2022-11-17 : correction finale du texte en français

# 2022-11-23 :
# mettre à jour photos des variétés
# enlever le texte de description des variétés
# ajouter les données jusqu'en 2021 + vérification des bugs, mise à jour des scripts en fonction
# remettre palette de couleur cohérente
# ajouter photo verger taille
# mettre en place le setup pour la traduction espagnole


# 2022-12-02
# inverser photo et carte verger taille
# remettre palette de couleur jaune
# Ajout de 2 boutons "tout sélectionner" pour les graphiques temp et spatial du verger taille

# 2023-02-13
# alléger et intégrer les nouvelles fiches variétales
# ajouter des données (0) des arbres manquants 
# harmoniser mise en page présentation des essais
# Modifier le graphique 'Comparaison des modalités de taille' en enlevant 2011 dans les dates sélectionnées par défaut
# Corriger le calcul du nombre de fruits pris en compte dans les calculs dans les 2 graphiques 'Suivi temporel' + préciser signification de n dans l'aide


# 2023-05-03 et 2023-05-16 (intégration des retours des testeurs)
# bande de logos institutionnels sur fond blanc dans l'accueil, sans lien
# cliquer sur un lien externe ouvre maintenant un nouvel onglet
# ajouter un outil de tracking (plausible)
# mettre un bouton pour télécharger les 10 fiches variété en pdf
# harmoniser les boutons de sélection des variétés et des modalités de taille (checkboxgroupbutton)
# Ajouter boutons pour tout sélectionner et déselectionner
# Modifier couleur pour que ce soit cohérent avec les autres sélections





# A faire : 

# Choisir ce qui est sélectionné par défaut (1ère année, 1ère modalité ?)
# mise en forme des textes d'aide (une fois que le français est validé)
# en attente d'une plus jolie photo de mangue pour le logo (sans flash)
# en attente des textes de l'accueil (Fred)
# ménage des commentaires et des fichiers devenus inutiles
# publication (code privé d'ici au symposium) sur le serveur du CIRAD
# mettre le pdf sur un autre dépôt ? demander à Guillaume


####


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
  # variete_multi_var = c("Heidi", "Irwin", "José"),
  variete_multi_var = "Heidi", # plusieurs pour temp
  variete_select_var = "all",
  # variete_select_var = "Heidi", # une seule pour spatial
  variete_all_year = TRUE # switch
)


coul_var <- c("#b94137", "#0080b4", "#008355", "#7f4ecc", "#ce7e26", "#8aa543", "#56423e", "#be4b7f", "#a5af98", "#00c1ff", "#0300000C") %>% setNames(c(levels(variete$cultivar), "bordure"))


## Plan de la parcelle ####

variete %>% 
  distinct(X, Y, cultivar) %>%
  bind_rows(
    tibble(
      cultivar = "bordure",
      X = c(rep("K",17), rep(LETTERS[10:2], each = 2), rep("A",17)),
      Y = c(1:17, rep(c(1,17), times = 9), 1:17) %>% factor()
    )
  ) %>%
  mutate(cultivar_trad = ifelse(cultivar == "bordure", textesUI[textesUI$id == "bordure", lang], cultivar)) %>% 
  {ggplot(.) +
      aes(x = X, y = Y, fill = cultivar, tooltip = cultivar_trad, data_id = cultivar) +
      geom_tile_interactive(color = "black") +
      annotate(geom = "point", x = "F", y = "12", shape = 4, size = 10) +
      scale_fill_manual(
        values = coul_var, labels = c(bordure = textesUI[textesUI$id == "bordure", lang]),
        guide = guide_legend(byrow = TRUE)) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      coord_fixed() +
      labs(x = NULL, y = NULL, fill = textesUI[textesUI$id == "cultivar", lang])} %>% 
  girafe(
    ggobj = ., height_svg = 8,
    options = list(
      opts_hover_inv(css = "opacity:0.2;"),
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
      aes(x = cultivar, y = Valeur, fill = cultivar, label = arbre) +
      geom_violin(alpha = 0.3, color = "transparent", scale = "count") +
      geom_jitter_interactive(alpha = 0.3, width = 0.2, height = 0, aes(tooltip = paste(..label.., round(..y.., 1), sep = "<br>"), data_id = arbre)) +
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
        Moyenne = mean(Valeur, na.rm = TRUE), n = n()
      ) %>%
      suppressMessages() %>% # group message
      # suppressWarnings() %>% # NA & NaN values
      ggplot() +
      aes(x = Annee, y = Moyenne, colour = cultivar, tooltip = paste(cultivar, "<br>", round(Moyenne, 1), "(n=", n, ")"), data_id = cultivar) +
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
{if(input$variete_all_year) {
  variete %>% 
    filter(
      Mesure == input$variete_mesure, 
      cultivar %in% if(input$variete_select_var == "all") {levels(variete$cultivar)} else {input$variete_select_var}
    ) %>% 
    group_by(X, Y, cultivar) %>% 
    summarise(Moyenne = mean(Valeur, na.rm = TRUE), n = n()) %>% 
    suppressMessages() %>% # group message
    ggplot() +
      aes(x = X, y = Y, fill = Moyenne, tooltip = paste(cultivar, "<br>", round(Moyenne, 1), "(n=", n, ")"), data_id = cultivar) +
      geom_tile_interactive(colour = "black") +
      # scale_fill_gradientn(
        # colours = c("#a40000",  "#de7500", "#ee9300", "#f78b28", "#fc843d", "#ff7e50", "#ff5d7a", "#e851aa", "#aa5fd3", "#0070e9"),
        # colours = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF', '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1', '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD', '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388', '#805770', '#684957', '#46353A'),
      #   na.value = "transparent" # https://personal.sron.nl/~pault/#fig:scheme_iridescent
      # ) +
      scale_fill_distiller(palette = "YlOrRd", na.value = "transparent", direction = 1) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      coord_fixed() +
      labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$variete_mesure, lang], fill = NULL)
  } else {
    variete %>% 
      filter(
        Mesure == input$variete_mesure, 
        cultivar %in% if(input$variete_select_var == "all") {levels(variete$cultivar)} else {input$variete_select_var}
      ) %>%
    ggplot() +
      aes(x = X, y = Y, fill = Valeur, tooltip = paste(cultivar, round(Valeur, 1), sep = "<br>"), data_id = cultivar) +
      geom_tile_interactive(colour = "black") +
      scale_fill_gradientn(
        # colours = c("#a40000",  "#de7500", "#ee9300", "#f78b28", "#fc843d", "#ff7e50", "#ff5d7a", "#e851aa", "#aa5fd3", "#0070e9"), 
        colours = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF', '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1', '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD', '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388', '#805770', '#684957', '#46353A'),
        na.value = "transparent" # https://personal.sron.nl/~pault/#fig:scheme_iridescent
      ) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      coord_fixed() +
      labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$variete_mesure, lang], fill = NULL) +
      facet_wrap(~ Annee, nrow = 1)
  }}%>%
  girafe(
    ggobj = ., width_svg = 16,
    options = list(
      opts_hover_inv(css = "opacity:0.2;"),
      opts_tooltip(use_stroke = TRUE),
      opts_hover(css = ""),
      opts_selection(type = "none")
    )
  )
  

  





# TAILLE ####




coul_taille <- c(`taille_ete` = "darkgreen", `taille_hiver` = "darkblue", `taille_sans` = "darkred", bordure = "#0300000C")

input <- list(
  # taille_mesure = "masse",
  # taille_mesure = "nbfruit",
  taille_mesure = "masse_fruit",
  taille_checkbox_year = 2011:2018,
  # taille_checkbox_year = 2016,
  # taille_multi = c("taille_ete", "taille_hiver", "taille_sans")
  taille_multi = "taille_ete", # pour temp
  # taille_select = "all",
  taille_select = "taille_ete", # pour spatial
  taille_all_year = FALSE # switch
)


# Attention la taille des arbres a commencé entre 2011 et 2012
# que signifie taille en été/en hiver par rapport à l'année de récolte ?


## plan de la parcelle ####
# représentation des bordures à valider
# expliquer les bloc
# traduire label des bloc

taille %>% 
  distinct(X, Y, arbre, bloc, Taille) %>%
  bind_rows(
    tibble(
      arbre = textesUI[textesUI$id == "bordure", lang],
      bloc = "bordure",
      Taille = "bordure",
      X = rep(LETTERS[9:1], each = 2),
      Y = rep(c(1,17), times = 9) %>% factor()
    )
  ) %>%
  {ggplot(.) +
      aes(x = X, y = Y, fill = Taille, data_id = Taille, tooltip = arbre) +
      # geom_tile_interactive(color = "black", aes(label = arbre, tooltip = paste(..label.., textesUI[textesUI$id == "taille_ete", lang], sep = "<br>"))) + # ne marche pas
      geom_tile_interactive(color = "black") +
      scale_fill_manual(
        values = coul_taille, labels = textesUI[textesUI$id %in% c(levels(taille$Taille), "bordure"), lang] %>% setNames(c(levels(taille$Taille), "bordure")),
        guide = guide_legend(byrow = TRUE)
      ) +
      # scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      coord_fixed() +
      labs(x = NULL, y = NULL, fill = textesUI[textesUI$id == "taille_legend", lang])} %>% 
  girafe(
    ggobj = ., height_svg = 8, width_svg = 8,
    options = list(
      opts_hover_inv(css = "opacity:0.2;"),
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
      geom_jitter_interactive(alpha = 0.3, width = 0.2, height = 0, aes(tooltip = paste(..label.., round(..y.., 1), sep = "<br>"), data_id = arbre)) +
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
    ) %>% 
    suppressWarnings()
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


{if(input$taille_all_year) {
  taille %>% 
    filter(
      Mesure == input$taille_mesure, 
      Taille %in% if(input$taille_select == "all") {levels(taille$Taille)} else {input$taille_select}
    ) %>% 
    group_by(X, Y, Taille) %>% 
    summarise(Moyenne = mean(Valeur, na.rm = TRUE)) %>% 
    suppressMessages() %>% # group message
    rowwise() %>% 
    mutate(Taille_trad = textesUI[textesUI$id == Taille, lang]) %>% 
    ggplot() +
    aes(x = X, y = Y, fill = Moyenne, tooltip = paste(Taille_trad, round(Moyenne, 1), sep = "<br>"), data_id = Taille) +
    geom_tile_interactive(colour = "black") +
    scale_fill_gradientn(
      # colours = c("#a40000",  "#de7500", "#ee9300", "#f78b28", "#fc843d", "#ff7e50", "#ff5d7a", "#e851aa", "#aa5fd3", "#0070e9"), 
      colours = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF', '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1', '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD', '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388', '#805770', '#684957', '#46353A'),
      na.value = "transparent" # https://personal.sron.nl/~pault/#fig:scheme_iridescent
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    coord_fixed() +
    labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$taille_mesure, lang], fill = NULL)
} else {
  taille %>% 
    filter(
      Mesure == input$taille_mesure, 
      Taille %in% if(input$taille_select == "all") {levels(taille$Taille)} else {input$taille_select}
    ) %>%
    rowwise() %>% 
    mutate(Taille_trad = textesUI[textesUI$id == Taille, lang]) %>% 
    ggplot() +
    aes(x = X, y = Y, fill = Valeur, tooltip = paste(Taille_trad, round(Valeur, 1), sep = "<br>"), data_id = Taille) +
    geom_tile_interactive(colour = "black") +
    scale_fill_gradientn(
      # colours = c("#a40000",  "#de7500", "#ee9300", "#f78b28", "#fc843d", "#ff7e50", "#ff5d7a", "#e851aa", "#aa5fd3", "#0070e9"), 
      colours = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF', '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1', '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD', '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388', '#805770', '#684957', '#46353A'),
      na.value = "transparent" # https://personal.sron.nl/~pault/#fig:scheme_iridescent
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    coord_fixed() +
    labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$taille_mesure, lang], fill = NULL) +
    facet_wrap(~ Annee, nrow = 2)
}}%>%
  girafe(
    ggobj = ., width_svg = 16,
    options = list(
      opts_hover_inv(css = "opacity:0.2;"),
      opts_tooltip(use_stroke = TRUE),
      opts_hover(css = ""),
      opts_selection(type = "none")
    )
  )






# Cycle manguier ----------------------------------------------------------

lang <- "fr"

library(tidyverse)
library(lubridate)
# library(scales)
library(ggimage)
library(mangoviz)
# library(png)
# library(grid)

# img <- png::readPNG("inst/app/www/shears.png")
# rasterGrob(img)


date_taille <- tibble(
  Taille = rep(c("taille_hiver", "taille_ete"), times = 2),
  Date_taille = c("2016-08-01", "2016-02-01", "2018-08-01", "2018-02-01") %>% as.Date(),
  Depart = c(4, 4, 2, 2), # sens et position de la flèche
  Pointe = Depart - 0.6,
  pos_img = Depart + 0.5,
  img = "inst/app/www/shears_ratio.png"
) %>% 
  rowwise() %>% 
  mutate(
    Taille = textesUI[textesUI$id == Taille, lang]
  )

cycle <- tribble(
  ~Debut,    ~Fin,                  ~Etape,
  "2015-08-01", "2016-05-01", "croissance_vegetative",
  "2016-05-01", "2016-07-01",                 "repos",
  "2016-07-01", "2016-10-01",             "floraison",
  "2016-10-01", "2016-12-15",      "croissance_fruit",
  "2016-12-15", "2017-02-01",        "maturite_fruit",
  "2016-08-01", "2017-05-01", "croissance_vegetative",
  "2017-05-01", "2017-07-01",                 "repos",
  "2017-07-01", "2017-10-01",             "floraison",
  "2017-10-01", "2017-12-15",      "croissance_fruit",
  "2017-12-15", "2018-02-01",        "maturite_fruit",
  "2017-08-01", "2018-05-01", "croissance_vegetative",
  "2018-05-01", "2018-07-01",                 "repos",
  "2018-07-01", "2018-10-01",             "floraison",
  "2018-10-01", "2018-12-15",      "croissance_fruit",
  "2018-12-15", "2019-02-01",        "maturite_fruit"
) %>% 
  mutate(
    Debut = as.Date(Debut),
    Fin = (ymd(Fin) - days(4)) %>% as.Date(),
    Cycle = rep(3:1, each = 5) %>% factor(),
    Etape = paste(rep(1:5, times = 3), Etape, sep = "_") %>% factor()
  )

date_labels <- tibble(
  pas = seq(as.Date("2015-06-01"), as.Date("2019-04-01"), "month"),
  pas_label = ifelse(
    lang == "sp" & month(pas) == 1, 
    "E",  # "enero" in spanish
    month(pas, label = TRUE, locale = "C") %>% as.character() %>% str_sub(end = 1) %>% str_to_upper()
  ),
  annee = paste(textesUI[textesUI$id == "annee", lang], year(pas) - 2015),
  # etiquette = ifelse(month(pas) == 7, paste(format(pas, "%m"), annee, sep = "\n"), format(pas, "%m"))
  etiquette = case_when(
    month(pas) == 7 ~ paste(pas_label, annee, sep = "\n"),
    month(pas) == 1 ~ paste(pas_label, "|", sep = "\n"),
    TRUE ~ pas_label
  )
)


ggplot(cycle) +
  aes(x = Debut, y = Cycle) +
  geom_vline(xintercept = seq(as.Date("2016-01-01"), as.Date("2019-01-01"), "year"), size = 2, color = "white") +
  geom_segment(
    aes(xend = Fin, yend = Cycle, colour = Etape),
    linewidth = 8
  ) +
  geom_segment(
    data = date_taille, 
    mapping = aes(x = Date_taille, xend = Date_taille, y = Depart, yend = Pointe), 
    arrow = arrow(length = unit(0.1, "inches")),
    linewidth = 1, colour = "black"
  ) +
  geom_image(
    data = date_taille, 
    mapping = aes(x = Date_taille, y = pos_img, image = img), 
    size = 0.05
  ) +
  geom_text(
    data = date_taille, 
    mapping = aes(x = Date_taille, y = pos_img + 0.8, label = Taille)
  ) +
  scale_color_manual(
    labels = textesUI[textesUI$id %in% levels(cycle$Etape), lang] %>% setNames(levels(cycle$Etape)),
    values = c("#007510", "#47CBFF", "#FFC038", "#FF8800", "#DE3800") %>% setNames(levels(cycle$Etape))
  ) +
  scale_y_discrete(breaks = NULL) +
  scale_x_date(
    breaks = date_labels$pas, 
    minor_breaks = NULL, 
    labels = date_labels$etiquette
    ) +
  coord_fixed(70, ylim = c(0.8, 5.2)) +
  labs(x = NULL, y = "", colour = "") +
  theme(legend.position = "bottom", axis.text.x = element_text(face = "bold"))




# Carte RUN ---------------------------------------------------------------

library(leaflet)

leaflet() %>% 
  setView(55.4884, -21.3226, zoom = 18) %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addMarkers(55.4884, -21.3226)
# taille : -21.322638332348824, 55.488400916471825
# cultivar : -21.322763, 55.490350


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



# Import données 2022 ####

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


variete2022 <- read_csv2("data-raw/MA02 up to 2022.csv", locale = locale(encoding = "ISO-8859-1")) %>%
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


ftable(Annee ~ cultivar, data = variete)
ftable(Annee ~ cultivar, data = variete2022)

skimr::skim(variete2022)


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


taille2022 <- read_csv2("data-raw/MA05 up to 2022.csv", locale = locale(encoding = "ISO-8859-1")) %>%
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

ftable(Annee ~ bloc + Taille, data = taille)
ftable(Annee ~ bloc + Taille, data = taille2022)

skimr::skim(taille2022)

