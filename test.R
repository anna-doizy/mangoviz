
# A FAIRE ####

# - travailler les couleurs et les échelles (à harmoniser entre les différentes espèces)
# - ratio des graphes
# - textui
# - format des nombres à checker en fonction de la langue
# - vérifier que le emmeans fait bien ce qu'il faut quand on met plusieurs années ensemble
# - limite supérieure des légendes à harmoniser
# - couleurs
# - axe des y de la carte
# - echelle carte qui commence à 0


# ajouter du loading ? e_show_loading()
# e_highlight
# e_labels ?
# e_mark_line(data = list(xAxis = 7), title = "Tortoise") # pour l'année de début de taille



# mail du 12 oct
# - je n'ai rempli que l'onglet Évaluation variétale,
# - ce n'est pas la peine de chercher à le mettre en anglais
# - je n'ai pas encore travaillé sur les textes des graphiques, leurs couleurs et les échelles des axes, mais j'ai réussi à formater les nombres correctement
# - il faut sélectionner au moins une variété pour que le dernier graphique fonctionne
# - je n'ai pas encore intégré les bandes de confiance et la ligne horizontale pour le dernier graphique dans le cas où une seule variété est sélectionnée



# rdv du 19 oct
# - mode d'emploi
# - passer les g en kg
# - ordre des ligne colonne du plan de parcelle à mettre comme suivi spatial
# - décimales dans les étiquettes du graphe 1 à enlever
# - arbre en facteur aléatoire ?
# - comparer les moyennes estimes aux moyennes observées
# - faire un graphique pour représenter l'étendue et pas l'intervalle de confiance des moyennes (pour tester)
# - logo : photo plus nette, trait plus fin, autre police (plu ronde et fine), couleur ?
# - format de citation à réfléchir
# -  dans présentation : photos des variétés, fiches variétales, photo aérienne de la parcelle, description du protocole et des valeurs mesurées
# - dans résultats : 
# 1/ comp variétale : barres horizontales
# 2/ suivi temporel : trouver un format de sélection qui prend moins de place
# 3/ suivi spatial : en bas, tester de repasser en ggplot avec un facet_wrap(~ Annee), ajouter possibilité d'afficher toutes les variétés ensemble
# - garder en tete possible ajout des valeurs brix pH etc. (données au niveau du fruit)








library(mangoviz)

lang <- "fr"



library(lme4)
library(lmerTest)
library(car)
library(emmeans)

library(readr)
library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
library(echarts4r)


# Evaluation variétale ----------------------------------------------------

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


## plan ####
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



## Récapitulons ####

# input

mesure <- "masse"
mesure <- "nbfruit"
mesure <- "masse_fruit"



### Bar plot : comparaison variétale ####
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
  e_tooltip()



### Heat map : suivi spatial ####

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




### lines :  Suivi temporel ####


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





## travail en cours ####



### spatial ####


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











### temporel ####



# ggplot(variete_arbre_annee) +
#   aes(x = Annee, y = nbfruit, group = arbre) +
#   geom_line(alpha = 0.2) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(~ cultivar)
# 
# ggplot(variete_arbre_annee) +
#   aes(x = Annee, y = masse, group = arbre) +
#   geom_line(alpha = 0.2) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(~ cultivar)
# ggplot(variete_arbre_annee) +
#   aes(x = cultivar, y = masse) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(~ Annee)
# 
# ggplot(variete_arbre_annee) +
#   aes(x = Annee, y = masse_fruit, group = arbre) +
#   geom_line(alpha = 0.2) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(~ cultivar)
# 
# ggplot(variete_arbre_annee) +
#   aes(x = nbfruit, y = masse, color = Annee) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(~ cultivar)
# ggplot(variete_arbre_annee) +
#   aes(x = sqrt(nbfruit), y = sqrt(masse), color = Annee) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(~ cultivar)



#### Vérification des modèles ####

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



  
  
  
  
  
  
  
  
#### comparaisons multiples ####
  
  
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






# Facteur taille ----------------------------------------------------------



taille <- read_delim("data-raw/MA05.txt", locale = locale(encoding = "ISO-8859-1", decimal_mark = ",", grouping_mark = "")) %>%
  mutate(
    Annee = str_sub(annee, end = 4) %>% as.numeric(),
    Taille = taille %>% fct_recode("taille en été" = "taille02", "taille en hiver" = "taille07" ,"sans taille" ="tem")
  )

ftable(Annee ~ arbre, data = taille)

taille %>% 
  filter(masse == 0, nbfruit != 0)
taille %>% 
  filter(masse != 0, nbfruit == 0)


# couleurs_taille_b <- c(bordure = "grey", `taille en été` = "darkgreen", `taille en hiver` = "darkblue", `sans taille` = "darkred")
couleurs_taille <- c(`taille en été` = "darkgreen", `taille en hiver` = "darkblue", `sans taille` = "darkred")

# plan
# représentation des bordures à valider

taille %>% 
  filter(taille != "bordure") %>% 
  distinct(arbre, bloc, Taille) %>%
  mutate(
    X = str_sub(arbre, end = 1),
    Y = str_sub(arbre, start = 2) %>% factor(levels = 17:1)
  ) %>% 
  ggplot() +
  aes(x = X, y = Y, fill = Taille) +
  geom_tile(color = "black") +
  scale_fill_manual(values = couleurs_taille) + # revoir les couleurs
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


# modele masse

mod_taille_masse <- lmer(masse ~ nbfruit * factor(Annee) * Taille + (1|bloc), data = taille_arbre_annee)

ranova(mod_taille_masse) # effet bloc
Anova(mod_taille_masse)

ref_grid(mod_taille_masse)

emmeans(mod_taille_masse, "Taille", by = "Annee") %>% plot(comparisons = TRUE)
emmip(mod_taille_masse, Taille ~ Annee, CIs = TRUE) +
  coord_cartesian(ylim = c(0, 100000))
emmip(mod_taille_masse, Annee ~ Taille, CIs = TRUE)

emtrends(mod_taille_masse, "Taille", by = "Annee", var = "nbfruit") %>% plot(comparisons = TRUE)
emtrends(mod_taille_masse, ~ Taille, var = "nbfruit") %>% plot(comparisons = TRUE)


# modele nombre de fruits

mod_taille_nb <- lmer(nbfruit ~ factor(Annee) * Taille * (1|bloc), data = taille_arbre_annee)

ranova(mod_taille_nb) # effet bloc
Anova(mod_taille_nb)

ref_grid(mod_taille_nb)

emmeans(mod_taille_nb, "Taille", by = "Annee") %>% plot(comparisons = TRUE)
emmip(mod_taille_nb, Taille ~ Annee, CIs = TRUE) +
  scale_colour_manual(values = couleurs_taille)
emmip(mod_taille_nb, Annee ~ Taille, CIs = TRUE)








