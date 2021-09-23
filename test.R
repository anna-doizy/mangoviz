# deployment to shinyapps.io with rsconnect

# SETUP
# git remote set-url --add --push origin git@github.com:anna-doizy/ipsimcirsium.git
# git remote set-url --add --push origin git@gitlab.com:doana-r/ipsimcirsium.git
# git remote -v
# origin  git@gitlab.com:doana-r/ipsimcirsium.git (fetch)
# origin  git@github.com:anna-doizy/ipsimcirsium.git (push)
# origin  git@gitlab.com:doana-r/ipsimcirsium.git (push)

# EACH TIME before deployment
# update all packages
# change "update date" in the ui
# check and install this package
# check if parse("inst/app/server.R"), parse("inst/app/ui.R") work
# commit & push
# restart R session
# remotes::install_github("anna-doizy/ipsimcirsium")
# publish the app

# nécessaire car comme l'application charge le package pour démarrer (récupère le jeu suivi), il a besoin d'être installé proprement dans le serveur distant. Pour l'instant RStudio (shinyapps) ne permet de faire cela qu'avec des packages classiques (CRAN) ou github, mais pas gitlab...




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


# Evaluation variétale ----------------------------------------------------

variete <- read_delim("data-raw/MA02.txt", locale = locale(encoding = "ISO-8859-1", decimal_mark = ",", grouping_mark = "")) %>% 
  mutate(
    Variete = factor(cultivar)
  ) %>% 
  rename(Annee = annee)

ftable(Annee ~ arbre, data = taille)

variete %>% 
  filter(masse == 0, nbfruit != 0)
variete %>% 
  filter(masse != 0, nbfruit == 0)



# couleurs_var <- c()


# plan
# représentation des bordures à valider

variete %>% 
  distinct(arbre, Variete) %>%
  mutate(
    X = str_sub(arbre, end = 1) %>% factor(levels = LETTERS[1:11]),
    Y = str_sub(arbre, start = 2) %>% factor(levels = 17:1)
  ) %>% 
  ggplot() +
  aes(x = X, y = Y, fill = Variete) +
  geom_tile(color = "black") +
  scale_fill_viridis_d() + # revoir les couleurs
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  coord_fixed() +
  labs(x = NULL, y = NULL) 



# chaque arbre récolté plusieurs fois sur la même année : aggrégation des données pour avoir une valeur par arbre et par année
variete_arbre_annee <- variete %>% 
  group_by(arbre, Variete, Annee) %>% 
  summarise(
    masse = sum(masse),
    nbfruit = sum(nbfruit)
  ) %>% 
  ungroup() %>% 
  mutate(
    masse_fruit = masse / nbfruit,
    Variete = fct_reorder(Variete, .x = masse, .fun = mean)
  )

skimr::skim(variete_arbre_annee)

ftable(Annee ~ Variete, data = variete_arbre_annee)


ggplot(variete_arbre_annee) +
  aes(x = Annee, y = nbfruit, group = arbre) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Variete)

ggplot(variete_arbre_annee) +
  aes(x = Annee, y = masse, group = arbre) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Variete)
ggplot(variete_arbre_annee) +
  aes(x = Variete, y = masse) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Annee)

ggplot(variete_arbre_annee) +
  aes(x = Annee, y = masse_fruit, group = arbre) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Variete)

variete_arbre_annee %>%
  ggplot() +
  aes(x = nbfruit, y = masse, color = Annee) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Variete)


# modele masse

powerTransform(variete_arbre_annee$masse, family = "bcnPower") %>% summary()
powerTransform(variete_arbre_annee$masse + 0.1) %>% summary()

# tran <- make.tran("boxcox", 0.33)
# mod_var_masse <- with(
#   tran, 
#   lm(masse ~ nbfruit * factor(Annee) * Variete, data = variete_arbre_annee)
# )

mod_var_masse <- lm(sqrt(masse) ~ sqrt(nbfruit) * factor(Annee) * Variete, data = variete_arbre_annee)


plot(mod_var_masse)

anova(mod_var_masse)
Anova(mod_var_masse)

variete_arbre_annee %>% 
  modelr::add_predictions(mod_var_masse) %>% 
  # group_by(Variete, Annee) %>%
  yardstick::mae(masse, pred^2) # rsq

broom::glance(mod_var_masse)

variete_arbre_annee %>% 
  modelr::add_predictions(mod_var_masse) %>% 
  ggplot() +
  aes(masse, pred^2) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point()



ref_grid(mod_var_masse)
ref_grid(mod_var_masse, cov.reduce = range)

# pour un nombre fixé de fruits (à voir si ça change avec le nb de fruits ?)
emmeans(mod_var_masse, "Variete", by = "Annee", type = "response", cov.reduce = range) %>% plot(comparisons = TRUE)
emmip(mod_var_masse, Variete ~ Annee, CIs = TRUE, type = "response", cov.reduce = range)
emmip(mod_var_masse, Annee ~ Variete, CIs = TRUE, type = "response", cov.reduce = range)
emmip(mod_var_masse, ~ Variete, CIs = TRUE, type = "response", cov.reduce = range) # ???

emtrends(mod_var_masse, "Variete", by = "Annee", var = "sqrt(nbfruit)") %>% plot(comparisons = TRUE)
emtrends(mod_var_masse, ~ Variete, var = "nbfruit") %>% plot(comparisons = TRUE)
# correspond(ait) au poids estimé en g d'un fruit





# modele nombre de fruits

powerTransform(variete_arbre_annee$nbfruit, family = "bcnPower") %>% summary()

mod_var_nb <- lm(sqrt(nbfruit) ~ factor(Annee) * Variete, data = variete_arbre_annee)

plot(mod_var_nb)

anova(mod_var_nb)
Anova(mod_var_nb)

variete_arbre_annee %>% 
  modelr::add_predictions(mod_var_nb) %>% 
  # group_by(Variete, Annee) %>% 
  yardstick::mae(nbfruit, pred^2) # rsq

variete_arbre_annee %>% 
  modelr::add_predictions(mod_var_nb) %>% 
  ggplot() +
  aes(nbfruit, pred^2) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point()

broom::glance(mod_var_nb)

ref_grid(mod_var_nb)

emmeans(mod_var_nb, "Variete", by = "Annee", type = "response") %>% plot(comparisons = TRUE)
emmip(mod_var_nb, Variete ~ Annee, CIs = TRUE, type = "response")
emmip(mod_var_nb, Annee ~ Variete, CIs = TRUE, type = "response")








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








