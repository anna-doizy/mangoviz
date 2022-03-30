#### SERVER ####

server <- function(input, output, session) {
  
  # for local use : stop the server when the session ends
  is_local <- Sys.getenv('SHINY_PORT') == ""
  if(is_local) session$onSessionEnded(function() stopApp())


  # Observe -----------------------------------------------------------------
  
  # observe_helpers()

  ## Get language in the url ####
  lang <- dyn <- NULL

  observe(priority = 1000, {
    lang <<- isolate(getQueryString()$lang)

    if (is.null(lang) || !(lang %in% c("fr", "en"))) {
      lang <<- "fr"
    }

  })
  
  
  ## Plat themes ####
  
  theme_update(text = element_text(family = "sans"))
  
  
  
# Essai taille ------------------------------------------------------------
  
  # couleur des différents types de taille
  coul_taille <- c(`taille_ete` = "darkgreen", `taille_hiver` = "darkblue", `taille_sans` = "darkred")
  
  ## Plan de la parcelle ####
  
  output$taille_parcelle <- renderGirafe({
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
        ggobj = ., height_svg = 4, width_svg = 4,
        options = list(
          opts_hover_inv(css = "opacity:0.4;"),
          opts_tooltip(use_fill = TRUE),
          opts_hover(css = "fill:black;opacity:0.8;"),
          opts_selection(type = "none")
        )
      )
  })
  
  
  ## comparaison des tailles ####
  
  output$taille_taille <- renderGirafe({
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
  })
  
  
  ## comparaison des années (suivi temporel) ####
  
  output$taille_temporel <- renderGirafe({
    
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
          scale_color_manual(values = coul_taille[input$taille_multi], labels = textesUI[textesUI$id %in% levels(taille$Taille), lang] %>% setNames(levels(taille$Taille))) +
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
          ggplot() +
          aes(x = Annee, y = Moyenne, colour = Taille, tooltip = paste(Taille, round(Moyenne, 1), sep = "<br>"), data_id = Taille) +
          geom_vline_interactive(xintercept = 2010.5, color = "white", size = 2, aes(tooltip = textesUI[textesUI$id == "pruning_start", lang])) + # ou 2011.5 ??
          geom_line(aes(group = Taille)) +
          geom_point_interactive() +
          scale_color_manual(values = coul_taille[input$taille_multi], labels = textesUI[textesUI$id %in% levels(taille$Taille), lang] %>% setNames(levels(taille$Taille))) +
          scale_x_continuous(breaks = seq(2008, 2018, by = 2)) +
          labs(
            x = NULL, y = NULL, 
            title = textesUI[textesUI$id == input$taille_mesure, lang],
            colour = textesUI[textesUI$id == "taille_legend", lang]
          )
      } } %>% 
        suppressWarnings() %>% # geom_vline(): Ignoring `mapping` because `xintercept` was provided.
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
    
    
  })
  
  ## mesures à l'échelle de la parcelle ####
  
  output$taille_spatial <- renderGirafe({
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
          opts_selection(type = "none")
        )
      )
    
  })
  
  
  

# Evaluation variétale ----------------------------------------------------

  
  ## Présentation ####
  
  # Présentation des 10 variétés
  
  output$variete_ui_desc <- renderUI({
    textesUI[textesUI$id == input$variete_radio_desc, lang] %>% 
      markdown()
  })
  
  output$variete_img_desc <- renderImage({
    list(src = paste0("./www/varietes/", input$variete_radio_desc, ".JPG"))
  }, deleteFile = FALSE)
  
  # Bilan : fiches variétales
  output$variete_img_bilan <- renderImage({
    list(src = paste0("./www/fiches-varietales/", input$variete_radio_bilan, ".jpg"))
  }, deleteFile = FALSE)
  
  
  # Couleur des variétés
  coul_var <- c("#b94137", "#0080b4", "#008355", "#7f4ecc", "#ce7e26", "#8aa543", "#56423e", "#be4b7f", "#002853", "#00c1ff") %>% 
    setNames(unique(variete$cultivar))
  
  ## Plan de la parcelle ####

  output$variete_parcelle <- renderGirafe({
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
  })
  
  
  
  
  ## comparaison des variétés ####
  
  output$variete_var <- renderGirafe({
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
  })
  
  ## comparaison des années (suivi temporel) ####
  
  output$variete_temporel <- renderGirafe({

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
    
    
  })
  
  # titre : nb fruit moyen par arbre et par an
  # expliquer ce que sont les lignes noires
  
  
  ## mesures à l'échelle de la parcelle ####
  
  output$variete_spatial <- renderGirafe({
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

  })
  
  
  
} # end of server
