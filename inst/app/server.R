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
  

# Evaluation variétale ----------------------------------------------------


  output$variete_parcelle <- renderPlotly({
    var_plan <- variete %>% 
      distinct(X, Y, cultivar) %>%
      ggplot() +
      aes(x = X, y = Y, fill = cultivar) + # ajouter textui pour variété
      geom_tile(color = "black") +
      scale_fill_viridis_d() + # revoir les couleurs
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      coord_fixed() +
      labs(x = NULL, y = NULL)
    
    ggplotly(var_plan)
  })
  
  
  output$variete_spatial <- renderEcharts4r({
    variete %>%
      filter(Mesure == input$variete_mesure) |>
      tidyr::pivot_wider(names_from = cultivar, values_from = Valeur) |> # pour garder toutes les lignes et colonnes de la parcelle
      arrange(as.numeric(as.character(Y)), desc(X)) |>
      rename(Selec = all_of(input$variete_select_var)) |>
      group_by(Annee)|>
      e_charts(X, reorder = FALSE, timeline = TRUE) |>
      e_grid(left = "30%") |>
      e_heatmap(Y, Selec) |>
      e_visual_map(Selec) |>
      e_title(variete) # "Production annuelle par arbre (kg) de la variété XX"
  })
  
  
  output$variete_var <- renderEcharts4r({
    # A FAIRE : se prévenir de si pas de date sélectionnée
    # enlever les messages d'avis sur les interactions et le e_axis_formatter
    # graphe suffisamment large pour lire le nom des espèce ou inverser les x et y ?
    
    t_params <- powerTransform(variete[variete$Mesure == "masse", "Valeur"], family = "bcnPower")
    
    variete %>% 
      filter(Mesure == input$variete_mesure) %>%
      mutate(Valeur_t = ifelse(!is.na(Valeur), bcnPower(Valeur, lambda = t_params$lambda, gamma = t_params$gamma), NA)) %>% # tranformation de variable
      lm(Valeur_t ~ factor(Annee) * cultivar, data = .) %>%
      ref_grid(at = list(Annee = input$variete_checkbox_year %>% as.numeric())) %>% # POURQUOI ?
      update(tran = make.tran("bcnPower", c(t_params$lambda, t_params$gamma))) %>% # prise en compte de la transformation pour estimer les moyennes marginales
      emmeans("cultivar", type = "response") %>% # estimation des moyennes marginales
      as_tibble() %>%
      e_charts(cultivar) %>% 
      e_bar(response, legend = FALSE, name = NA) %>%
      e_error_bar(lower.CL, upper.CL) %>%
      e_axis(axis = "y", formatter = e_axis_formatter(locale = lang)) |>
      e_title("Production annuelle moyenne par arbre (kg)", "en fonction de la variété et des années de récolte sélectionnées") %>% # textui
      e_tooltip()
  })
  
  output$variete_temporel <- renderEcharts4r({
    # A FAIRE : prévenir de si pas de variété sélectionnée
    
    # pourrait se mettre en eventReactive pour optimiser
    t_params <- powerTransform(variete[variete$Mesure == "masse", "Valeur"], family = "bcnPower")
    
    
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
      # e_band(lower.CL, upper.CL) %>% # à ajouter si une seule variété cochée
      e_axis(axis = "y", formatter = e_axis_formatter(locale = lang)) |>
      e_tooltip(trigger = "axis", formatter = e_tooltip_pointer_formatter(locale = lang, digits = 0)) #%>% 
    # e_mark_line(data = list(xAxis = cultivar)) %>% # si on arrivait à mettre l'année en train d'être visualisée dans les timeline ?
    # e_title(cultivar)
    
    # ajouter la ligne horizontale et la bande de confiance pour une variété cochée
  })
  
  
  
} # end of server
