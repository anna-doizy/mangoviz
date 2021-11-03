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

  
  ## Présentation ####
  
  output$variete_img <- renderImage({
    list(src = paste0("./www/fiches-varietales/", input$variete_radio_cultivar, ".png"))
  }, deleteFile = FALSE)
  
  
  
  ## Résultats ####

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
  
  
  
  
  
  
  output$variete_var <- renderEcharts4r({
    # graphe suffisamment large pour lire le nom des espèce ou inverser les x et y ?
    
    t_params <- powerTransform(variete[variete$Mesure == input$variete_mesure, "Valeur"], family = "bcnPower")
    
    if(!is.null(input$variete_checkbox_year)) # if no selected date, no plot
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
  })
  
  
  
  output$variete_temporel <- renderEcharts4r({
    
    # pourrait se mettre en eventReactive pour optimiser
    t_params <- powerTransform(variete[variete$Mesure == input$variete_mesure, "Valeur"], family = "bcnPower")
    
    var_annee <- variete %>% 
      filter(Mesure == input$variete_mesure) %>%
      mutate(Valeur_t = ifelse(!is.na(Valeur), bcnPower(Valeur, lambda = t_params$lambda, gamma = t_params$gamma), NA)) %>% # tranformation de variable
      lm(Valeur_t ~ factor(Annee) * cultivar, data = .) %>% # modèles vérifiés préalablement
      ref_grid(at = list(cultivar = input$variete_multi_var)) %>%
      update(tran = make.tran("bcnPower", c(t_params$lambda, t_params$gamma))) %>% # prise en compte de la transformation pour estimer les moyennes marginales
      emmeans("Annee", by = "cultivar", type = "response") %>% # estimation des moyennes marginales
      as_tibble() %>%
      mutate(
        Annee = factor(Annee), 
        response = round(response, 0)
      ) |>
      group_by(cultivar)
    
    if(!is.null(input$variete_multi_var)) # if no selected cultivar, no plot
      if(length(input$variete_multi_var) == 1) {
        var_annee |>
          e_charts(Annee) %>% 
          e_line(response) %>%
          e_labels(digits = 0) %>% 
          e_mark_line(data = list(yAxis = mean(var_annee$response) %>% round(0))) |># textui ?
          e_band(lower.CL, upper.CL) %>%
          e_axis(axis = "y", formatter = e_axis_formatter(locale = lang))
      } else {
        var_annee |>
          e_charts(Annee) %>% 
          e_line(response) %>%
          e_axis(axis = "y", formatter = e_axis_formatter(locale = lang)) |>
          e_tooltip(trigger = "axis", formatter = e_tooltip_pointer_formatter(locale = lang, digits = 0))
      }
    
    
  })
  
  
  
  output$variete_spatial <- renderPlot(res = 100, {
    if(!is.null(input$variete_multi_var)) # if no selected cultivar, no plot
      variete %>% 
      filter(Mesure == input$variete_mesure, cultivar %in% input$variete_multi_var) %>%
      ggplot() +
      aes(x = X, y = Y, fill = Valeur) +
      geom_tile(color = "black") +
      # scale_fill_viridis_d() + # revoir les couleurs
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      coord_fixed() +
      labs(x = NULL, y = NULL) +
      facet_wrap(~ Annee, nrow = 1)

  })
  
  
  
} # end of server
