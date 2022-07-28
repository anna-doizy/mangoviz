#### SERVER ####

server <- function(input, output, session) {
  
  # for local use : stop the server when the session ends
  is_local <- Sys.getenv('SHINY_PORT') == ""
  if(is_local) session$onSessionEnded(function() stopApp())


  # Observe -----------------------------------------------------------------
  
  observe_helpers()

  ## Get language in the url ####
  lang <- dyn <- NULL

  observe(priority = 1000, {
    lang <<- isolate(getQueryString()$lang)

    if (is.null(lang) || !(lang %in% c("fr", "en"))) {
      lang <<- "fr"
    }

  })
  
  
  ## Plot themes ####
  
  theme_update(text = element_text(family = "sans"))
  
  ## Orchards map ####
  
  output$pruning_orchard_map <- renderLeaflet({
    leaflet() %>% addTiles() %>% addMarkers(55.489, -21.3216)
  })
  
  output$cultivar_orchard_map <- renderLeaflet({
    leaflet() %>% addTiles() %>% addMarkers(55.489, -21.3216)
  })
  
  
  
# Essai taille ------------------------------------------------------------
  
  # couleur des différents types de taille
  coul_taille <- c(`taille_ete` = "darkgreen", `taille_hiver` = "darkblue", `taille_sans` = "darkred", bordure = "#0300000C")
  
  ## Plan de la parcelle ####
  
  output$taille_parcelle <- renderGirafe({
    
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
        ggobj = ., height_svg = 4, width_svg = 4,
        options = list(
          opts_hover_inv(css = "opacity:0.2;"),
          opts_tooltip(use_fill = TRUE),
          opts_hover(css = "fill:black;opacity:0.8;"),
          opts_selection(type = "none")
        )
      )
  })
  
  ## Description du cycle des tailles ####
  
  output$cycles_taille <- renderPlot({
    ggplot(cycle) +
      aes(x = Debut, y = Cycle) +
      geom_vline(xintercept = seq(as.Date("2016-01-01"), as.Date("2019-01-01"), "year"), size = 2, color = "white") +
      geom_segment(
        aes(xend = Fin, yend = Cycle, colour = Etape),
        size = 8
      ) +
      geom_segment(
        data = date_taille, 
        mapping = aes(x = Date_taille, xend = Date_taille, y = Depart, yend = Pointe), 
        arrow = arrow(length = unit(0.1, "inches")),
        size = 1, colour = "black"
      ) +
      geom_image(
        data = date_taille, 
        mapping = aes(x = Date_taille, y = pos_img, image = img), 
        size = 0.05
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
      coord_fixed(70, ylim = c(0.8, 4.5)) +
      labs(x = NULL, y = "", colour = "") +
      theme(legend.position = "bottom", axis.text.x = element_text(face = "bold"))
  })
  
  
  ## comparaison des tailles ####
  
  output$taille_taille <- renderGirafe({
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
          scale_x_discrete(labels = textesUI[textesUI$id %in% levels(taille$Taille), lang] %>% setNames(levels(taille$Taille))) +
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
    
    # if(!is.null(input$taille_multi)) { # if no selected taille, no plot
      # {if(length(input$taille_multi) == 1) { # if one selected taille
      {if(input$taille_multi != "all") { # if one selected taille
        taille %>% 
          filter(Mesure == input$taille_mesure, Taille == input$taille_multi, !is.na(Valeur)) %>%
          rowwise() %>% 
          mutate(Taille_trad = textesUI[textesUI$id == Taille, lang]) %>% 
          ggplot() +
          aes(x = Annee, y = Valeur) +
          geom_vline_interactive(xintercept = 2010.5, color = "white", size = 2, aes(tooltip = textesUI[textesUI$id == "pruning_start", lang])) + # ou 2011.5 ??
          geom_line_interactive(aes(group = arbre, data_id = arbre, tooltip = arbre, hover_css = "fill:none"), alpha = 0.1) +
          geom_point_interactive(alpha = 0.3, aes(data_id = arbre, tooltip = arbre)) +
          geom_line(stat = "summary", fun = mean, aes(colour = Taille_trad)) +
          geom_point_interactive(stat = "summary", fun = mean, size = 3, aes(colour = Taille_trad, tooltip = paste(..color.., round(..y.., 1), sep = "<br>"))) +
          geom_smooth_interactive(method = "lm", formula = "y~1", se = FALSE, color = "black", linetype = 2, aes(tooltip = paste(textesUI[textesUI$id == "global_mean", lang], round(..y.., 1), sep = "<br>"))) +
          scale_color_manual(
            values = coul_taille[input$taille_multi] %>% unname() # car aes color : Taille_trad
            # labels = textesUI[textesUI$id %in% levels(taille$Taille), lang] %>% setNames(levels(taille$Taille))
            ) +
          scale_x_continuous(breaks = seq(2008, 2018, by = 2)) +
          labs(
            x = NULL, y = NULL, 
            title = textesUI[textesUI$id == input$taille_mesure, lang],
            colour = textesUI[textesUI$id == "taille_legend", lang]
          )
      } else { # if several/all selected taille
        taille %>% 
          # filter(Mesure == input$taille_mesure, Taille %in% input$taille_multi) %>%
          filter(Mesure == input$taille_mesure) %>%
          group_by(Annee, Taille) %>% 
          summarise(
            Moyenne = mean(Valeur, na.rm = TRUE), n = n()
          ) %>%
          suppressMessages() %>% # group message
          rowwise() %>% 
          mutate(Taille_trad = textesUI[textesUI$id == Taille, lang]) %>% 
          ggplot() +
          aes(x = Annee, y = Moyenne, colour = Taille, tooltip = paste(Taille_trad, "<br>", round(Moyenne, 1), "(n=", n, ")"), data_id = Taille) +
          geom_vline_interactive(xintercept = 2010.5, color = "white", size = 2, aes(tooltip = textesUI[textesUI$id == "pruning_start", lang])) + # ou 2011.5 ??
          geom_line(aes(group = Taille)) +
          geom_point_interactive() +
          # scale_color_manual(values = coul_taille[input$taille_multi], labels = textesUI[textesUI$id %in% levels(taille$Taille), lang] %>% setNames(levels(taille$Taille))) +
          scale_color_manual(values = coul_taille, labels = textesUI[textesUI$id %in% levels(taille$Taille), lang] %>% setNames(levels(taille$Taille))) +
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
    # }
    
    
  })
  
  ## mesures à l'échelle de la parcelle ####
  
  output$taille_spatial <- renderGirafe({
    # traduire tooltip OK
    # vérifier valeurs extrêmes de poids moyen de fruit
    # une ou deux lignes ?
    # comment montrer l'année où on a commencé à tailler ? -> l'ajouter dans le texte ?
    
    {if(input$taille_all_year) {
      taille %>% 
        filter(
          Mesure == input$taille_mesure, 
          Taille %in% if(input$taille_select == "all") {levels(taille$Taille)} else {input$taille_select}
        ) %>% 
        group_by(X, Y, Taille) %>% 
        summarise(Moyenne = mean(Valeur, na.rm = TRUE), n = n()) %>% 
        suppressMessages() %>% # group message
        rowwise() %>% 
        mutate(Taille_trad = textesUI[textesUI$id == Taille, lang]) %>% 
        ggplot() +
        aes(x = X, y = Y, fill = Moyenne, tooltip = paste(Taille_trad, "<br>", round(Moyenne, 1), "(n=", n, ")"), data_id = Taille) +
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
  coul_var <- c("#b94137", "#0080b4", "#008355", "#7f4ecc", "#ce7e26", "#8aa543", "#56423e", "#be4b7f", "#a5af98", "#00c1ff", "#0300000C") %>% 
    setNames(c(levels(variete$cultivar), "bordure"))
  
  ## Plan de la parcelle ####

  output$variete_parcelle <- renderGirafe({
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
          annotate(geom = "point", x = "F", y = "12", shape = 4, size = 5) +
          scale_fill_manual(
            values = coul_var, labels = c(bordure = textesUI[textesUI$id == "bordure", lang]),
            guide = guide_legend(byrow = TRUE)) +
          scale_x_discrete(drop = FALSE) +
          scale_y_discrete(drop = FALSE) +
          coord_fixed() +
          labs(x = NULL, y = NULL, fill = textesUI[textesUI$id == "cultivar", lang])} %>% 
      girafe(
        ggobj = ., height_svg = 4, width_svg = 4,
        options = list(
          opts_hover_inv(css = "opacity:0.2;"),
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
  })
  
  ## comparaison des années (suivi temporel) ####
  
  # action du bouton "tout sélectionner"
  observeEvent(input$variete_temp_all, { 
      updateSelectInput(
        session,
        "variete_temp_var",
        selected = levels(variete$cultivar)
      )
  })
  
 
  output$variete_temp_graph <- renderGirafe({

    if(!is.null(input$variete_temp_var)) # if no selected cultivar, no plot
    {
      {if(length(input$variete_temp_var) == 1) { # if one selected cultivar
        variete %>% 
          filter(Mesure == input$variete_mesure, cultivar == input$variete_temp_var, !is.na(Valeur)) %>%
          ggplot() +
          aes(x = Annee, y = Valeur) +
          geom_line_interactive(aes(group = arbre, data_id = arbre, tooltip = arbre, hover_css = "fill:none"), alpha = 0.1) +
          geom_point_interactive(alpha = 0.3, aes(data_id = arbre, tooltip = arbre)) +
          geom_smooth_interactive(method = "lm", formula = "y~1", se = FALSE, color = "black", linetype = 2, aes(tooltip = paste(textesUI[textesUI$id == "global_mean", lang], round(..y.., 1), sep = "<br>"))) +
          geom_line(stat = "summary", fun = mean, aes(colour = cultivar)) +
          geom_point_interactive(stat = "summary", fun = mean, size = 3, aes(colour = cultivar, tooltip = paste(..color.., round(..y.., 1), sep = "<br>"))) +
          scale_color_manual(values = coul_var[input$variete_temp_var]) +
          labs(
            x = NULL, y = NULL, 
            title = textesUI[textesUI$id == input$variete_mesure, lang],
            colour = textesUI[textesUI$id == "cultivar", lang]
          )
      } else { # if several selected cultivars
        variete %>% 
          filter(Mesure == input$variete_mesure, cultivar %in% input$variete_temp_var) %>%
          group_by(Annee, cultivar) %>% 
          summarise(
            Moyenne = mean(Valeur, na.rm = TRUE), n = n()
          ) %>%
          suppressMessages() %>% # group message
          ggplot() +
          aes(x = Annee, y = Moyenne, colour = cultivar, tooltip = paste(cultivar, "<br>", round(Moyenne, 1), "(n=", n, ")"), data_id = cultivar) +
          geom_line(aes(group = cultivar)) +
          geom_point_interactive() +
          scale_color_manual(values = coul_var[input$variete_temp_var]) +
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
  
  ## mesures à l'échelle de la parcelle ####
  
  # action du bouton "tout sélectionner"
  observeEvent(input$variete_spatial_all, { 
    updateSelectInput(
      session,
      "variete_spatial_var",
      selected = levels(variete$cultivar)
    )
  })
  
  output$variete_spatial_graph <- renderGirafe({
    
    if(!is.null(input$variete_spatial_var)) { # if no selected cultivar, no plot
      {if(input$variete_all_year) {
        variete %>% 
          filter(
            Mesure == input$variete_mesure, 
            # cultivar %in% if(input$variete_select_var == "all") {levels(variete$cultivar)} else {input$variete_select_var}
            cultivar %in% input$variete_spatial_var
          ) %>% 
          group_by(X, Y, cultivar) %>% 
          summarise(Moyenne = mean(Valeur, na.rm = TRUE), n = n()) %>% 
          suppressMessages() %>% # group message
          ggplot() +
          aes(x = X, y = Y, fill = Moyenne, tooltip = paste(cultivar, "<br>", round(Moyenne, 1), "(n=", n, ")"), data_id = cultivar) +
          geom_tile_interactive(colour = "black") +
          scale_fill_gradientn(
            # colours = c("#a40000",  "#de7500", "#ee9300", "#f78b28", "#fc843d", "#ff7e50", "#ff5d7a", "#e851aa", "#aa5fd3", "#0070e9"), 
            colours = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF', '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1', '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD', '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388', '#805770', '#684957', '#46353A'),
            na.value = "transparent" # https://personal.sron.nl/~pault/#fig:scheme_iridescent
          ) +
          scale_x_discrete(drop = FALSE) +
          scale_y_discrete(drop = FALSE) +
          coord_fixed() +
          labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$variete_mesure, lang], fill = NULL)
      } else {
        variete %>% 
          filter(
            Mesure == input$variete_mesure, 
            # cultivar %in% if(input$variete_select_var == "all") {levels(variete$cultivar)} else {input$variete_select_var}
            cultivar %in% input$variete_spatial_var
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
      }

  })
  
  


  
  
  
  
  
  
  
  
} # end of server
