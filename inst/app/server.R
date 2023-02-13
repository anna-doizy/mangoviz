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

    if (is.null(lang) || !(lang %in% c("fr", "en", "sp"))) {
      lang <<- "fr"
    }

  })
  
  
  ## Plot themes ####
  
  theme_update(text = element_text(family = "sans-serif"))
  
  ## Orchards map ####
  
  output$pruning_orchard_map <- renderLeaflet({
    leaflet() %>% 
      setView(55.4884, -21.32264, zoom = 18) %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addMarkers(55.4884, -21.32264)
  })
  
  #-21.322763, 55.490350
  output$cultivar_orchard_map <- renderLeaflet({
    leaflet() %>% 
      setView(55.49035, -21.32276, zoom = 18) %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addMarkers(55.49035, -21.32276)
  })
  
  
  
# Essai taille ------------------------------------------------------------
  
  # couleur des différents types de taille
  coul_taille <- c(`taille_ete` = "#5acc56", `taille_hiver` = "#009eff", `taille_sans` = "#fb7017", bordure = "#0300000C")
  
  ## Plan de la parcelle ####
  
  output$taille_parcelle <- renderGirafe({
    
    taille %>% 
      distinct(X, Y, arbre, bloc, Taille) %>%
      bind_rows(
        tibble(
          arbre = textesUI[textesUI$id == "bordure", lang],
          bloc = "bordure",
          Taille = "bordure",
          X = rep(LETTERS[9:1], each = 2) %>% factor(),
          Y = rep(c(1,17), times = 9) %>% factor()
        )
      ) %>%
      {ggplot(.) +
          aes(x = X, y = Y, fill = Taille, data_id = Taille, tooltip = arbre) +
          # geom_tile_interactive(color = "black", aes(label = arbre, tooltip = paste(after_stat(label), textesUI[textesUI$id == "taille_ete", lang], sep = "<br>"))) + # ne marche pas
          geom_tile_interactive(color = "black") +
          scale_fill_manual(
            values = coul_taille, labels = textesUI[textesUI$id %in% c(levels(taille$Taille), "bordure"), lang] %>% setNames(c(levels(taille$Taille), "bordure")),
            guide = guide_legend(byrow = TRUE)
          ) +
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
  
  output$cycles_taille <- renderPlot(res = 90, { # width = 960, height = 288, {
    
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
    date_taille <- tibble(
      Taille = rep(c("taille_hiver", "taille_ete"), times = 2),
      Date_taille = c("2016-08-01", "2016-02-01", "2018-08-01", "2018-02-01") %>% as.Date(),
      Depart = c(4, 4, 2, 2), # sens et position de la flèche
      Pointe = Depart - 0.6,
      pos_img = Depart + 0.5,
      img = "www/shears_ratio.png"
    ) %>% 
      rowwise() %>% 
      mutate(
        Taille = textesUI[textesUI$id == Taille, lang]
      )
    
    #' Data for pruning cycle graph: Date labels for the x axis
    #'
    #' @format A data frame of 47 x 3
    #' 
    #' \describe{
    #'   \item{pas}{date break}
    #'   \item{pas_label}{first letter of the month}
    #'   \item{annee}{turn the year into a general identifier}
    #'   \item{etiquette}{date label, with a month format + the year id once per year}
    #'   }
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
      geom_vline(xintercept = seq(as.Date("2016-01-01"), as.Date("2019-01-01"), "year"), linewidth = 2, color = "white") +
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
      labs(title = textesUI[textesUI$id == "taille_plan_title", lang], x = NULL, y = "", colour = "") +
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
          geom_jitter_interactive(alpha = 0.3, width = 0.2, height = 0, aes(tooltip = paste(after_stat(label), round(after_stat(y), 1), sep = "<br>"), data_id = arbre)) +
          geom_point(stat = "summary", fun = mean, size = 4, color = "white") +
          geom_point_interactive(
            stat = "summary", 
            fun = mean, size = 3, 
            aes(color = Taille, tooltip = round(after_stat(y), 1), data_id = Taille)
          ) +
          scale_fill_manual(values = coul_taille, aesthetics = c("colour", "fill")) +
          scale_x_discrete(labels = textesUI[textesUI$id %in% levels(taille$Taille), lang] %>% setNames(levels(taille$Taille))) +
          labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$taille_mesure, lang]) +
          theme(legend.position = "none")
      } %>% 
        girafe(
          ggobj = ., height_svg = 4, width_svg = 5,
          options = list(
            opts_hover_inv(css = "opacity:0.4;"),
            opts_tooltip(use_fill = TRUE),
            opts_hover(css = "fill:black;"),
            opts_selection(type = "none")
          )
        ) %>% 
        suppressWarnings()
    }
  })
  
  
  ## comparaison des années (suivi temporel) ####
  
  # action du bouton "tout sélectionner"
  observeEvent(input$taille_temp_all, { 
    updateSelectInput(
      session,
      "taille_temps_multi",
      selected = levels(taille$Taille)
    )
  })
  
  output$taille_temporel <- renderGirafe({
    
    if(!is.null(input$taille_temps_multi)) { # if no selected taille, no plot
      {if(length(input$taille_temps_multi) == 1) { # if one selected taille
      # {if(input$taille_temps_multi != "all") { # if one selected taille
        taille %>% 
          filter(Mesure == input$taille_mesure, Taille == input$taille_temps_multi, !is.na(Valeur)) %>%
          rowwise() %>% 
          mutate(Taille_trad = textesUI[textesUI$id == Taille, lang]) %>% 
          ggplot() +
          aes(x = Annee, y = Valeur) +
          geom_vline_interactive(xintercept = 2011.5, color = "white", linewidth = 2, aes(tooltip = textesUI[textesUI$id == "pruning_start", lang])) +
          geom_line_interactive(aes(group = arbre, data_id = arbre, tooltip = arbre, hover_css = "fill:none"), alpha = 0.1) +
          geom_point_interactive(alpha = 0.3, aes(data_id = arbre, tooltip = arbre)) +
          geom_line(stat = "summary", fun = mean, aes(colour = Taille_trad)) +
          geom_point_interactive(stat = "summary", fun = mean, size = 3, aes(colour = Taille_trad, tooltip = paste(after_stat(colour), round(after_stat(y), 1), sep = "<br>"))) +
          geom_smooth_interactive(method = "lm", formula = "y~1", se = FALSE, color = "black", linetype = 2, aes(tooltip = paste(textesUI[textesUI$id == "global_mean", lang], round(after_stat(y), 1), sep = "<br>"))) +
          scale_color_manual(
            values = coul_taille[input$taille_temps_multi] %>% unname() # car aes color : Taille_trad
            # labels = textesUI[textesUI$id %in% levels(taille$Taille), lang] %>% setNames(levels(taille$Taille))
            ) +
          scale_x_continuous(breaks = seq(2008, 2022, by = 2)) +
          labs(
            x = NULL, y = NULL, 
            title = textesUI[textesUI$id == input$taille_mesure, lang],
            colour = textesUI[textesUI$id == "taille_legend", lang]
          )
      } else { # if several/all selected taille
        taille %>% 
          filter(Mesure == input$taille_mesure, Taille %in% input$taille_temps_multi) %>%
          # filter(Mesure == input$taille_mesure) %>%
          group_by(Annee, Taille) %>% 
          summarise(
            Moyenne = mean(Valeur, na.rm = TRUE), 
            # n = n()
            n = length(na.omit(Valeur))
          ) %>%
          suppressMessages() %>% # group message
          rowwise() %>% 
          mutate(Taille_trad = textesUI[textesUI$id == Taille, lang]) %>% 
          ggplot() +
          aes(x = Annee, y = Moyenne, colour = Taille, tooltip = paste(Taille_trad, "<br>", round(Moyenne, 1), "(n=", n, ")"), data_id = Taille) +
          geom_vline_interactive(xintercept = 2011.5, color = "white", linewidth = 2, aes(tooltip = textesUI[textesUI$id == "pruning_start", lang])) +
          geom_line(aes(group = Taille)) +
          geom_point_interactive() +
          scale_color_manual(values = coul_taille[input$taille_temps_multi], labels = textesUI[textesUI$id %in% levels(taille$Taille), lang] %>% setNames(levels(taille$Taille))) +
          # scale_color_manual(values = coul_taille, labels = textesUI[textesUI$id %in% levels(taille$Taille), lang] %>% setNames(levels(taille$Taille))) +
          scale_x_continuous(breaks = seq(2008, 2022, by = 2)) +
          labs(
            x = NULL, y = NULL, 
            title = textesUI[textesUI$id == input$taille_mesure, lang],
            colour = textesUI[textesUI$id == "taille_legend", lang]
          )
      } } %>% 
        suppressWarnings() %>% # geom_vline(): Ignoring `mapping` because `xintercept` was provided.
        girafe(
          ggobj = ., height_svg = 4, width_svg = 5,
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
  observeEvent(input$taille_spatial_all, { 
    updateSelectInput(
      session,
      "taille_spatial_multi",
      selected = levels(taille$Taille)
    )
  })
  
  output$taille_spatial <- renderGirafe({
    
    if(!is.null(input$taille_spatial_multi)) { # if no selected taille, no plot
      {if(input$taille_all_year) {
        taille %>% 
          filter(
            Mesure == input$taille_mesure, 
            # Taille %in% if(input$taille_select == "all") {levels(taille$Taille)} else {input$taille_select}
            Taille %in% input$taille_spatial_multi
          ) %>% 
          group_by(X, Y, Taille) %>% 
          summarise(Moyenne = mean(Valeur, na.rm = TRUE), n = n()) %>% 
          suppressMessages() %>% # group message
          rowwise() %>% 
          mutate(Taille_trad = textesUI[textesUI$id == Taille, lang]) %>% 
          ggplot() +
          aes(x = X, y = Y, fill = Moyenne, tooltip = paste(Taille_trad, "<br>", round(Moyenne, 1), "(n=", n, ")"), data_id = Taille) +
          geom_tile_interactive(colour = "black") +
          # scale_fill_gradientn(
          #   # colours = c("#a40000",  "#de7500", "#ee9300", "#f78b28", "#fc843d", "#ff7e50", "#ff5d7a", "#e851aa", "#aa5fd3", "#0070e9"),
          #   colours = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF', '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1', '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD', '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388', '#805770', '#684957', '#46353A'),
          #   na.value = "transparent" # https://personal.sron.nl/~pault/#fig:scheme_iridescent
          # ) +
          scale_fill_distiller(palette = "YlOrRd", na.value = "transparent", direction = 1) +
          scale_x_discrete(drop = FALSE) +
          scale_y_discrete(drop = FALSE) +
          coord_fixed() +
          labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$taille_mesure, lang], fill = NULL)
      } else {
        taille %>% 
          filter(
            Mesure == input$taille_mesure, 
            # Taille %in% if(input$taille_select == "all") {levels(taille$Taille)} else {input$taille_select}
            Taille%in% input$taille_spatial_multi
          ) %>%
          rowwise() %>% 
          mutate(Taille_trad = textesUI[textesUI$id == Taille, lang]) %>% 
          ggplot() +
          aes(x = X, y = Y, fill = Valeur, tooltip = paste(Taille_trad, round(Valeur, 1), sep = "<br>"), data_id = Taille) +
          geom_tile_interactive(colour = "black") +
          # scale_fill_gradientn(
          #   # colours = c("#a40000",  "#de7500", "#ee9300", "#f78b28", "#fc843d", "#ff7e50", "#ff5d7a", "#e851aa", "#aa5fd3", "#0070e9"),
          #   colours = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF', '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1', '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD', '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388', '#805770', '#684957', '#46353A'),
          #   na.value = "transparent" # https://personal.sron.nl/~pault/#fig:scheme_iridescent
          # ) +
          scale_fill_distiller(palette = "YlOrRd", na.value = "transparent", direction = 1) +
          scale_x_discrete(drop = FALSE) +
          scale_y_discrete(drop = FALSE) +
          coord_fixed() +
          labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$taille_mesure, lang], fill = NULL) +
          facet_wrap(~ Annee, nrow = 2)
      }}%>%
        girafe(
          ggobj = ., width_svg = 10, height_svg = 5,
          options = list(
            opts_hover_inv(css = "opacity:0.2;"),
            opts_tooltip(use_stroke = TRUE),
            opts_hover(css = ""),
            opts_selection(type = "none")
          )
        )
    }
    
  })
  
  
  

# Evaluation variétale ----------------------------------------------------

  
  ## Présentation ####
  
  # Présentation des 10 variétés
  
  output$variete_ui_desc <- renderUI({
    textesUI[textesUI$id == input$variete_radio_desc, lang] %>% 
      markdown()
  })
  
  output$variete_img_desc <- renderImage({
    list(src = paste0("./www/varietes/", input$variete_radio_desc, ".jpg"))
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
          X = c(rep("K",17), rep(LETTERS[10:2], each = 2), rep("A",17)) %>% factor(),
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
          geom_jitter_interactive(alpha = 0.3, width = 0.2, height = 0, aes(tooltip = paste(after_stat(label), round(after_stat(y), 1), sep = "<br>"), data_id = arbre)) +
        geom_point(stat = "summary", fun = mean, size = 4, color = "white") +
        geom_point_interactive(
          stat = "summary", 
          fun = mean, size = 3, 
          aes(color = cultivar, tooltip = paste(after_stat(colour), round(after_stat(y), 1), sep = "<br>"), data_id = cultivar)
        ) +
        scale_fill_manual(values = coul_var, aesthetics = c("colour", "fill")) +
        labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$variete_mesure, lang]) +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
      } %>% 
        girafe(
          ggobj = ., height_svg = 4, width_svg = 5,
          options = list(
            opts_hover_inv(css = "opacity:0.4;"),
            opts_tooltip(use_fill = TRUE),
            opts_hover(css = "fill:black;"),
            opts_selection(type = "none")
          )
        ) %>% 
        suppressWarnings()
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
          geom_smooth_interactive(method = "lm", formula = "y~1", se = FALSE, color = "black", linetype = 2, aes(tooltip = paste(textesUI[textesUI$id == "global_mean", lang], round(after_stat(y), 1), sep = "<br>"))) +
          geom_line(stat = "summary", fun = mean, aes(colour = cultivar)) +
          geom_point_interactive(stat = "summary", fun = mean, size = 3, aes(colour = cultivar, tooltip = paste(after_stat(color), round(after_stat(y), 1), sep = "<br>"))) +
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
            Moyenne = mean(Valeur, na.rm = TRUE), 
            # n = n()
            n = length(na.omit(Valeur))
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
          ggobj = ., height_svg = 4, width_svg = 5,
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
          # scale_fill_gradientn(
          #   # colours = c("#a40000",  "#de7500", "#ee9300", "#f78b28", "#fc843d", "#ff7e50", "#ff5d7a", "#e851aa", "#aa5fd3", "#0070e9"),
          #   colours = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF', '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1', '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD', '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388', '#805770', '#684957', '#46353A'),
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
            # cultivar %in% if(input$variete_select_var == "all") {levels(variete$cultivar)} else {input$variete_select_var}
            cultivar %in% input$variete_spatial_var
          ) %>%
          ggplot() +
          aes(x = X, y = Y, fill = Valeur, tooltip = paste(cultivar, round(Valeur, 1), sep = "<br>"), data_id = cultivar) +
          geom_tile_interactive(colour = "black") +
          # scale_fill_gradientn(
          #   # colours = c("#a40000",  "#de7500", "#ee9300", "#f78b28", "#fc843d", "#ff7e50", "#ff5d7a", "#e851aa", "#aa5fd3", "#0070e9"),
          #   colours = c('#FEFBE9', '#FCF7D5', '#F5F3C1', '#EAF0B5', '#DDECBF', '#D0E7CA', '#C2E3D2', '#B5DDD8', '#A8D8DC', '#9BD2E1', '#8DCBE4', '#81C4E7', '#7BBCE7', '#7EB2E4', '#88A5DD', '#9398D2', '#9B8AC4', '#9D7DB2', '#9A709E', '#906388', '#805770', '#684957', '#46353A'),
          #   na.value = "transparent" # https://personal.sron.nl/~pault/#fig:scheme_iridescent
          # ) +
          scale_fill_distiller(palette = "YlOrRd", na.value = "transparent", direction = 1) +
          scale_x_discrete(drop = FALSE) +
          scale_y_discrete(drop = FALSE) +
          coord_fixed() +
          labs(x = NULL, y = NULL, title = textesUI[textesUI$id == input$variete_mesure, lang], fill = NULL) +
          facet_wrap(~ Annee, nrow = 2)
      }}%>%
        girafe(
          ggobj = ., width_svg = 10, height_svg = 5,
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
