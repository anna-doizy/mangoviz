#### UI ####

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinycssloaders)
  library(shinyhelper)
  library(leaflet)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(ggplot2)
  library(ggiraph)
  library(ggimage)
  library(mangoviz)
})



function(req) {
  
  # Get language
  lang <- try(parseQueryString(req$QUERY_STRING)$lang)

  if (is.null(lang) || !(lang %in% c("fr", "en", "sp"))) {
    lang <- "fr"
  }


  # TITLE -------------------------------------------------------------------

  header <- dashboardHeader(title = a(href = paste0("./?lang=", lang), img(src="title-mangoviz.png", width = 190)))

  header$children[[3]]$children[[3]]$children[[1]]$children[[1]] <- 
    h4(a(img(src = "logo-cirad.jpg", height = 30, alt = "CIRAD")
         , href = "https://www.cirad.fr/"
         , style = "padding:10px")
       , a(img(src = "by-nc-sa.eu.svg", height = 30, alt = "Licence Creative Commons")
           , rel = "license"
           , href = "https://creativecommons.org/licenses/by-nc-sa/4.0/"
           , style = "padding-right:60px")
    )

  # SIDEBAR -----------------------------------------------------------------



  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      br(),
      if(shiny:::inShinyServer()) { # show langage icons if in server
        tagList(
          lapply(c("fr", "en", "sp"), 
          function(l) {
            a(img(src = paste0("flag_", l, ".png"), height = 30), 
              href = paste0("./?lang=", l), 
              style = "padding:20px") # 40 pour 2 langues
          }),
          hr()
        )
      },

      # Tabs

      menuItem(
        text = strong(textesUI[textesUI$id == "accueil", lang]),
        tabName = "accueil",
        icon = icon("seedling")
      ),
      
      menuItem(
        text = strong(textesUI[textesUI$id == "info", lang]),
        tabName = "info",
        icon = icon("circle-info")
      ),
      
      menuItem(
        text = strong(textesUI[textesUI$id == "taille", lang]),
        tabName = "taille",
        menuSubItem(
          text = textesUI[textesUI$id == "presentation", lang],
          tabName = "taille_presentation", icon = icon("angles-right")
        ),
        menuSubItem(
          text = textesUI[textesUI$id == "resultats", lang],
          tabName = "taille_resultats", icon = icon("angles-right")
        ),
        icon = icon("scissors")
      ),
      
      menuItem(
        text = strong(textesUI[textesUI$id == "variete", lang]),
        tabName = "variete",
        menuSubItem(
          text = textesUI[textesUI$id == "presentation", lang],
          tabName = "var_presentation", icon = icon("angles-right")
        ),
        menuSubItem(
          text = textesUI[textesUI$id == "resultats", lang],
          tabName = "var_resultats", icon = icon("angles-right")
        ),
        menuSubItem(
          text = textesUI[textesUI$id == "bilan", lang],
          tabName = "var_bilan", icon = icon("angles-right")
        ),
        icon = icon("dna")
      ),
      
      menuItem(
        text = strong(textesUI[textesUI$id == "savoirplus", lang]),
        tabName = "savoirplus",
        icon = icon("book")
      )
    )
  )





  # BODY --------------------------------------------------------------------




  body <- dashboardBody(
    
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.png"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "https://plausible.io/js/script.js", `data-domain` = "doana-r.shinyapps.io/mangoviz") # tracking
      ),
    
    tabItems(

      # Onglet Accueil ####
      
      tabItem(tabName = "accueil", fluidRow(
        div(includeMarkdown(sprintf("locale/accueil_%s.md", lang)), class = "markdown-tab")
      )),
      
      # Onglet info ####
      tabItem(
        tabName = "info", 
        fluidRow(
          column(
            12, 
            box(
              title = textesUI[textesUI$id == "pourqui_box", lang], 
              width = 12,
              status = "success",
              solidHeader = TRUE,
              includeMarkdown(sprintf("locale/pourqui_%s.md", lang))
            )
          )),
        fluidRow(
          column(
            12, 
            box(
              title = textesUI[textesUI$id == "comment_box", lang],
              width = 12,
              status = "success",
              solidHeader = TRUE,
              includeMarkdown(sprintf("locale/comment_%s.md", lang))
            ))
        )
      ),

      # Onglet essai taille ####
      ## Le verger ####
      tabItem(
        tabName = "taille_presentation", 
        fluidRow(
          column(
            12, 
            box(
              title = textesUI[textesUI$id == "pres_box", lang], 
              width = 12,
              status = "success",
              solidHeader = TRUE,
              column(6, includeMarkdown(sprintf("locale/verger-taille_%s.md", lang))),
              column(6, leafletOutput("pruning_orchard_map", width = "80%"))
            )
          )),
        fluidRow(
          column(
            5, 
            box(
              title = textesUI[textesUI$id == "plan_box", lang],
              width = 12,
              status = "success",
              solidHeader = TRUE,
              girafeOutput("taille_parcelle", height = "400px", width = "90%"),
              # column(6, img(src = "verger_MA05.png", class = "inbox-img"), style = "text-align:center;")
              
            # )
          )),
        # fluidRow(
          column(
            7,
            box(
              title = textesUI[textesUI$id == "taille_cycle_box", lang],
              width = 12,
              status = "success",
              solidHeader = TRUE,
              plotOutput("cycles_taille")
            )
          )
        )
      ),
      
      ## le suivi ####
      tabItem(tabName = "taille_resultats", 
              
              fluidRow(
                column(12,
                       box(
                         width = 12,
                         status = "success",
                         solidHeader = TRUE,
                         radioButtons(
                           inputId = "taille_mesure",
                           label = textesUI[textesUI$id == "variete_mesure_label", lang],
                           choices = unique(taille$Mesure) %>% setNames(textesUI[textesUI$id %in% unique(taille$Mesure), lang]),
                           inline = TRUE
                         )
                       )
                )
              ),
              
              fluidRow(
                column(6,
                       box(
                         title = textesUI[textesUI$id == "taille_comp_box", lang] %>% 
                           helper(
                             content = paste("taille_comp_text", lang, sep = "_"),
                             buttonLabel = "OK",
                             size = "l", class = "shinyhelper-container2", icon = "circle-question"
                           ),
                         width = 12,
                         status = "success",
                         solidHeader = TRUE,
                         # checkboxGroupButtons(
                         #   "taille_checkbox_year",
                         #   textesUI[textesUI$id == "variete_comp_label", lang],
                         #   # status = "danger",
                         #   choices = unique(taille$Annee),
                         #   selected = 2012:2021 # par défaut, à partir de la première année de taille
                         # ),
                         
                         fluidRow(
                           column(
                             10,
                            checkboxGroupButtons(
                              "taille_checkbox_year",
                              textesUI[textesUI$id == "variete_comp_label", lang],
                              choices = unique(taille$Annee),
                              selected = max(taille$Annee) # la dernière année
                            )
                           ),
                           
                           column(
                             2, p(), 
                             actionBttn( # bouton tout désélectionner
                               inputId = "taille_year_none",
                               icon = icon("trash"),
                               style = "material-flat",
                               size = "sm",
                               title = textesUI[textesUI$id == "trash_title", lang]
                             ),
                             actionBttn( # bouton tout sélectionner
                               inputId = "taille_year_all",
                               icon = icon("square-check"),
                               style = "material-flat",
                               size = "sm",
                               title = textesUI[textesUI$id == "squarecheck_title", lang]
                             )
                           )
                         ), # end of fluidrow
                         
                         
                         
                         
                         girafeOutput("taille_taille", height = "400px") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
                       )
                ),
                column(6,
                       box(
                         title = textesUI[textesUI$id == "taille_temps_box", lang] %>% 
                           helper(
                             content = paste("taille_temps_text", lang, sep = "_"),
                             buttonLabel = "OK",
                             size = "l", class = "shinyhelper-container2", icon = "circle-question"
                           ),
                         width = 12,
                         status = "success",
                         solidHeader = TRUE,
                         # selectInput(
                         #   "taille_temps_multi",
                         #   textesUI[textesUI$id == "taille_multi_label", lang],
                         #   choices = levels(taille$Taille) %>% setNames(textesUI[textesUI$id %in% levels(taille$Taille), lang]),
                         #   multiple = TRUE,
                         #   selected = "taille_sans"
                         # ),
                         
                         # fluidRow(
                         #   column(8, selectInput(
                         #     "taille_temps_multi",
                         #     textesUI[textesUI$id == "taille_multi_label", lang],
                         #     choices = levels(taille$Taille) %>% setNames(textesUI[textesUI$id %in% levels(taille$Taille), lang]),
                         #     selected = "taille_sans",
                         #     multiple = TRUE
                         #   )),
                         #   
                         #   column(4, p(), actionBttn(
                         #     inputId = "taille_temp_all",
                         #     label = textesUI[textesUI$id == "all", lang],
                         #     style = "material-flat",
                         #     size = "sm"
                         #   ))
                         # ),
                         
                         fluidRow(
                           column(9,
                              checkboxGroupButtons(
                                "taille_temps_multi",
                                textesUI[textesUI$id == "taille_multi_label", lang],
                                choices = levels(taille$Taille) %>% setNames(textesUI[textesUI$id %in% levels(taille$Taille), lang]),
                                selected = "taille_sans",
                                individual = TRUE
                              )
                           ),
                           
                           column(
                             3, p(), 
                             actionBttn( # bouton tout désélectionner
                               inputId = "taille_temp_none",
                               icon = icon("trash"),
                               style = "material-flat",
                               size = "sm",
                               title = textesUI[textesUI$id == "trash_title", lang]
                             ),
                             actionBttn( # bouton tout sélectionner
                               inputId = "taille_temp_all",
                               icon = icon("square-check"),
                               style = "material-flat",
                               size = "sm",
                               title = textesUI[textesUI$id == "squarecheck_title", lang]
                             )
                           )
                         ), # end of fluidrow
                         
                         girafeOutput("taille_temporel", height = "400px") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
                       )
                )
                
              ),
              
              fluidRow(
                column(12,
                       box(
                         title = textesUI[textesUI$id == "taille_spatial_box", lang] %>% 
                           helper(
                             content = paste("taille_spatial_text", lang, sep = "_"),
                             buttonLabel = "OK",
                             size = "l", class = "shinyhelper-container2", icon = "circle-question"
                           ),
                         width = 12,
                         status = "success",
                         solidHeader = TRUE,
                         
                         fluidRow(
                           column(2, #p(strong(textesUI[textesUI$id == "timeviz_global_switch", lang])),
                                  # materialSwitch(
                                  #   inputId = "taille_all_year"
                                  # )
                              radioButtons(
                                inputId = "taille_all_year",
                                label = textesUI[textesUI$id == "timeviz_global_switch", lang],
                                choices = c(FALSE, TRUE) %>% 
                                  setNames(textesUI[textesUI$id %in% c("timeviz_global_switch_f", "timeviz_global_switch_t"), lang])
                                  ),
                           ),
                         #   column(6, selectInput(
                         #     "taille_spatial_multi",
                         #     textesUI[textesUI$id == "taille_multi_label", lang],
                         #     choices = levels(taille$Taille) %>% setNames(textesUI[textesUI$id %in% levels(taille$Taille), lang]),
                         #     multiple = TRUE,
                         #     selected = "taille_sans"
                         #     ),
                         #   # column(10, radioGroupButtons(
                         #   #   "taille_select",
                         #   #   individual = TRUE,
                         #   #   textesUI[textesUI$id == "taille_temps_label", lang],
                         #   #   choices = c("all", levels(taille$Taille)) %>% setNames(textesUI[textesUI$id %in% c(levels(taille$Taille), "all"), lang])
                         #   # )
                         #   ),
                         #   
                         #   column(4, p(), actionBttn(
                         #     inputId = "taille_spatial_all",
                         #     label = textesUI[textesUI$id == "all", lang],
                         #     style = "material-flat",
                         #     size = "sm"
                         #   ))
                         # ),
                         
                         column(8,
                            checkboxGroupButtons(
                              "taille_spatial_multi",
                              textesUI[textesUI$id == "taille_multi_label", lang],
                              choices = levels(taille$Taille) %>% setNames(textesUI[textesUI$id %in% levels(taille$Taille), lang]),
                              selected = "taille_sans",
                              individual = TRUE
                            )
                         ),
                         column(
                           2, p(), 
                           actionBttn( # bouton tout désélectionner
                             inputId = "taille_spatial_none",
                             icon = icon("trash"),
                             style = "material-flat",
                             size = "sm",
                             title = textesUI[textesUI$id == "trash_title", lang]
                           ),
                           actionBttn( # bouton tout sélectionner
                             inputId = "taille_spatial_all",
                             icon = icon("square-check"),
                             style = "material-flat",
                             size = "sm",
                             title = textesUI[textesUI$id == "squarecheck_title", lang]
                           )
                         )
                       ), # end of fluidRow
                         
                         girafeOutput("taille_spatial", height = "600px") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
                       )
                )
              )
      ),
      
      
      # Onglet évaluation variétale ####
      ## Le verger ####
      tabItem(
        tabName = "var_presentation", 
        
        fluidRow(
          column(
            12, 
            box(
              title = textesUI[textesUI$id == "pres_box", lang], 
              width = 12,
              status = "success",
              solidHeader = TRUE,
              column(6, includeMarkdown(sprintf("locale/verger-variete_%s.md", lang))),
              column(6, leafletOutput("cultivar_orchard_map", width = "80%"))
            )
          )),
        fluidRow(
          # column(
          #   6, 
          #   box(
          #     title = textesUI[textesUI$id == "pres_box", lang], 
          #     width = 12, #height = 570,
          #     status = "success",
          #     solidHeader = TRUE,
          #     includeMarkdown(sprintf("locale/verger-variete_%s.md", lang)),
          #     leafletOutput("cultivar_orchard_map", width = "80%")
          #   )
          # ),
          column(
            6, 
            box(
              title = textesUI[textesUI$id == "plan_box", lang],
              width = 12,
              status = "success",
              solidHeader = TRUE,
              girafeOutput("variete_parcelle", height = "400px", width = "90%")
            )
            ),
          column(
            6,
            box(
              title = textesUI[textesUI$id == "variete_var_box", lang],
              width = 12,
              status = "success",
              solidHeader = TRUE,
              
              radioButtons(
                inputId = "variete_radio_desc",
                label = textesUI[textesUI$id == "variete_bilan_label", lang],
                choices = levels(variete$cultivar) %>% 
                  str_to_lower() %>% 
                  str_replace_all(" ", "_") %>% str_replace("é", "e") %>% 
                  setNames(levels(variete$cultivar)),
                inline = TRUE
              ),
              
              uiOutput("variete_ui_desc"),
              imageOutput("variete_img_desc", height = "auto"),
            )
          )
        )
      ),

      ## le suivi ####
      tabItem(tabName = "var_resultats", 
          
        fluidRow(
          column(12,
            box(
              width = 12,
              status = "success",
              solidHeader = TRUE,
              radioButtons(
                inputId = "variete_mesure",
                label = textesUI[textesUI$id == "variete_mesure_label", lang],
                choices = unique(variete$Mesure) %>% setNames(textesUI[textesUI$id %in% unique(variete$Mesure), lang]),
                inline = TRUE
              )
            )
          )
        ),
        
        fluidRow(
          column(6,
            box(
              title = textesUI[textesUI$id == "variete_comp_box", lang] %>% 
                helper(
                  content = paste("variete_comp_text", lang, sep = "_"),
                  buttonLabel = "OK",
                  size = "l", class = "shinyhelper-container2", icon = "circle-question"
                ),
              width = 12,
              status = "success",
              solidHeader = TRUE,
              # checkboxGroupButtons(
              #   "variete_checkbox_year",
              #   textesUI[textesUI$id == "variete_comp_label", lang],
              #   # status = "danger",
              #   choices = unique(variete$Annee),
              #   selected = unique(variete$Annee) # toutes les années sélectionées par défaut
              # ),
              
              fluidRow(
                column(
                  10,
                  checkboxGroupButtons(
                    "variete_checkbox_year",
                    textesUI[textesUI$id == "variete_comp_label", lang],
                    choices = unique(variete$Annee),
                    selected = max(variete$Annee) # la dernière année sélectionée par défaut
                  )
                ),
                
                column(
                  2, p(), 
                  actionBttn( # bouton tout désélectionner
                    inputId = "variete_year_none",
                    icon = icon("trash"),
                    style = "material-flat",
                    size = "sm",
                    title = textesUI[textesUI$id == "trash_title", lang]
                  ),
                  actionBttn( # bouton tout sélectionner
                    inputId = "variete_year_all",
                    icon = icon("square-check"),
                    style = "material-flat",
                    size = "sm",
                    title = textesUI[textesUI$id == "squarecheck_title", lang]
                  )
                )
              ), # end of fluidrow
              
              
              
              
              girafeOutput("variete_var", height = "400px") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
            )
          ),
          column(6,
            box(
              title = textesUI[textesUI$id == "variete_temps_box", lang] %>% 
                helper(
                  content = paste("variete_temps_text", lang, sep = "_"),
                  buttonLabel = "OK",
                  size = "l", class = "shinyhelper-container2", icon = "circle-question"
                ),
              width = 12,
              status = "success",
              solidHeader = TRUE,
              
              fluidRow(
                column(9, 
                 # selectInput(
                 #  "variete_temp_var",
                 #  textesUI[textesUI$id == "variete_label_vars", lang],
                 #  choices = levels(variete$cultivar),
                 #  selected = "Caro",
                 #  multiple = TRUE
                 # )
                 checkboxGroupButtons(
                   "variete_temp_var",
                   textesUI[textesUI$id == "variete_label_vars", lang],
                   choices = levels(variete$cultivar),
                   selected = "Caro",
                   individual = TRUE
                 )
                ),
                
                column(
                  3, p(), 
                  actionBttn( # bouton tout désélectionner
                    inputId = "variete_temp_none",
                    icon = icon("trash"),
                    style = "material-flat",
                    size = "sm",
                    title = textesUI[textesUI$id == "trash_title", lang]
                  ),
                  actionBttn( # bouton tout sélectionner
                    inputId = "variete_temp_all",
                    icon = icon("square-check"),
                    style = "material-flat",
                    size = "sm",
                    title = textesUI[textesUI$id == "squarecheck_title", lang]
                  )
                )
              ), # end of fluidrow

              girafeOutput("variete_temp_graph", height = "400px") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
            )
          )
        
        ),
        
        fluidRow(
          column(12,
            box(
              title = textesUI[textesUI$id == "variete_spatial_box", lang] %>% 
                helper(
                  content = paste("variete_spatial_text", lang, sep = "_"),
                  buttonLabel = "OK",
                  size = "l", class = "shinyhelper-container2", icon = "circle-question"
                ),
              width = 12,
              status = "success",
              solidHeader = TRUE,
              
              
              fluidRow(
                column(2, #p(strong(textesUI[textesUI$id == "timeviz_global_switch", lang])),
                  # materialSwitch(
                  #   inputId = "variete_all_year"
                  # )
                  radioButtons(
                    inputId = "variete_all_year",
                    label = textesUI[textesUI$id == "timeviz_global_switch", lang],
                    choices = c(FALSE, TRUE) %>% 
                      setNames(textesUI[textesUI$id %in% c("timeviz_global_switch_f", "timeviz_global_switch_t"), lang])
                  ),
                ),
                # column(10, radioGroupButtons(
                #   "variete_spatial_var",
                #   individual = TRUE,
                #   textesUI[textesUI$id == "variete_label_vars", lang],
                #   choices = c("all", levels(variete$cultivar)) %>% setNames(c(textesUI[textesUI$id =="all", lang], levels(variete$cultivar)))
                #   )
                # ),
                column(8, 
                  # selectInput(
                  # "variete_spatial_var",
                  # textesUI[textesUI$id == "variete_label_vars", lang],
                  # choices = levels(variete$cultivar),
                  # selected = "Caro",
                  # multiple = TRUE
                  # )
                  
                  checkboxGroupButtons(
                    "variete_spatial_var",
                    textesUI[textesUI$id == "variete_label_vars", lang],
                    choices = levels(variete$cultivar),
                    selected = "Caro",
                    individual = TRUE
                  )
                
                ),
                
                
                column(
                  2, p(), 
                  actionBttn( # bouton tout désélectionner
                    inputId = "variete_spatial_none",
                    icon = icon("trash"),
                    style = "material-flat",
                    size = "sm",
                    title = textesUI[textesUI$id == "trash_title", lang]
                  ),
                  actionBttn( # bouton tout sélectionner
                    inputId = "variete_spatial_all",
                    icon = icon("square-check"),
                    style = "material-flat",
                    size = "sm",
                    title = textesUI[textesUI$id == "squarecheck_title", lang]
                  )
                )
              ),
              
              girafeOutput("variete_spatial_graph", height = "600px") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
            )
          )
        )
      ),
      
      ## le bilan ####
      tabItem(
        tabName = "var_bilan",
        fluidRow(
          column(
            12,
            box(
              title = textesUI[textesUI$id == "variete_bilan_box", lang],
              width = 12, height = 1200,
              status = "success",
              solidHeader = TRUE,
              
              fluidRow(
                column(
                  8,
                  radioButtons(
                  inputId = "variete_radio_bilan",
                  label = textesUI[textesUI$id == "variete_bilan_label", lang],
                  # choices = unique(variete$cultivar) %>% sort(),
                  choices = levels(variete$cultivar) %>%
                    str_to_lower() %>%
                    str_replace_all(" ", "_") %>% str_replace("é", "e") %>%
                    setNames(levels(variete$cultivar)),
                  inline = TRUE
                )
                  ),
                column(
                  4,
                  a(href="Fiches_varietales.pdf",  HTML(paste(icon(name = "download"), textesUI[textesUI$id == "telecharger_fiches", lang])), download=NA, target="_blank", class="btn btn-default shiny-download-link shiny-bound-output") # c'est un peu tordu, mais ça marche !
                )
              ), # end of fluidrow
              
              imageOutput("variete_img_bilan")
            ) # end of box
          )
        )
      ),
      
      # Onglet en savoir plus ####
      tabItem(tabName = "savoirplus", fluidRow(
        div(includeMarkdown(sprintf("locale/savoirplus_%s.md", lang)), class = "markdown-tab")
      ))

    ) # end of tabItems
  ) # end of Dashboardbody



# PAGE --------------------------------------------------------------------

  dashboardPage(header, sidebar, body, title = "MangoViz", skin = "green")
} # end of req
