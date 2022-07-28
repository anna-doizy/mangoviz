#### UI ####

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinycssloaders)
  library(shinyhelper)
  library(leaflet)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(ggiraph)
  library(ggimage)
  library(mangoviz)
})



function(req) {
  
  # Get language
  lang <- try(parseQueryString(req$QUERY_STRING)$lang)

  if (is.null(lang) || !(lang %in% c("fr", "en"))) {
    lang <- "fr"
  }


  # TITLE -------------------------------------------------------------------

  header <- dashboardHeader(title = a(href = paste0("./?lang=", lang), img(src="title-mangoviz.png", width = 190)))


  # SIDEBAR -----------------------------------------------------------------



  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      br(),
      if(shiny:::inShinyServer()) { # show langage icons if in server
        tagList(
          lapply(c("fr", "en"), 
          function(l) {
            a(img(src = paste0("flag_", l, ".png"), height = 30), 
              href = paste0("./?lang=", l), 
              style = "padding:40px") # 20 pour 3 langues
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
        text = strong(textesUI[textesUI$id == "taille", lang]),
        tabName = "taille",
        menuSubItem(
          text = textesUI[textesUI$id == "presentation", lang],
          tabName = "taille_presentation"
        ),
        menuSubItem(
          text = textesUI[textesUI$id == "resultats", lang],
          tabName = "taille_resultats"
        ),
        icon = icon("cut")
      ),
      
      menuItem(
        text = strong(textesUI[textesUI$id == "variete", lang]),
        tabName = "variete",
        menuSubItem(
          text = textesUI[textesUI$id == "presentation", lang],
          tabName = "var_presentation"
        ),
        menuSubItem(
          text = textesUI[textesUI$id == "resultats", lang],
          tabName = "var_resultats"
        ),
        menuSubItem(
          text = textesUI[textesUI$id == "bilan", lang],
          tabName = "var_bilan"
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
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
    
    tabItems(

      # Onglet Accueil ####
      
      tabItem(tabName = "accueil", fluidRow(
        div(includeMarkdown(sprintf("locale/accueil_%s.md", lang)), class = "markdown-tab")
      )),

      # Onglet essai taille ####
      ## Le verger ####
      tabItem(
        tabName = "taille_presentation", 
        fluidRow(
          column(
            6, 
            box(
              title = textesUI[textesUI$id == "pres_box", lang], 
              width = 12,
              status = "success",
              solidHeader = TRUE,
              includeMarkdown(sprintf("locale/verger-taille_%s.md", lang)),
              leafletOutput("pruning_orchard_map", width = "80%")
            )
          ),
          column(
            6, 
            box(
              title = textesUI[textesUI$id == "plan_box", lang],
              width = 12,
              status = "success",
              solidHeader = TRUE,
              girafeOutput("taille_parcelle", height = "auto", width = "90%")
            ),
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
                             size = "l", class = "shinyhelper-container2"
                           ),
                         width = 12,
                         status = "success",
                         solidHeader = TRUE,
                         # p(em(textesUI[textesUI$id == "variete_comp_text", lang])),
                         checkboxGroupButtons(
                           "taille_checkbox_year",
                           textesUI[textesUI$id == "variete_comp_label", lang],
                           # status = "danger",
                           choices = unique(taille$Annee),
                           selected = 2011:2018 # par défaut, à partir de la première année de taille
                         ),
                         girafeOutput("taille_taille") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
                       )
                ),
                column(6,
                       box(
                         title = textesUI[textesUI$id == "taille_temps_box", lang] %>% 
                           helper(
                             content = paste("taille_temps_text", lang, sep = "_"),
                             buttonLabel = "OK",
                             size = "l", class = "shinyhelper-container2"
                           ),
                         width = 12,
                         status = "success",
                         solidHeader = TRUE,
                         # selectInput(
                         #   "taille_multi",
                         #   textesUI[textesUI$id == "variete_temps_label", lang],
                         #   choices = levels(taille$Taille) %>% setNames(textesUI[textesUI$id %in% levels(taille$Taille), lang]),
                         #   multiple = TRUE,
                         #   selected = "taille_sans"
                         # ),
                         radioGroupButtons(
                           "taille_multi",
                           individual = TRUE,
                           textesUI[textesUI$id == "taille_temps_label", lang],
                           choices = c("all", levels(taille$Taille)) %>% setNames(textesUI[textesUI$id %in% c(levels(taille$Taille), "all"), lang]) # Attention à l'ordre
                         ),
                         girafeOutput("taille_temporel") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
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
                             size = "l", class = "shinyhelper-container2"
                           ),
                         width = 12,
                         status = "success",
                         solidHeader = TRUE,
                         # p(em(textesUI[textesUI$id == "variete_spatial_text", lang])),
                         # selectInput(
                         # "variete_select_var",
                         # "Choix de la variété",
                         # choices = levels(variete$cultivar),
                         # selected = "Caro"
                         # ),
                         
                         fluidRow(
                           column(2, p(strong(textesUI[textesUI$id == "timeviz_global_switch", lang])),
                                  materialSwitch(
                                    inputId = "taille_all_year" #, label = ""
                                  )
                           ),
                           column(10, radioGroupButtons(
                             "taille_select",
                             individual = TRUE,
                             textesUI[textesUI$id == "taille_temps_label", lang],
                             choices = c("all", levels(taille$Taille)) %>% setNames(textesUI[textesUI$id %in% c(levels(taille$Taille), "all"), lang])
                           )
                           )
                         ),
                         
                         girafeOutput("taille_spatial") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
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
            6, 
            box(
              title = textesUI[textesUI$id == "pres_box", lang], 
              width = 12, #height = 570,
              status = "success",
              solidHeader = TRUE,
              includeMarkdown(sprintf("locale/verger-variete_%s.md", lang)),
              leafletOutput("cultivar_orchard_map", width = "80%")
            )
          ),
          column(
            6, 
            box(
              title = textesUI[textesUI$id == "plan_box", lang],
              width = 12,
              status = "success",
              solidHeader = TRUE,
              girafeOutput("variete_parcelle", height = "auto", width = "90%")
            ),
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
                  size = "l", class = "shinyhelper-container2"
                ),
              width = 12,
              status = "success",
              solidHeader = TRUE,
              checkboxGroupButtons(
                "variete_checkbox_year",
                textesUI[textesUI$id == "variete_comp_label", lang],
                # status = "danger",
                choices = unique(variete$Annee),
                selected = unique(variete$Annee) # toutes les années sélectionées par défaut
              ),
              girafeOutput("variete_var") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
            )
          ),
          column(6,
            box(
              title = textesUI[textesUI$id == "variete_temps_box", lang] %>% 
                helper(
                  content = paste("variete_temps_text", lang, sep = "_"),
                  buttonLabel = "OK",
                  size = "l", class = "shinyhelper-container2"
                ),
              width = 12,
              status = "success",
              solidHeader = TRUE,
              
              fluidRow(
                column(8, selectInput(
                 "variete_temp_var",
                 textesUI[textesUI$id == "variete_label_vars", lang],
                 choices = levels(variete$cultivar),
                 selected = "Caro",
                 multiple = TRUE
                )),
                
                column(4, p(), actionBttn(
                  inputId = "variete_temp_all",
                  label = textesUI[textesUI$id == "all", lang],
                  style = "material-flat"
                ))
              ),
              
              girafeOutput("variete_temp_graph") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
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
                  size = "l", class = "shinyhelper-container2"
                ),
              width = 12,
              status = "success",
              solidHeader = TRUE,
              
              
              fluidRow(
                column(2, p(strong(textesUI[textesUI$id == "timeviz_global_switch", lang])),
                  materialSwitch(
                    inputId = "variete_all_year"
                  )
                ),
                # column(10, radioGroupButtons(
                #   "variete_spatial_var",
                #   individual = TRUE,
                #   textesUI[textesUI$id == "variete_label_vars", lang],
                #   choices = c("all", levels(variete$cultivar)) %>% setNames(c(textesUI[textesUI$id =="all", lang], levels(variete$cultivar)))
                #   )
                # ),
                column(6, selectInput(
                  "variete_spatial_var",
                  textesUI[textesUI$id == "variete_label_vars", lang],
                  choices = levels(variete$cultivar),
                  selected = "Caro",
                  multiple = TRUE
                )),
                
                column(4, p(), actionBttn(
                  inputId = "variete_spatial_all",
                  label = textesUI[textesUI$id == "all", lang],
                  style = "material-flat"
                ))
              ),
              
              girafeOutput("variete_spatial_graph") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
            )
          )
        )
      ),
      
      ##le bilan ####
      tabItem(
        tabName = "var_bilan", 
        fluidRow(
          column(
            12,
            box(
              title = textesUI[textesUI$id == "variete_bilan_box", lang],
              width = 12, height = 1100,
              status = "success",
              solidHeader = TRUE,
              radioButtons(
                inputId = "variete_radio_bilan",
                label = textesUI[textesUI$id == "variete_bilan_label", lang],
                # choices = unique(variete$cultivar) %>% sort(),
                choices = levels(variete$cultivar) %>% 
                  str_to_lower() %>% 
                  str_replace_all(" ", "_") %>% str_replace("é", "e") %>% 
                  setNames(levels(variete$cultivar)),
                inline = TRUE
              ),
              imageOutput("variete_img_bilan") # A FAIRE : centrer l'image
            )
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
