#### UI ####

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  # library(shinyhelper)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(car)
  library(emmeans)
  library(echarts4r)
  library(mangoviz)
})



function(req) {
  
  # Get language
  lang <- try(parseQueryString(req$QUERY_STRING)$lang)

  if (is.null(lang) || !(lang %in% c("fr", "en"))) {
    lang <- "fr"
  }


  # TITLE -------------------------------------------------------------------

  header <- dashboardHeader(title = "MangoViz")


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
        text = strong(textesUI[[lang]][textesUI$id == "accueil"]),
        tabName = "accueil",
        icon = icon("seedling")
      ),
      
      menuItem(
        text = strong(textesUI[[lang]][textesUI$id == "taille"]),
        tabName = "taille",
        menuSubItem(
          text = textesUI[[lang]][textesUI$id == "presentation"],
          tabName = "taille_presentation"
        ),
        menuSubItem(
          text = textesUI[[lang]][textesUI$id == "resultats"],
          tabName = "taille_resultats"
        ),
        icon = icon("cut")
      ),
      
      menuItem(
        text = strong(textesUI[[lang]][textesUI$id == "variete"]),
        tabName = "variete",
        menuSubItem(
          text = textesUI[[lang]][textesUI$id == "presentation"],
          tabName = "var_presentation"
        ),
        menuSubItem(
          text = textesUI[[lang]][textesUI$id == "resultats"],
          tabName = "var_resultats"
        ),
        radioButtons(
          inputId = "variete_mesure",
          label = "Mesure", # textui
          choices = unique(variete$Mesure) %>% setNames(c("Production", "Nombre de fruits", "Masse des fruits")) # vecteur nommé pour textui
          # individual = TRUE,
          # direction = "vertical",
          # checkIcon = list(
          #   yes = tags$i(class = "fa fa-circle", 
          #                style = "color: steelblue"), # changer couleur
          #   no = tags$i(class = "fa fa-circle-o", 
          #               style = "color: steelblue"))
        ),
        icon = icon("dna")
      ),
      
      # rassembler les 3 en un seul ?
      menuItem(
        text = strong(textesUI[[lang]][textesUI$id == "savoirplus"]),
        tabName = "savoirplus",
        icon = icon("book")
      )
      
      # menuItem(
      #   text = strong(textesUI[[lang]][textesUI$id == "remerciements"]),
      #   tabName = "remerciements",
      #   icon = icon("heart")
      # ),
      # 
      # menuItem(
      #   text = strong(textesUI[[lang]][textesUI$id == "contact"]),
      #   href = "mailto:mangoviz@cirad.fr", # à créer
      #   icon = icon("at")
      # )
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
      ))

      # Onglet essai taille ####
      , tabItem(tabName = "taille", fluidRow(
        
      )) # end of taille tab
      
      
      # Onglet évaluation variétale ####
      , tabItem(
        tabName = "var_presentation", 
        fluidRow(
          column(
            8, 
            box(
              title = "Verger de l'évaluation variétale", # textui
              width = 12, height = 570,
              status = "success",
              solidHeader = TRUE,
              includeMarkdown(sprintf("locale/verger-variete_%s.md", lang))
            ),
            box(
              title = "Information sur les variétés", # textui
              width = 12, height = 1100,
              status = "success",
              solidHeader = TRUE,
              radioButtons(
                inputId = "variete_radio_cultivar",
                label = "Choix de la variété", # textui
                choices = unique(variete$cultivar) %>% sort(),
                inline = TRUE
              ),
              imageOutput("variete_img") # A FAIRE : centrer l'image
            )
          ),
          column(
            4, 
            box(
              title = "Plan du verger", # textui
              width = 12,
              status = "success",
              solidHeader = TRUE,
              plotlyOutput("variete_parcelle", height = 600)
            )
          )
        )
      )


      , tabItem(tabName = "var_resultats", 
          
          
          
          fluidRow(
          column(6,
            box(
              title = "Comparaison des variétés", # textui
              width = 12,
              status = "success",
              solidHeader = TRUE,
              checkboxGroupButtons(
                "variete_checkbox_year",
                "Choix d'une ou plusieurs années", # textui
                choices = unique(variete$Annee),
                selected = unique(variete$Annee) # toutes les années sélectionées par défaut
              ),
              echarts4rOutput("variete_var")
            )
          ),
          column(6,
            box(
              title = "Suivi temporel", # textui
              width = 12,
              status = "success",
              solidHeader = TRUE,
              selectInput(
               "variete_multi_var",
               "Choix d'une ou plusieurs variétés", # vérifier si ça fonctionne avec le téléphone
               choices = levels(variete$cultivar),
               selected = "Caro",
               multiple = TRUE
              ),
              echarts4rOutput("variete_temporel")
            )
          )
          
        ),
        fluidRow(
          column(12,
            box(
              title = "Suivi spatial", # textui
              width = 12,
              status = "success",
              solidHeader = TRUE,
              # selectInput(
              # "variete_select_var",
              # "Choix de la variété", # textui
              # choices = levels(variete$cultivar),
              # selected = "Caro"
              # ),
              plotOutput("variete_spatial")
            )
          )
        )
      )
      
      
      # Onglet en savoir plus ####
      , tabItem(tabName = "savoirplus", fluidRow(
        div(includeMarkdown(sprintf("locale/savoirplus_%s.md", lang)), class = "markdown-tab")
      ))
      
      # # Onglet Remerciements ####
      # , tabItem(tabName = "remerciements", fluidRow(
      #   div(includeMarkdown(sprintf("locale/remerciements_%s.md", lang)), class = "markdown-tab")
      # ))

    ) # end of tabItems
  ) # end of Dashboardbody



# PAGE --------------------------------------------------------------------

  dashboardPage(header, sidebar, body, title = "MangoViz", skin = "green")
} # end of req
