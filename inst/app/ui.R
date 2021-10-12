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
      }

      # Tabs
      , lapply(seq(along.with = onglets$id), function(tab) {
        menuItem(
          text = strong(onglets[[lang]][tab]),
          tabName = onglets$id[tab],
          icon = icon(onglets$icon[tab])
        )
      }),
      menuItem(
        text = strong(textesUI[[lang]][textesUI$id == "contact"]),
        href = "mailto:mangoviz@cirad.fr", # CHANGER
        icon = icon("at")
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
      
      tabItem(tabName = "presentation", fluidRow(
        div(includeMarkdown(sprintf("locale/accueil_%s.md", lang)), class = "markdown-tab")
      ))

      # Onglet essai taille ####
      , tabItem(tabName = "taille", fluidRow(
        
      )) # end of taille tab
      
      # Onglet évaluation variétale ####
      , tabItem(tabName = "variete", fluidRow(
        column(
          6, 
          box(
            title = "Présentation", # textui
            width = 12,
            status = "success",
            solidHeader = TRUE,
            "texte pour décrire le protocole expérimental",
            plotlyOutput("variete_parcelle")
          ),
          
          # où le mettre ?
          radioGroupButtons(
            inputId = "variete_mesure",
            label = "Mesure", # textui
            choices = unique(variete$Mesure), # vecteur nommé pour textui
            individual = TRUE,
            checkIcon = list(
              yes = tags$i(class = "fa fa-circle", 
                           style = "color: steelblue"), # changer couleur
              no = tags$i(class = "fa fa-circle-o", 
                          style = "color: steelblue"))
          ),
          
          
          box(
            title = "Suivi spatial", # textui
            width = 12,
            status = "success",
            solidHeader = TRUE,
            selectInput(
              "variete_select_var",
              "Choix de la variété", # textui
              choices = levels(variete$cultivar),
              selected = variete$cultivar[1] # temporaire
            ),
            echarts4rOutput("variete_spatial")
          )
        ),
        column(
          6, 
          box(
            title = "Comparaison des variétés", # textui
            width = 12,
            status = "success",
            solidHeader = TRUE,
            checkboxGroupButtons(
              "variete_checkbox_year",
              "Choix d'une ou plusieurs années", # textui
              choices = unique(variete$Annee),
              selected = variete$Annee[1]
            ),
            echarts4rOutput("variete_var")
          ),
          box(
            title = "Suivi temporel", # textui
            width = 12,
            status = "success",
            solidHeader = TRUE,
            multiInput(
              "variete_multi_var",
              "Choix d'une ou plusieurs variétés",
              choices = levels(variete$cultivar)
            ),
            echarts4rOutput("variete_temporel")
          )
        )
      ))
      
      
      # Onglet en savoir plus ####
      , tabItem(tabName = "savoirplus", fluidRow(
        div(includeMarkdown(sprintf("locale/savoirplus_%s.md", lang)), class = "markdown-tab")
      ))
      
      # Onglet Remerciements ####
      , tabItem(tabName = "remerciements", fluidRow(
        div(includeMarkdown(sprintf("locale/remerciements_%s.md", lang)), class = "markdown-tab")
      ))

    ) # end of tabItems
  ) # end of Dashboardbody



# PAGE --------------------------------------------------------------------

  dashboardPage(header, sidebar, body, title = "MangoViz", skin = "green")
} # end of req
