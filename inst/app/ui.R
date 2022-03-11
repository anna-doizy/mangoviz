#### UI ####

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinycssloaders)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(ggiraph)
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
      ))

      # Onglet essai taille ####
      , tabItem(tabName = "taille", fluidRow(
        
      )), # end of taille tab
      
      
      # Onglet évaluation variétale ####
      ## Le verger ####
      tabItem(
        tabName = "var_presentation", 
        fluidRow(
          column(
            8, 
            box(
              title = textesUI[textesUI$id == "variete_pres_box", lang], 
              width = 12, #height = 570,
              status = "success",
              solidHeader = TRUE,
              includeMarkdown(sprintf("locale/verger-variete_%s.md", lang)),
              
              # test sur une figure
              # HTML('<figure>
              #   <img src="logo-eu.jpg" alt="Image 1">
              #   <figcaption>Image 1</figcaption>
              # </figure>'),
              
              # A FAIRE : mettre en radio buttons...
              # apply(cultivar_desc,1, function(var) {
              #   paste0('<figure>
              #     <img src="varietes/', var["cultivar"],'.JPG" alt="', var[paste0("desc_", lang)],'">
              #     <figcaption>', var[paste0("desc_", lang)],'</figcaption>
              #     </figure>') 
              #   }) %>% 
              #   paste(collapse = "\n") %>% 
              #   HTML()
              
              
              
              radioButtons(
                inputId = "variete_radio_desc",
                label = textesUI[textesUI$id == "variete_bilan_label", lang],
                choices = levels(variete$cultivar) %>% str_to_lower() %>% str_replace_all(" ", "_") %>% setNames(levels(variete$cultivar)),
                inline = TRUE
              ),
              
              uiOutput("variete_ui_desc"),
              imageOutput("variete_img_desc")
              
            )#,
            # box(
            #   title = "Information sur les variétés",
            #   width = 12, height = 1100,
            #   status = "success",
            #   solidHeader = TRUE,
            #   radioButtons(
            #     inputId = "variete_radio_cultivar",
            #     label = "Choix de la variété",
            #     choices = unique(variete$cultivar) %>% sort(),
            #     inline = TRUE
            #   ),
            #   imageOutput("variete_img") # A FAIRE : centrer l'image
            # )
          ),
          column(
            4, 
            box(
              title = textesUI[textesUI$id == "variete_plan_box", lang],
              width = 12,
              status = "success",
              solidHeader = TRUE,
              girafeOutput("variete_parcelle", height = 600)
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
                choices = unique(variete$Mesure) %>% setNames(c(textesUI[textesUI$id == "masse", lang], textesUI[textesUI$id == "nbfruit", lang], textesUI[textesUI$id == "masse_fruit", lang])),
                inline = TRUE
              )
            )
          )
        ),
        
        fluidRow(
          column(6,
            box(
              title = textesUI[textesUI$id == "variete_comp_box", lang],
              width = 12,
              status = "success",
              solidHeader = TRUE,
              p(em(textesUI[textesUI$id == "variete_comp_text", lang])),
              checkboxGroupButtons(
                "variete_checkbox_year",
                textesUI[textesUI$id == "variete_comp_label", lang],
                status = "danger",
                choices = unique(variete$Annee),
                selected = unique(variete$Annee) # toutes les années sélectionées par défaut
              ),
              girafeOutput("variete_var") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
            )
          ),
          column(6,
            box(
              title = textesUI[textesUI$id == "variete_temps_box", lang],
              width = 12,
              status = "success",
              solidHeader = TRUE,
              p(em(textesUI[textesUI$id == "variete_temps_text", lang])),
              selectInput(
               "variete_multi_var",
               textesUI[textesUI$id == "variete_temps_label", lang],
               choices = levels(variete$cultivar),
               selected = "Caro",
               multiple = TRUE
              ),
              girafeOutput("variete_temporel") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
            )
          )
        
        ),
        
        fluidRow(
          column(12,
            box(
              title = textesUI[textesUI$id == "variete_spatial_box", lang],
              width = 12,
              status = "success",
              solidHeader = TRUE,
              p(em(textesUI[textesUI$id == "variete_spatial_text", lang])),
              # selectInput(
              # "variete_select_var",
              # "Choix de la variété",
              # choices = levels(variete$cultivar),
              # selected = "Caro"
              # ),
              girafeOutput("variete_spatial") %>% withSpinner(type = 7, color = "black", hide.ui = FALSE)
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
                choices = levels(variete$cultivar) %>% str_to_lower() %>% str_replace_all(" ", "_") %>% setNames(levels(variete$cultivar)),
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
