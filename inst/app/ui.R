#### UI ####

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyhelper)
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
