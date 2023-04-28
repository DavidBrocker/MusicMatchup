# -------------------------------------------------------------------------
# Define UI 
# -------------------------------------------------------------------------
ui <-
  dashboardPage(
    title = "Music Matchup",
    skin = "midnight",
    dashboardHeader(
      titleWidth = "300px",
      tags$li(class = "dropdown", 
              tags$a(href = "https://twitter.com/DaveBrocker", 
                     icon("twitter"), "", 
                     target = "_blank")),
      tags$li(class = "dropdown", 
              tags$a(href = "https://medium.com/@davidabrocker", 
                     icon("medium"), "", 
                     target = "_blank")),
      tags$li(class = "dropdown", 
              tags$a(href = "https://github.com/DavidBrocker/MusicMatchup", 
                     icon("github"), "", 
                     target = "_blank")),
      title = span(
        "Music Matchup",
        tags$img(
          src = "image1.png",
          height = '30',
          width = '30'
        ),
      )),
    dashboardSidebar(
      collapsed = F,
      width = 300,
      sidebarMenu(
        sidebarSearchForm(
          textId = "input",
          buttonId = "click",
          label = "Artist1, Artist2",
          icon = icon("magnifying-glass")
        ),
        menuItem("Artist Similarity", tabName = "main", icon = icon('copy')),
        menuItem("Examples", icon = icon("chart-simple"), tabName = "Example", startExpanded = TRUE,
                 menuSubItem("Example with No Matches",tabName = "Example1",icon=icon("triangle-exclamation")),
                 menuSubItem("Example with Matches",tabName = "Example2",icon=icon("circle-nodes"))
        ),
        tags$style(".fa-code {color:#FFCC00}
                    .fa-copy {color:#4CD964}
                    .fa-triangle-exclamation {color: #FFCC00}
                    .fa-circle-nodes {color:#007AFF}
                    .fa-chart-simple {color:#007AFF}
                    .fa-medium {color:#F2F2F2}
                    .fa-twitter {color:#1DA1F2}
                    .fa-thumbs-up {color: #F2F2F2}"),
        menuItemOutput("first_similarity"),
        useShinyjs(),
        uiOutput("rendy"),
        uiOutput("rendyex2")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem("main",
                visNetworkOutput("network",
                                 height = "900px")),
        tabItem("Example1",
                visNetworkOutput("exampleplot",
                                 height = "900px")),
        tabItem("Example2",
                visNetworkOutput("example2plot",
                                 height = "900px"))
      ),
      tags$head(tags$style(".shiny-notification {color: white; position: fixed; width: 280px; background:#272c30; bottom: 2.5%; right: 2.5%}"))
    )
  )
