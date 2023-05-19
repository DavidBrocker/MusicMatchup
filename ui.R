# -------------------------------------------------------------------------
# Load packages
# -------------------------------------------------------------------------
library(htmlwidgets)
library(dplyr)
library(shinydisconnect)
library(tidyr)
library(purrr)
library(spotifyr)
library(shiny)
library(shinyjs)
library(stringr)
library(shinydashboard)
library(ggraph)
library(igraph)
library(progressr)
library(shinyalert)
library(shinybrowser)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyscreenshot)
library(shinyvalidate)
library(fontawesome)
library(visNetwork)
library(bslib)
# -------------------------------------------------------------------------
# Set Environment
# -------------------------------------------------------------------------
Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxxxxxx')
access_token <- get_spotify_access_token()

# -------------------------------------------------------------------------
# For Example -------------------------------------------------------------
# -------------------------------------------------------------------------
# Aesop Rock and Dessiderium: No Matches
ex1_nodes <- read.csv("https://raw.githubusercontent.com/DavidBrocker/MusicMatchup/main/Examples/ar_d_node.csv")
ex1_edge <- read.csv("https://raw.githubusercontent.com/DavidBrocker/MusicMatchup/main/Examples/ar_d_edge.csv")

# Allie X and Magdalena Bay: Matches
ex2_nodes <- read.csv("https://raw.githubusercontent.com/DavidBrocker/MusicMatchup/main/Examples/ax_mb_node.csv")
ex2_edge <- read.csv("https://raw.githubusercontent.com/DavidBrocker/MusicMatchup/main/Examples/ax_mb_edge.csv")


# ---------------------------------------------------------------------------------------------
# Artist Similarity----------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
artistSimilarity <- function(artist){
  # Get similar artists
  main_art <-
    search_spotify(artist, type = "artist") |> 
    filter(!duplicated(name)) |> 
    filter(name == artist) |> 
    filter(popularity == max(popularity))
  
  # Get Main Artist Pictures Save for Later
  tp <- 
    main_art |> 
    mutate(image = map(images,~.x[1,2]) |> unlist()) |>
    select(name,image) |> 
    rename(id = name)
  
  # Pipe into get_related_artists
  artist_sim <- 
    main_art |> 
    select(id) |> 
    get_related_artists() |> 
    mutate(original_artist = artist) |> 
    rename(parent_artist = name) |> 
    mutate(image = map(images, ~.x[1,2]) |> unlist()) |> 
    select(original_artist,parent_artist,genres,followers.total,
           image)
  
  mget(ls())
}

# ------------------------------------------------------------------------------
# Test for Similarity: First Order ---------------------------------------------
# ------------------------------------------------------------------------------
first_order <- function(artist) {
  artist <-
    artist |>
    select(original_artist, parent_artist) |>
    group_by(original_artist) |>
    unique() |>
    group_by(parent_artist) |>
    summarise(isDup = n() > 1,
              n = n()) |>
    group_by(isDup) |>
    count() -> er_mgmt
  er_mgmt
  if (length(er_mgmt[er_mgmt$isDup == TRUE]) < 1) {
    "0%" -> pct_alike
  } else {
    er_mgmt |>
      ungroup() |>
      mutate(alike = n / sum(n) * 100) |>
      filter(isDup == "TRUE") |>
      select(alike) |>
      as.numeric() |>
      round(2) |>
      paste0("%") -> pct_alike
  }
  mget(ls())
}

# -------------------------------------------------------------------------
# Create Function: Artist Network Graphing --------------------------------
# -------------------------------------------------------------------------
artistVis <- function(artist,tp) {
  # Create Node
  node <- 
    tp |> 
    distinct(id,.keep_all = T)
  
  # Reformat with artist Dataframe
  node_2 <-
    artist |>
    select(parent_artist, image) |> 
    rename(id = parent_artist) 
  
  # Combine
  node <-
    node |>
    full_join(node_2) |> 
    distinct(id,.keep_all = T)
  
  # Create Node Label
  node$label <- node$id
  
  print(node)
  
  # Create Node Shape
  node$shape <- "circularImage"
  
  node$color <- "white"
  
  # Create Edge Label
  edge <-
    artist |>
    select(original_artist, parent_artist) |>
    unique() |>
    rename(from = original_artist,
           to = parent_artist)
  
  visNetwork(node, edge) |>
    visNodes(shapeProperties = list(useBorderWithImage = TRUE),
             font = list(color = "white")) |> 
    visOptions(nodesIdSelection = T) |> 
    visEvents(type = "once", startStabilizing = "function() {
            this.moveTo({scale:0.3})}")
}


# Validation Function -------------------------------------------------------------------------
art_check <- function(x){
  str_detect(x,"\\w+,\\s\\w+")
}

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
              tags$a(href = "https://twitter.com/DaveBrocker", icon("twitter"), "", target = "_blank")),
      tags$li(class = "dropdown", tags$a(href = "https://medium.com/@davidabrocker",icon("medium"), "", target = "_blank")),
      tags$li(class = "dropdown", 
              tags$a(href = "https://github.com/DavidBrocker/MusicMatchup", icon("github"), "", target = "_blank")),
      title = span(
        "Music Matchup",
        tags$img(src = "image1.png", height = '30', width = '30'),
        )
      ),
    dashboardSidebar(
      collapsed = F,
      width = 300,
      sidebarMenu(style = "white-space: normal;",
        sidebarSearchForm(
          textId = "input",
          buttonId = "click",
          label = "Artist1, Artist2",
          icon = icon("magnifying-glass")
        ),
        menuItem("Artist Similarity", 
                 tabName = "main", 
                 icon = icon('clone')),
        menuItem("Examples", 
                 icon = icon("sitemap"), 
                 tabName = "Example", 
                 startExpanded = TRUE,
                 menuSubItem("No Matches",
                             tabName = "Example1",
                             icon=icon("link-slash")),
                 menuSubItem("Matches",
                             tabName = "Example2",
                             icon=icon("network-wired"))
        ),
        tags$style(".fa-clone {color:#1B5E20}
                    .fa-copy {color:green}
                    .fa-link-slash {color: white}
                    .fa-network-wired {color:white}
                    .fa-sitemap {color: #1B5E20}
                    .fa-medium {color:#F2F2F2}
                    .fa-twitter {color:#1DA1F2}
                    .fa-calculator {color: white}"),
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
      tags$head(
        tags$style(
          ".shiny-notification {
                   color: white;
                   position: fixed;
                   width: 280px;
                   background:#272c30;
                   bottom: 2.5%;
                   right: 2.5%;
                   }
          #nodeSelectexampleplot.dropdown {
                   background-color: #272C30;
                   -webkit-appearance: none;
                   background: url(data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+PHN2ZyAgIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIgICB4bWxuczpjYz0iaHR0cDovL2NyZWF0aXZlY29tbW9ucy5vcmcvbnMjIiAgIHhtbG5zOnJkZj0iaHR0cDovL3d3dy53My5vcmcvMTk5OS8wMi8yMi1yZGYtc3ludGF4LW5zIyIgICB4bWxuczpzdmc9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiAgIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgICB4bWxuczpzb2RpcG9kaT0iaHR0cDovL3NvZGlwb2RpLnNvdXJjZWZvcmdlLm5ldC9EVEQvc29kaXBvZGktMC5kdGQiICAgeG1sbnM6aW5rc2NhcGU9Imh0dHA6Ly93d3cuaW5rc2NhcGUub3JnL25hbWVzcGFjZXMvaW5rc2NhcGUiICAgaWQ9IkxheWVyXzEiICAgZGF0YS1uYW1lPSJMYXllciAxIiAgIHZpZXdCb3g9IjAgMCA0Ljk1IDEwIiAgIHZlcnNpb249IjEuMSIgICBpbmtzY2FwZTp2ZXJzaW9uPSIwLjkxIHIxMzcyNSIgICBzb2RpcG9kaTpkb2NuYW1lPSJkb3dubG9hZC5zdmciPiAgPG1ldGFkYXRhICAgICBpZD0ibWV0YWRhdGE0MjAyIj4gICAgPHJkZjpSREY+ICAgICAgPGNjOldvcmsgICAgICAgICByZGY6YWJvdXQ9IiI+ICAgICAgICA8ZGM6Zm9ybWF0PmltYWdlL3N2Zyt4bWw8L2RjOmZvcm1hdD4gICAgICAgIDxkYzp0eXBlICAgICAgICAgICByZGY6cmVzb3VyY2U9Imh0dHA6Ly9wdXJsLm9yZy9kYy9kY21pdHlwZS9TdGlsbEltYWdlIiAvPiAgICAgIDwvY2M6V29yaz4gICAgPC9yZGY6UkRGPiAgPC9tZXRhZGF0YT4gIDxzb2RpcG9kaTpuYW1lZHZpZXcgICAgIHBhZ2Vjb2xvcj0iI2ZmZmZmZiIgICAgIGJvcmRlcmNvbG9yPSIjNjY2NjY2IiAgICAgYm9yZGVyb3BhY2l0eT0iMSIgICAgIG9iamVjdHRvbGVyYW5jZT0iMTAiICAgICBncmlkdG9sZXJhbmNlPSIxMCIgICAgIGd1aWRldG9sZXJhbmNlPSIxMCIgICAgIGlua3NjYXBlOnBhZ2VvcGFjaXR5PSIwIiAgICAgaW5rc2NhcGU6cGFnZXNoYWRvdz0iMiIgICAgIGlua3NjYXBlOndpbmRvdy13aWR0aD0iMTkyMCIgICAgIGlua3NjYXBlOndpbmRvdy1oZWlnaHQ9IjEwMjciICAgICBpZD0ibmFtZWR2aWV3NDIwMCIgICAgIHNob3dncmlkPSJmYWxzZSIgICAgIGlua3NjYXBlOnpvb209Ijg0LjMiICAgICBpbmtzY2FwZTpjeD0iMi40NzQ5OTk5IiAgICAgaW5rc2NhcGU6Y3k9IjUiICAgICBpbmtzY2FwZTp3aW5kb3cteD0iMTkyMCIgICAgIGlua3NjYXBlOndpbmRvdy15PSIyNyIgICAgIGlua3NjYXBlOndpbmRvdy1tYXhpbWl6ZWQ9IjEiICAgICBpbmtzY2FwZTpjdXJyZW50LWxheWVyPSJMYXllcl8xIiAvPiAgPGRlZnMgICAgIGlkPSJkZWZzNDE5MCI+ICAgIDxzdHlsZSAgICAgICBpZD0ic3R5bGU0MTkyIj4uY2xzLTJ7ZmlsbDojNDQ0O308L3N0eWxlPiAgPC9kZWZzPiAgPHRpdGxlICAgICBpZD0idGl0bGU0MTk0Ij5hcnJvd3M8L3RpdGxlPiAgPHBvbHlnb24gICAgIGNsYXNzPSJjbHMtMiIgICAgIHBvaW50cz0iMy41NCA1LjMzIDIuNDggNi44MiAxLjQxIDUuMzMgMy41NCA1LjMzIiAgICAgaWQ9InBvbHlnb240MTk4IiAgICAgc3R5bGU9ImZpbGw6I2ZmZmZmZjtmaWxsLW9wYWNpdHk6MSIgLz48L3N2Zz4=) no-repeat 101% 50%;
                   padding-right:10px;
                   padding-left: 10px;
                   }
          #nodeSelectexample2plot.dropdown {
                   background-color: #272C30;
                   -webkit-appearance: none;
                   background: url(data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+PHN2ZyAgIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIgICB4bWxuczpjYz0iaHR0cDovL2NyZWF0aXZlY29tbW9ucy5vcmcvbnMjIiAgIHhtbG5zOnJkZj0iaHR0cDovL3d3dy53My5vcmcvMTk5OS8wMi8yMi1yZGYtc3ludGF4LW5zIyIgICB4bWxuczpzdmc9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiAgIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgICB4bWxuczpzb2RpcG9kaT0iaHR0cDovL3NvZGlwb2RpLnNvdXJjZWZvcmdlLm5ldC9EVEQvc29kaXBvZGktMC5kdGQiICAgeG1sbnM6aW5rc2NhcGU9Imh0dHA6Ly93d3cuaW5rc2NhcGUub3JnL25hbWVzcGFjZXMvaW5rc2NhcGUiICAgaWQ9IkxheWVyXzEiICAgZGF0YS1uYW1lPSJMYXllciAxIiAgIHZpZXdCb3g9IjAgMCA0Ljk1IDEwIiAgIHZlcnNpb249IjEuMSIgICBpbmtzY2FwZTp2ZXJzaW9uPSIwLjkxIHIxMzcyNSIgICBzb2RpcG9kaTpkb2NuYW1lPSJkb3dubG9hZC5zdmciPiAgPG1ldGFkYXRhICAgICBpZD0ibWV0YWRhdGE0MjAyIj4gICAgPHJkZjpSREY+ICAgICAgPGNjOldvcmsgICAgICAgICByZGY6YWJvdXQ9IiI+ICAgICAgICA8ZGM6Zm9ybWF0PmltYWdlL3N2Zyt4bWw8L2RjOmZvcm1hdD4gICAgICAgIDxkYzp0eXBlICAgICAgICAgICByZGY6cmVzb3VyY2U9Imh0dHA6Ly9wdXJsLm9yZy9kYy9kY21pdHlwZS9TdGlsbEltYWdlIiAvPiAgICAgIDwvY2M6V29yaz4gICAgPC9yZGY6UkRGPiAgPC9tZXRhZGF0YT4gIDxzb2RpcG9kaTpuYW1lZHZpZXcgICAgIHBhZ2Vjb2xvcj0iI2ZmZmZmZiIgICAgIGJvcmRlcmNvbG9yPSIjNjY2NjY2IiAgICAgYm9yZGVyb3BhY2l0eT0iMSIgICAgIG9iamVjdHRvbGVyYW5jZT0iMTAiICAgICBncmlkdG9sZXJhbmNlPSIxMCIgICAgIGd1aWRldG9sZXJhbmNlPSIxMCIgICAgIGlua3NjYXBlOnBhZ2VvcGFjaXR5PSIwIiAgICAgaW5rc2NhcGU6cGFnZXNoYWRvdz0iMiIgICAgIGlua3NjYXBlOndpbmRvdy13aWR0aD0iMTkyMCIgICAgIGlua3NjYXBlOndpbmRvdy1oZWlnaHQ9IjEwMjciICAgICBpZD0ibmFtZWR2aWV3NDIwMCIgICAgIHNob3dncmlkPSJmYWxzZSIgICAgIGlua3NjYXBlOnpvb209Ijg0LjMiICAgICBpbmtzY2FwZTpjeD0iMi40NzQ5OTk5IiAgICAgaW5rc2NhcGU6Y3k9IjUiICAgICBpbmtzY2FwZTp3aW5kb3cteD0iMTkyMCIgICAgIGlua3NjYXBlOndpbmRvdy15PSIyNyIgICAgIGlua3NjYXBlOndpbmRvdy1tYXhpbWl6ZWQ9IjEiICAgICBpbmtzY2FwZTpjdXJyZW50LWxheWVyPSJMYXllcl8xIiAvPiAgPGRlZnMgICAgIGlkPSJkZWZzNDE5MCI+ICAgIDxzdHlsZSAgICAgICBpZD0ic3R5bGU0MTkyIj4uY2xzLTJ7ZmlsbDojNDQ0O308L3N0eWxlPiAgPC9kZWZzPiAgPHRpdGxlICAgICBpZD0idGl0bGU0MTk0Ij5hcnJvd3M8L3RpdGxlPiAgPHBvbHlnb24gICAgIGNsYXNzPSJjbHMtMiIgICAgIHBvaW50cz0iMy41NCA1LjMzIDIuNDggNi44MiAxLjQxIDUuMzMgMy41NCA1LjMzIiAgICAgaWQ9InBvbHlnb240MTk4IiAgICAgc3R5bGU9ImZpbGw6I2ZmZmZmZjtmaWxsLW9wYWNpdHk6MSIgLz48L3N2Zz4=) no-repeat 101% 50%;
                   padding-right:10px;
                   padding-left: 10px;
                   }
          #nodeSelectnetwork.dropdown {
                   background-color: #272C30;
                   -webkit-appearance: none;
                   background: url(data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+PHN2ZyAgIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIgICB4bWxuczpjYz0iaHR0cDovL2NyZWF0aXZlY29tbW9ucy5vcmcvbnMjIiAgIHhtbG5zOnJkZj0iaHR0cDovL3d3dy53My5vcmcvMTk5OS8wMi8yMi1yZGYtc3ludGF4LW5zIyIgICB4bWxuczpzdmc9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiAgIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgICB4bWxuczpzb2RpcG9kaT0iaHR0cDovL3NvZGlwb2RpLnNvdXJjZWZvcmdlLm5ldC9EVEQvc29kaXBvZGktMC5kdGQiICAgeG1sbnM6aW5rc2NhcGU9Imh0dHA6Ly93d3cuaW5rc2NhcGUub3JnL25hbWVzcGFjZXMvaW5rc2NhcGUiICAgaWQ9IkxheWVyXzEiICAgZGF0YS1uYW1lPSJMYXllciAxIiAgIHZpZXdCb3g9IjAgMCA0Ljk1IDEwIiAgIHZlcnNpb249IjEuMSIgICBpbmtzY2FwZTp2ZXJzaW9uPSIwLjkxIHIxMzcyNSIgICBzb2RpcG9kaTpkb2NuYW1lPSJkb3dubG9hZC5zdmciPiAgPG1ldGFkYXRhICAgICBpZD0ibWV0YWRhdGE0MjAyIj4gICAgPHJkZjpSREY+ICAgICAgPGNjOldvcmsgICAgICAgICByZGY6YWJvdXQ9IiI+ICAgICAgICA8ZGM6Zm9ybWF0PmltYWdlL3N2Zyt4bWw8L2RjOmZvcm1hdD4gICAgICAgIDxkYzp0eXBlICAgICAgICAgICByZGY6cmVzb3VyY2U9Imh0dHA6Ly9wdXJsLm9yZy9kYy9kY21pdHlwZS9TdGlsbEltYWdlIiAvPiAgICAgIDwvY2M6V29yaz4gICAgPC9yZGY6UkRGPiAgPC9tZXRhZGF0YT4gIDxzb2RpcG9kaTpuYW1lZHZpZXcgICAgIHBhZ2Vjb2xvcj0iI2ZmZmZmZiIgICAgIGJvcmRlcmNvbG9yPSIjNjY2NjY2IiAgICAgYm9yZGVyb3BhY2l0eT0iMSIgICAgIG9iamVjdHRvbGVyYW5jZT0iMTAiICAgICBncmlkdG9sZXJhbmNlPSIxMCIgICAgIGd1aWRldG9sZXJhbmNlPSIxMCIgICAgIGlua3NjYXBlOnBhZ2VvcGFjaXR5PSIwIiAgICAgaW5rc2NhcGU6cGFnZXNoYWRvdz0iMiIgICAgIGlua3NjYXBlOndpbmRvdy13aWR0aD0iMTkyMCIgICAgIGlua3NjYXBlOndpbmRvdy1oZWlnaHQ9IjEwMjciICAgICBpZD0ibmFtZWR2aWV3NDIwMCIgICAgIHNob3dncmlkPSJmYWxzZSIgICAgIGlua3NjYXBlOnpvb209Ijg0LjMiICAgICBpbmtzY2FwZTpjeD0iMi40NzQ5OTk5IiAgICAgaW5rc2NhcGU6Y3k9IjUiICAgICBpbmtzY2FwZTp3aW5kb3cteD0iMTkyMCIgICAgIGlua3NjYXBlOndpbmRvdy15PSIyNyIgICAgIGlua3NjYXBlOndpbmRvdy1tYXhpbWl6ZWQ9IjEiICAgICBpbmtzY2FwZTpjdXJyZW50LWxheWVyPSJMYXllcl8xIiAvPiAgPGRlZnMgICAgIGlkPSJkZWZzNDE5MCI+ICAgIDxzdHlsZSAgICAgICBpZD0ic3R5bGU0MTkyIj4uY2xzLTJ7ZmlsbDojNDQ0O308L3N0eWxlPiAgPC9kZWZzPiAgPHRpdGxlICAgICBpZD0idGl0bGU0MTk0Ij5hcnJvd3M8L3RpdGxlPiAgPHBvbHlnb24gICAgIGNsYXNzPSJjbHMtMiIgICAgIHBvaW50cz0iMy41NCA1LjMzIDIuNDggNi44MiAxLjQxIDUuMzMgMy41NCA1LjMzIiAgICAgaWQ9InBvbHlnb240MTk4IiAgICAgc3R5bGU9ImZpbGw6I2ZmZmZmZjtmaWxsLW9wYWNpdHk6MSIgLz48L3N2Zz4=) no-repeat 101% 50%;
                   padding-right:10px;
                   padding-left: 10px;
                   }")
        ),
      disconnectMessage(
        text = "Your session has timed out.",
        refresh = "Refresh",
        refreshColour = "#4CD964",
        background = "#272C30",
        size = 36,
        width = "full",
        top = "center",
        colour = "white",
        overlayColour = "#4CD964",
        overlayOpacity = .3
      )
      )
    )
