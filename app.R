# -------------------------------------------------------------------------
# Load packages
# -------------------------------------------------------------------------
library(htmlwidgets)
library(dplyr)
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
# -------------------------------------------------------------------------
# Set Environment
# -------------------------------------------------------------------------
Sys.setenv(SPOTIFY_CLIENT_ID = '1f6c9bc7d5314065bb3467e9ff984084')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b1b24f99d46541769013cf20539b191e')
access_token <- get_spotify_access_token()

# -------------------------------------------------------------------------
# For Example -------------------------------------------------------------
# -------------------------------------------------------------------------
# Aesop Rock and Dessiderium: No Matches
ex1_nodes <- read.csv("https://raw.githubusercontent.com/DavidBrocker/Similarity2/main/Examples/ar_d_node.csv")
ex1_edge <- read.csv("https://raw.githubusercontent.com/DavidBrocker/Similarity2/main/Examples/ar_d_edge.csv")

# Allie X and Magdalena Bay: Matches
ex2_nodes <- read.csv("https://raw.githubusercontent.com/DavidBrocker/Similarity2/main/Examples/ax_mb_node.csv")
ex2_edge <- read.csv("https://raw.githubusercontent.com/DavidBrocker/Similarity2/main/Examples/ax_mb_edge.csv")
# -------------------------------------------------------------------------
# Establish Functions -----------------------------------------------------
# -------------------------------------------------------------------------
artist_similarity2 <- function(artist){
  # Get similar artists
  artist_sim <-
    # Need to use GAF to get artist_id
    get_artist_audio_features(artist,
                              include_groups = c("album", "single")) |>
    # Isolate id
    select(artist_id) |>
    # Get Unique value
    unique() |>
    # Pipe into get_related_artists
    get_related_artists() |> 
    # Unnest by the id column
    unnest(id, names_sep = "") |>
    # Make the id column contain the related artists for that id
    mutate(id = id |> map(get_related_artists)) |>
    # Get full list
    unnest(id, names_sep = "id") |> 
    mutate(original_artist = artist) |> 
    rename(parent_artist = name,
           related_artist = ididname,
           related_artist_genres = ididgenres,
           related_artist_followers.total = ididfollowers.total,
           related_artist_images = ididimages) |> 
    select(original_artist,parent_artist,genres,followers.total,
           related_artist, images,related_artist, related_artist_genres,
           related_artist_followers.total, related_artist_images)
}

# ---------------------------------------------------------------------------------------------
# Artist Similarity First Order Only ----------------------------------------------------------
# ---------------------------------------------------------------------------------------------
artist_similarity <- function(artist){
  # Get similar artists
  artist_sim <-
    # Need to use GAF to get artist_id
    get_artist_audio_features(artist,
                              include_groups = c("album", "single")) |>
    # Isolate id
    select(artist_id) |>
    # Get Unique value
    unique() |>
    # Pipe into get_related_artists
    get_related_artists() |> 
    mutate(original_artist = artist) |> 
    rename(parent_artist = name) |> 
    select(original_artist,parent_artist,genres,followers.total,
           images)
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
# ------------------------------------------------------------------------------
# Test for Similarity: Second Order --------------------------------------------
# ------------------------------------------------------------------------------
second_order <- function(artist) {
  artist |>
    select(original_artist, related_artist) |>
    group_by(related_artist) |>
    unique() |>
    summarize(isDup = n() > 1, n = n()) |>
    group_by(isDup) |>
    count() -> er_mgmt
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

# ---------------------------------------------------------------------------------------------
# Create Function: Get Main Artist Images -----------------------------------------------------
# ---------------------------------------------------------------------------------------------
two_pics <- function(x){
  print('begin function...')
  get_artist_audio_features(x,
                            include_groups = c("album","single")) |> 
    select(artist_id) |> 
    unique() |> 
    get_artists() |> 
    mutate(image = map(images,~.x[1,2]) |> unlist()) |>
    select(name,image) |> 
    rename(id = name)
}
# -------------------------------------------------------------------------
# Create Function: Artist Network Graphing --------------------------------
# -------------------------------------------------------------------------
artist_vis <- function(artist, tp) {
  # Create Node Dataframe from tp function
  node <- tp
  
  # Reformat with artist Dataframe
  node_2 <-
    artist |>
    select(parent_artist, images) |>
    mutate(image = map(images, ~ .x[1, 2]) |> unlist()) |>
    distinct(parent_artist, .keep_all = T) |>
    rename(id = parent_artist) |>
    select(-images)
  
  # Combine
  node <-
    node |>
    full_join(node_2)
  
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
              tags$a(href = "https://twitter.com/DaveBrocker", 
                     icon("twitter"), "", 
                     target = "_blank")),
      tags$li(class = "dropdown", 
              tags$a(href = "https://medium.com/@davidabrocker", 
                     icon("medium"), "", 
                     target = "_blank")),
      tags$li(class = "dropdown", 
              tags$a(href = "https://raw.githubusercontent.com/DavidBrocker/Similarity/main/app.R", 
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


# Server --------------------------------------------------------------------------------------
server <- function(input, output) {
  
  shinyalert(title = "Welcome!",
             text = "Get ready to discover new music with our app! 
             Simply enter two artists and voila! This app generates a network of 20 related artists for each based on Spotify data. Then, we'll show you if there's any overlap between the two networks. It's like a musical Venn diagram!",
             type = "info",
             closeOnEsc = TRUE,
             closeOnClickOutside = TRUE,
             confirmButtonText = "Got it!",
             size = "s",
             confirmButtonCol = "#4CD964")
  
  # Create Validator Object
  iv <- InputValidator$new()
  
  # Create Rule to check for: 'Artist1, Artist2'
  iv$add_rule(
    "input",
    sv_regex("\\w+,\\s\\w+", "Please try again")
  )
  
  iv$add_rule("input", function(string) {
    x <- 
      string |> 
      str_split(", ") |> 
      unlist()
    
    if (x[1] == x[2]){
      "Duplicate Values are not allowed"
    }
  }
)
  
  my_reactive <- reactive({input$input})

  observeEvent(input$click,{

    
    # Allow Input Validator to Run
    iv$enable()
    
    # Once input is valid, remove notification
    if(iv$is_valid()){
      removeNotification(id = "click")
    } else {
      # If Validator is not met, display message
      showNotification("Please enter in two distinct artists separated by a comma. e.g. 'Taylor Swift, Allie X'",
                       id = "click", 
                       type = "error", 
                       duration = NULL)
      req(iv$is_valid())
    }
    
    # Separate Artists
      artists <- 
        my_reactive() |> 
        str_split(", ") |> 
        unlist()
      
    # Create vector of artists
      artist <- artists
      
      # Get First Order Similarity
      # Create Progress Bar for first task
      
      withProgressShiny({
      incProgress(.25, message = "Separating artists and finding matches\n",
                  detail = "Pulling 40 Matches...")
      
      
      artists <- map_df(artists,artist_similarity)
      
      # Get Main Artist Images
      tp <- map_df(artist,two_pics)
      
      incProgress(.5, message = "Getting Artist Pictures",
                  detail = "Say Cheese!")
      
      # Get First Order Similarity
      artists1 <- first_order(artists)
      
      # Make First Order Menu
      output$first_similarity <- renderMenu({
        sidebarMenu(
          menuItem(
            paste0("First Order Similarity: ",
                   str_to_sentence(artists1$pct_alike)), 
            icon = icon("music"))
        )
      }) 
      
      # Set Reactive Nodes --------------------------------------------------------------------------
      
      # Create Reactive Value
      selected_nodes <- reactiveVal(0) 
      
      
      # Make Network Graph
      incProgress(.7, message = "Creating Artist Network",
                  detail = "Connecting the dots...")
    })
    
    output$network <- renderVisNetwork(
      artist_vis(artists,tp)
    ) 
    
    # Update Selected Node
    observeEvent(input$network_selected,{
      selected_nodes(input$network_selected)
      cat("Selected Nodes:", selected_nodes(), "\n")
    })
    
    # Search Artist Related to Node Selected
    artist_info <- reactive({
      req(selected_nodes() != '')
      search_spotify(selected_nodes(), 'artist') %>% 
        filter(!duplicated(name))
    })
    
    # 'Hard'-Code Selected Artist
    selected_artist <- reactive({
      req(nrow(artist_info()) > 0)
      artist_info() %>% 
        # Make Sure Name Matches Exactly
        filter(name == paste0(selected_nodes())) %>% 
        # Get Artist with Top Popularity
        filter(popularity == max(popularity))
    })
    
    # Get Artist Top Tracks
    artist_top_tracks <- eventReactive(selected_nodes(), {
      df <- get_artist_top_tracks(selected_artist()$id)
    })
    
    output$rendy <- renderUI({
      track_preview_url <-
        artist_top_tracks() |>
        select(preview_url) |>
        na.omit() |> 
        slice(1) |> 
        pull()
      
      print(track_preview_url[1])
      
      
      if (!is.na(track_preview_url[1])) {
        tagList(
          tags$audio(id = 'song_preview', 
                     src = track_preview_url, 
                     type = 'audio/mp3', 
                     autoplay = F, 
                     controls = NA),
          tags$script(JS("myAudio=document.getElementById('song_preview'); myAudio.play();")),
          tags$style(HTML("#song_preview { margin: 10px 20px; width: 75%;}")),
          tags$div(
            style = "margin: 10px 20px;color: #4CD964;overflow-wrap: break-word;",
            tags$h5(paste0("Now Playing: ", 
                           artist_top_tracks() |> 
                             select(name,preview_url) |> 
                             na.omit() |> 
                             slice(1) |> 
                             select(name))
            )
          ))
      } else {
        tags$div(
          style = "margin: 10px 20px; color: #9A031E",
          tags$h5('No preview for this artist on Spotify'))
      } 
    })
  })
  
  # Example Plot 1 ------------------------------------------------------------------------------
  output$exampleplot <- renderVisNetwork({
    visNetwork(ex1_nodes, ex1_edge) |>
      visNodes(
        shapeProperties = list(useBorderWithImage = TRUE),
        shadow = TRUE,
        font = list(color = "white")) |>
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = T) |>
      visEvents(type = "once",
                startStabilizing = "function() {
              this.moveTo({scale:0.3})}")
  })
  # Example Plot 2 ------------------------------------------------------------------------------
  output$example2plot <- renderVisNetwork({
    visNetwork(ex2_nodes, ex2_edge) |>
      visNodes(
        shapeProperties = list(useBorderWithImage = TRUE),
        shadow = TRUE,
        font = list(color = "white")) |>
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = T) |>
      visEvents(type = "once",
                startStabilizing = "function() {
              this.moveTo({scale:0.3})}")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
