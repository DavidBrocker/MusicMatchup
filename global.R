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
Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxxxx')
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