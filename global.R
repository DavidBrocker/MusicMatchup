# Load Packages -------------------------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(spotifyr)
library(visNetwork)
library(shinydashboard)
library(purrr)
library(dplyr)
library(slickR)
library(stringr)
library(shinyWidgets)
library(shinyalert)

# API Stuff -----------------------------------------------------------------------------------
Sys.setenv(SPOTIFY_CLIENT_ID = 'Shh It is a secret!')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'Shh It is a secret!')
access_token <- get_spotify_access_token()
# First Degree Net ----------------------------------------------------------------------------
net <- function(artist){
  
  xx <- 
    search_spotify(artist, type = "artist") |> 
    filter(name == artist) |> 
    filter(popularity == max(popularity)) |> 
    mutate(image = map(images, ~.x[1,2]) |> unlist())

  xxx <- 
    xx |> 
    select(id) |> 
    get_related_artists() |> 
    mutate(from = artist,
           image = map(images, ~.x[1,1]) |> unlist()) |> 
    select(-followers.href, -type, -images,-href) 
  
  xxx <- 
    xxx |> 
    bind_rows(xx)
  
  nodes <- 
    tibble(
      id = xxx$name,
      value = xxx$followers.total,
      image = xxx$image,
      shape = "circularImage"
    )
  
  edges <- 
    tibble(
      from = xxx$from,
      to = xxx$name
    )
  
  visNetwork(nodes,edges, width = "100%") |> 
    visNodes(
      shadow = TRUE,
      color = 
        list(
          background = "#f5f6f7",
          border = "#F5F6F7",
          highlight = 
            list(
              background = "#f5f6f7",
              border = "#f5f6f7"
              )
          )
      ) |> 
    visEdges(
      color = 
        list(
          highlight = "#f5f6f7"),
             width = .5) |> 
    visOptions(
      nodesIdSelection = 
        list(enabled = TRUE,
             main = "Select Artist...")) 

}

# Interconnection Function -------------------------------------------------------------------------
inter_fun <- function(artist,level){
  if(level == 1) {
    xx <- 
      search_spotify(artist, type = "artist") |> 
      filter(name == artist) |> 
      filter(popularity == max(popularity)) |> 
      mutate(image = map(images, ~.x[1,2]) |> unlist())
    
    xxx <- 
      xx |> 
      select(id) |> 
      get_related_artists() |> 
      mutate(from = artist,
             image = map(images, ~.x[1,2]) |> unlist()) |> 
      select(-followers.href, -type, -images,-href) |> 
      bind_rows(xx)
    
    nodes <- 
      tibble(
        id = xxx$name,
        value = xxx$followers.total,
        image = xxx$image,
        shape = "circularImage"
      )
    
    edges <- 
      tibble(
        from = xxx$from,
        to = xxx$name
      )
    
    visNetwork(nodes,edges) 
    
  } else{
    fd <- 
      search_spotify(artist, type = "artist") |>
      filter(name == artist) |> 
      filter(popularity == max(popularity)) |> 
      select(id) |> 
      get_related_artists() |> 
      select(-followers.href,-type) |> 
      relocate(name, .before = genres) |> 
      mutate(from = artist) |> 
      mutate(image = map(images, ~.x[1,2]) |> unlist())
    
    sed <- 
      map_df(fd$id, get_related_artists) |> 
      select(-followers.href,-type) |> 
      relocate(name, .before = genres) |> 
      mutate(image = map(images, ~.x[1,2]) |> unlist())
    
    
    
    full <- 
      fd |> 
      bind_rows(sed) |> 
      mutate(from = c(fd$from,rep(fd$name,each = 20)))
    
    # Inter-Connection
    in_full <- 
      full |> 
      select(name,from,image,followers.total) |> 
      filter(name %in% from) |> 
      group_by(name) |> 
      unique()
    
    nodes_in <- 
      tibble(
        id = unique(in_full$name),
        image = unique(in_full$image),
        shape = "circularImage",
        value = unique(in_full$followers.total)
      )
    
    edges_in <- 
      tibble(
        from = in_full$from,
        to = in_full$name
      )
    
    visNetwork(nodes_in,edges_in)  |> 
      visNodes(shadow = TRUE,
               color = 
                 list(
                   background = "#f5f6f7",
                   border = "#F5F6F7",
                   highlight = 
                     list(
                       background = "#f5f6f7",
                       border = "#f5f6f7"
                     )
                 )
      ) |>
      visEdges(color = "black",
               width = .5) |>
      visOptions(
        highlightNearest =
          list(enabled = TRUE,
               labelOnly = FALSE,
               hideColor = "rgba(255,255,255,0)"),
        nodesIdSelection =
          list(enabled = TRUE,
               main = "Select Artist...")) |>
      visPhysics(enabled = FALSE) |> 
      visIgraphLayout(layout = "layout_in_circle")
  }
}
# Interactive Search --------------------------------------------------------------------------
artsrch <- function(artist){
  search_spotify(artist,"artist") |> 
    filter(!duplicated(name))
}
# Define Cards --------------------------------------------------------------------------------
# Visnetwork
card_vis <- 
  card(
    class = "net_card",
    card_header(
      "Related Artist Network",
      tooltip(
        bs_icon("diagram-3"),
        "20 Related Artists based on User Listening History",
        placement = "bottom")),
    card_body(
      visNetworkOutput("net"),
    )
  )

# Card 2: Searchbar
card_srch <- 
  card(
    class = "main_card",
    card_header(
      "MusicMatchup",
      popover(
        actionLink("link", bs_icon("music-note")),
        "Data provided from ",
        a("Spotify API", href = "https://developer.spotify.com/documentation/web-api"))
    ),
    card_body(
      class = "main_card_body",
        textInput(
          inputId = "artist",
          label =  NULL,
          placeholder = "Enter Artist",
          value = "",
          width = "25%"
        ),
          pickerInput(
            inputId = "art_dropdown",
            label = "",
            choices = "",
            inline = FALSE,
            width = "fit",
            options = 
              pickerOptions(
                noneResultsText = "No Matching Artists Found",
                noneSelectedText = "No Artist Searched"
              )
            ),
        actionBttn(
          inputId = "search",
          label = "Search",
          icon = icon("search"),
          size = "sm"
        ),
      )
  )

# Card 3 - Artist Name
card_name <- 
  card(
    class = "name_card",
    card_header("Artist Name"),
    card_body(
      uiOutput("art_name")
    ),
    card_footer(
      div(class = "rfsh",
        actionBttn(
          inputId = "search2",
          label = "Re-Roll",
          icon = icon("refresh"),
          size = "sm",
          color = "royal"
        ),
        tooltip(
          bs_icon("info-circle"),
          "Re-search with selected artist",
          placement = "right")
      )
    )
  )
# Card 4 - Artist Followers 
card_fol <- 
  card(
    class = "fol_card",
    card_header("Followers"),
    card_body(
      uiOutput("art_pop")
    )
  )

# Card 5 - Track Preview
card_pre <- 
  card(
    class = "pre_card",
    card_header("Top Track Preview"),
    card_body(
      uiOutput("art_pre")
    )
    # card_footer(
    #   splitLayout(
    #     cellWidths = c("75%","25%"),
    #     bs_icon(
    #       "person-fill-add",
    #       size = "2em",
    #       margin_left = "5%"),
    #     bs_icon(
    #       "plus-circle-fill",
    #       size = "2em",
    #       class = "text-success",
    #       margin_left = "10%")
    #     )
    #   )
  )

# Card 6 - Artist Genres
card_gen <- 
  card(
    class = "gen_card",
    card_header("Genres"),
    card_body(
      uiOutput("art_gen")
    )
    # card_footer(
    #   bs_icon(
    #     "plus-circle-fill",
    #     size = "2em",
    #     class = "text-suc <- cess",
    #     width = "100%")
    # )
  )
