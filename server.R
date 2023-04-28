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
