# Server --------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  shinyalert(
    title = "Welcome to MusicMatchup!",
    text = "This tool can visualize
    your next favorite artist! Simply search and explore.",
    html = TRUE,
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    confirmButtonText = "Start!",
    imageUrl = "logo.png",
    imageWidth = 200,
    imageHeight = 200,
    className = "modal_al"
  )
  
  # Create Reactive for Artist
  art1 <- reactive({
    req(input$artist != '')
    artsrch(input$artist)
  })
  
  observeEvent(input$artist,{
    updatePickerInput(
      inputId = "art_dropdown",
      choices = art1()$name
    )
  })
  
  # Listen for Button Press
  pressed <- eventReactive(input$search, {
    input$art_dropdown
  })
  
  # Once Button is pressed...
  observeEvent(pressed(),{
    req(input$artist != "",cancelOutput = TRUE)
    # Convert reactive into variable
    artist <- pressed()
    
    # Create variable, selected nodes and initialize with no value
    selected_nodes <- reactiveVal("") 
    
    # Render Network
    output$net <- renderVisNetwork(
        net(artist)
    )
    
    # Once Node is selected, log that node as new selected_node value
    observeEvent(input$net_selected,{
      selected_nodes(input$net_selected)
    })
    
    # Zoom in to node selected
    observe({
      if(selected_nodes()!='') {
        visNetworkProxy("net") |>
          visFocus(id = selected_nodes(),
                   scale = 7)
      } else {
        visNetworkProxy("net") |>
          visFit()
      }
    })
    
    artist_info <- reactive({
      req(selected_nodes() !='')
      # Old Way using Literal Name
      search_spotify(selected_nodes(),type = "artist") |>
        filter(name == selected_nodes()) |> 
        filter(popularity == max(popularity))
    })
    
    output$art_name <- renderUI({
      an <- as.character(selected_nodes())
    
      h1(an,class = "display-6")
    })
    
    output$art_pop <- renderUI({
      h1(
        prettyNum(paste0(artist_info()$followers.total),
                  big.mark = ","),
        class = "display-6"
      )
    })
    
    output$art_pre <- renderUI({
      tt <- get_artist_top_tracks(artist_info()$id)
      tags$iframe(
        src = 
          paste0(
            "https://open.spotify.com/embed/track/", 
            sample(tt$id,1), 
            "?utm_source=generator?"
            ),
        width = "100%",
        height = "100%",
        fullscreen = TRUE
      )
    })
    
    output$art_gen <- renderUI({
      gen <- 
        artist_info() |> 
        select(genres) |> 
        unlist() |> 
        str_c() |> 
        str_to_title() |> 
        str_flatten(collapse = "<br>")
      HTML(paste0("<h2>",gen,"</h2>"))
    })
    
    observeEvent(input$search2,{
      updateTextInput(inputId = "artist",
                      value = selected_nodes())
    })
    
  })
}

shinyApp(ui, server)
