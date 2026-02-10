library(googlesheets4)
library(shiny)
SHEET_ID <- "https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/"

cache <- reactiveValues(
  pokemon = NULL,
  moves = NULL,
  pokemon_moves = NULL,
  last_refresh = Sys.time()
)

refresh_data <- function() {
  cache$pokemon       <- read_sheet(SHEET_ID, "pokemon")
  cache$moves         <- read_sheet(SHEET_ID, "moves")
  cache$pokemon_moves <- read_sheet(SHEET_ID, "pokemon_moves")
  cache$last_refresh  <- Sys.time()
}


ui <- fluidPage(
  titlePanel("Pokémon Move Manager"),

  sidebarLayout(
    sidebarPanel(
      selectInput("pokemon", "Select Pokémon", choices = NULL),

      selectInput("move", "Add Move", choices = NULL),
      checkboxInput("legacy", "Legacy move", value = FALSE),
      actionButton("add_move", "Add Move"),

      hr(),

      actionButton("remove_move", "Remove Selected Move")
    ),

    mainPanel(
      tableOutput("current_moves")
    )
  )
)

server <- function(input, output, session) {

  data <- reactiveVal(load_data())

  # Populate Pokémon selector
  observe({
    updateSelectInput(
      session,
      "pokemon",
      choices = setNames(
        data()$pokemon$pokemon_id,
        data()$pokemon$name
      )
    )
  })

  # Update move selector
observeEvent(input$pokemon, {
  used_moves <- cache$pokemon_moves %>%
    filter(pokemon_id == input$pokemon) %>%
    pull(move_id)

  available_moves <- cache$moves %>%
    filter(!move_id %in% used_moves)

  updateSelectInput(
    session,
    "move",
    choices = setNames(
      available_moves$move_id,
      available_moves$name
    )
  )
})


  # Show current moves
  output$current_moves <- renderTable({
    req(input$pokemon)

    cache$pokemon_moves %>%
      filter(pokemon_id == input$pokemon) %>%
      left_join(cache$moves, by = "move_id") %>%
      select(
        Move = name,
        Category = category,
        Type = type,
        Legacy = legacy
      )
  }, rownames = TRUE)


  # Add move
  observeEvent(input$add_move, {
    req(input$pokemon, input$move)

    new_row <- tibble(
      pokemon_id = as.numeric(input$pokemon),
      move_id = as.numeric(input$move),
      legacy     = input$legacy

    )

    updated <- bind_rows(data()$pokemon_moves, new_row)

    write_sheet(
      updated,
      ss = SHEET_ID,
      sheet = "pokemon_moves"
    )

      refresh_data()

  })

  observeEvent(input$remove_move, {
    req(input$pokemon)
    req(input$current_moves_rows_selected)

    selected_row <- input$current_moves_rows_selected

    current <- cache$pokemon_moves %>%
      filter(pokemon_id == input$pokemon)

    to_remove <- current[selected_row, ]

    updated <- anti_join(
      cache$pokemon_moves,
      to_remove,
      by = c("pokemon_id", "move_id", "legacy")
    )

    write_sheet(updated, SHEET_ID, "pokemon_moves")
    refresh_data()
})
  
  
}

shinyApp(ui, server)
