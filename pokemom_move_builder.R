library(googlesheets4)
library(shiny)
SHEET_ID <- "https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/"



# refresh_data()

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

  data <- reactiveValues(
    pokemon = NULL,
    moves = NULL,
    pokemon_moves = NULL,
    last_refresh = NULL
  )

  refresh_data <- function() {
    data$pokemon <- read_sheet(SHEET_ID, sheet = "pokemon_ids")
    data$moves <- read_sheet(SHEET_ID, sheet = "move_ids")
    data$pokemon_moves <- read_sheet(SHEET_ID, sheet = "pokemon_moves")
    data$last_refresh <- Sys.time()
  }

  # load once on startup
  refresh_data()

  # Populate Pokémon selector
  observe({
    req(data$pokemon)

    updateSelectInput(
      session,
      "pokemon",
      choices = setNames(
        data$pokemon$pokemon_id,
        data$pokemon$name
      )
    )
  })

  # Update move selector
  observeEvent(input$pokemon, {
    req(data$pokemon_moves, data$moves)

    used_moves <- data$pokemon_moves %>%
      filter(pokemon_id == input$pokemon) %>%
      pull(move_id)

    available_moves <- data$moves %>%
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

    data$pokemon_moves %>%
      filter(pokemon_id == input$pokemon) %>%
      left_join(data$moves, by = "move_id") %>%
      select(Move = name, Legacy = legacy)
  }, rownames = TRUE)

  # Add move
  observeEvent(input$add_move, {
    req(input$pokemon, input$move)
    print(input$pokemon)
    print(input$move)

    # pokemon_id <- data$pokemon %>%
    #   filter(name == input$pokemon) %>%
    #   pull(pokemon_id)
    
    # print(pokemon_id)

    # move_id <- data$moves %>%
    #   filter(name == input$move) %>%
    #   pull(move_id)
    
    # print(move_id)

    new_row <- tibble(
        pokemon_id = as.numeric(input$pokemon),
        move_id = as.numeric(input$move),
        legacy = if_else(input$legacy == TRUE, 'Yes', 'No')
      )

    updated <- bind_rows(
      data$pokemon_moves,
      new_row) %>%
      distinct()

    print(new_row)
    write_sheet(updated, ss = SHEET_ID, sheet = "pokemon_moves")
    refresh_data()
  })

  # Remove move
  observeEvent(input$remove_move, {
    req(input$current_moves_rows_selected)

    current <- data$pokemon_moves %>%
      filter(pokemon_id == input$pokemon)

    to_remove <- current[input$current_moves_rows_selected, ]

    updated <- anti_join(
      data$pokemon_moves,
      to_remove,
      by = c("pokemon_id", "move_id", "legacy")
    )

    write_sheet(updated, ss = SHEET_ID, sheet = "pokemon_moves")
    refresh_data()
  })
}

shinyApp(ui, server)
