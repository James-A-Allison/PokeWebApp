library(googlesheets4)
library(shiny)
library(DT)
library(dplyr)
SHEET_ID <- "https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/"


pokemon <- readRDS("data/pokemon_ids.rds")
moves <- readRDS("data/move_ids.rds")
pokemon_moves <- readRDS("data/pokemon_moves.rds")

move_dex_completion <- pokemon %>%
  rename(Pokemon = name) %>%
  left_join(pokemon_moves) %>%
  left_join(moves %>% rename(Move_Name = name)) %>%
  filter(legacy == "No" | is.na(legacy)) %>%
  group_by(Pokemon, pokemon_id) %>%
  summarise(n_fast = n_distinct(move_id[category == "Fast"], na.rm = TRUE),
            n_charge = n_distinct(move_id[category == "Charge"], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(valid = if_else(n_fast > 0 & n_charge > 0, "Yes", "No")) %>%
  group_by(valid) %>%
  summarise(Count = n()) %>%
  mutate(Total = sum(Count),
        Share = Count/Total) %>%
  filter(valid == "Yes") %>%
  pull

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

      actionButton("remove_move", "Remove Selected Move"),

      hr(),

      actionButton("sync", "Sync to Google Sheets"),

      tags$progress(value = move_dex_completion * 100, max = 100, style = "width: 100%; height: 20px;")

    ),

    mainPanel(
      DT::dataTableOutput("current_moves")
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
    data$pokemon <- readRDS("data/pokemon_ids.rds")
    data$moves <- readRDS("data/move_ids.rds")
    data$pokemon_moves <- readRDS("data/pokemon_moves.rds")
    # data$last_refresh <- Sys.time()
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
output$current_moves <- DT::renderDataTable({
  req(input$pokemon)

  data$pokemon_moves %>%
    filter(pokemon_id == input$pokemon) %>%
    left_join(data$moves, by = "move_id") %>%
    select(
      Move = name,
      Legacy = legacy
    )
}, selection = "single", rownames = FALSE)

  # Add move
  observeEvent(input$add_move, {
    req(input$pokemon, input$move)

    new_row <- tibble(
        pokemon_id = as.numeric(input$pokemon),
        move_id = as.numeric(input$move),
        legacy = if_else(input$legacy == TRUE, 'Yes', 'No')
      )

    updated <- bind_rows(
      data$pokemon_moves,
      new_row) %>%
      distinct()

    # print(new_row)
    saveRDS(updated, "data/pokemon_moves.RDS")
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

    saveRDS(updated, "data/pokemon_moves.RDS")
    refresh_data()
  })

    observeEvent(input$sync, { ## syncs back to Google Sheets

    write_sheet(data$pokemon_moves, ss = SHEET_ID, sheet = "pokemon_moves")
    refresh_data()
  })
}

shinyApp(ui, server)
