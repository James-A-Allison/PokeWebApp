# library(googlesheets4)
library(shiny)
library(DT)
library(dplyr)
# SHEET_ID <- "https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/"

files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

lapply(files, source)

pokemon <- get_pokemon_id()
moves <- get_moves()
pokemon_moves <- get_raw_pokemon_moves()

base_data <- get_base_stats() %>%
  select(name, `Lv 40 CP`, Released) %>%
  filter(!is.na(`Lv 40 CP`),
          (Released == "Yes" | is.na(Released))
        ) %>%
  distinct()


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

      # hr(),

      # actionButton("sync", "Sync to Google Sheets"),
      
      hr(),

      uiOutput("completion_bar"),

      hr(),

      tableOutput("best_mon_missing_moves")
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
    data$pokemon <- get_pokemon_id()
    data$moves <- get_moves()
    data$pokemon_moves <- get_raw_pokemon_moves()
    # data$last_refresh <- Sys.time()
  }

  # load once on startup
  refresh_data()

  
  move_dex_completion <- reactive({

  req(data$pokemon, data$pokemon_moves, data$moves)

  data$pokemon %>%
    inner_join(base_data) %>%
    rename(Pokemon = name) %>%
    left_join(data$pokemon_moves) %>%
    left_join(data$moves %>% rename(Move_Name = name)) %>%
    filter(legacy == "No" | is.na(legacy)) %>%
    group_by(Pokemon, pokemon_id, `Lv 40 CP`) %>%
    summarise(n_fast = n_distinct(move_id[category == "Fast"], na.rm = TRUE),
              n_charge = n_distinct(move_id[category == "Charge"], na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(valid = if_else(n_fast > 0 & n_charge > 0, "Yes", "No")) %>%
      count(valid, name = "Count") %>%
      mutate(
        Total = sum(Count),
        Share = Count / Total
      ) %>%
      filter(valid == "Yes") %>%
      pull(Share)
  })
    
  output$completion_bar <- renderUI({

    percent <- round(move_dex_completion() * 100)

    tags$progress(
      value = percent,
      max = 100,
      style = "width: 100%; height: 25px;"
    )
  })
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

output$best_mon_missing_moves <- renderTable({
  req(data$pokemon, data$pokemon_moves, data$moves)

  data$pokemon %>%
    inner_join(base_data) %>%
    rename(Pokemon = name) %>%
    left_join(data$pokemon_moves) %>%
    left_join(data$moves %>% rename(Move_Name = name)) %>%
    filter(legacy == "No" | is.na(legacy)) %>%
    group_by(Pokemon, pokemon_id, `Lv 40 CP`) %>%
    summarise(n_fast = n_distinct(move_id[category == "Fast"], na.rm = TRUE),
              n_charge = n_distinct(move_id[category == "Charge"], na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(valid = if_else(n_fast > 0 & n_charge > 0, "Yes", "No")) %>%
    filter(valid == "No") %>%
    arrange(desc(`Lv 40 CP`)) %>%
    slice(1:5) %>%
    select(Pokemon)
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

    # updated <- bind_rows(
    #   data$pokemon_moves,
    #   new_row) %>%
    #   distinct()

    # print(new_row)
    add_pokemon_move(new_row)
    # saveRDS(updated, "data/pokemon_moves.RDS")
    refresh_data()
  })

  # Remove move
  observeEvent(input$remove_move, {
    req(input$current_moves_rows_selected)
    # browser()
    current <- data$pokemon_moves %>%
      filter(pokemon_id == input$pokemon)

    to_remove <- current[input$current_moves_rows_selected, ]
    
    remove_move(pokemon_id = to_remove$pokemon_id,
                move_id = to_remove$move_id)
    # updated <- anti_join(
    #   data$pokemon_moves,
    #   to_remove,
    #   by = c("pokemon_id", "move_id", "legacy")
    # )

    # saveRDS(updated, "data/pokemon_moves.RDS")
    refresh_data()
  })

  #   observeEvent(input$sync, { ## syncs back to Google Sheets

  #   write_sheet(data$pokemon_moves, ss = SHEET_ID, sheet = "pokemon_moves")
  #   refresh_data()
  # })
}

shinyApp(ui, server)
