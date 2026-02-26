# devtools::install("C:/Users/james/pokemonGoSim")

library(tidyverse)
library(googlesheets4)
library(shiny)
library(pokemonGoSim)
library(DT)
library(rhandsontable)
library(jsonlite)
library(htmlwidgets)

base_stats <- readRDS("data/base_stats.rds")
levels <- readRDS("data/levels.rds")
user_move_combinations <- readRDS("data/user_move_combinations.RDS")
user_pokemon <- readRDS("data/user_pokemon.RDS")

fast_moves <- user_move_combinations %>%
  distinct(fast_move) %>%
  arrange() %>%
  pull()

charge_moves <- user_move_combinations %>%
  distinct(charge_move) %>%
  arrange() %>%
  pull() %>% c("", .)

ui <- fluidPage(
  titlePanel("Pokémon GO CP / IV Finder"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "pokemon",
        "Pokémon",
        choices = sort(unique(base_stats$name))
      ),

      selectInput(
        "status",
        "Status",
        choices = c("Normal", "Lucky", "Shadow", "Purified"),
        selected = "Normal"
      ),

      selectInput("fast_move", "Fast Move", choices = NULL),

      selectInput("charge_move_1", "Charge Move 1", choices = NULL),

      selectInput("charge_move_2", "Charge Move 2", choices = NULL),

      numericInput("cp", "Target CP", value = 1500, min = 10, max = 10000),

      uiOutput("dust_ui"),

      numericInput("visible_hp", "Visible HP (optional)", value = NA),
      numericInput("atk_iv", "Attack IV (optional)", value = NA, min = 0, max = 15),
      numericInput("def_iv", "Defence IV (optional)", value = NA, min = 0, max = 15),
      numericInput("hp_iv", "HP IV (optional)", value = NA, min = 0, max = 15),

      actionButton("run", "Find IVs", class = "btn-primary"),

      h4("Possible matches"),
      tableOutput("results"),

      actionButton("add_new", "Add Pokemon", class = "btn-primary"),

    ),

    mainPanel(
    actionButton("modify", "Modify existing pokemon", class = "btn-primary"),
      rHandsontableOutput("user_pokemon")
    )
  )
)

server <- function(input, output, session) {

  user_table <- reactiveVal(readRDS("data/user_pokemon.RDS"))
  
observeEvent(input$pokemon, {
  req(input$pokemon)
  
  pokemon_moves <- user_move_combinations %>%
    filter(Pokemon == input$pokemon)
  
  fast_move_options   <- unique(pokemon_moves$fast_move)
  charge_move_options <- unique(pokemon_moves$charge_move)
  
  updateSelectInput(session, "fast_move",
                    choices = fast_move_options)
  
  updateSelectInput(session, "charge_move_1",
                    choices = charge_move_options)
  
  updateSelectInput(session, "charge_move_2",
                    choices = c("", charge_move_options))
})
  
  results <- eventReactive(input$run, {

    CP_Finder(
      pokemon = input$pokemon,
      base_stats = base_stats,
      levels = levels,
      target_CP = input$cp,
      status = input$status, #if (input$status == "Normal") NULL else input$status,
      target_dust = if (is.na(input$dust)) NULL else input$dust,
      target_visible_hp = if (is.na(input$visible_hp)) NULL else input$visible_hp,
      target_attack_IV = if (is.na(input$atk_iv)) NULL else input$atk_iv,
      target_defence_IV = if (is.na(input$def_iv)) NULL else input$def_iv,
      target_HP_IV = if (is.na(input$hp_iv)) NULL else input$hp_iv
    )

  })

  output$results <- renderTable({
    results()
  })

  output$dust_ui <- renderUI({

  dust_choices <- switch(
    input$status,
    "Lucky"    = sort(unique(levels$`Marginal Dust Lucky`)),
    "Shadow"   = sort(unique(levels$`Marginal Dust Shadow`)),
    "Purified" = sort(unique(levels$`Marginal Dust Purified`)),
    sort(unique(levels$`Marginal Dust`))
  )

  selectInput(
    "dust",
    "Dust Cost",
    choices = dust_choices,
    selected = NULL
  )
})

  output$result_count <- renderText({
    df <- results()
    if (is.null(df) || nrow(df) == 0) {
      "No matching IV combinations found."
    } else {
      paste(nrow(df), "possible IV combinations")
    }
  })

  output$user_pokemon <- renderRHandsontable({
      rhandsontable(user_table() %>%
      as_tibble() %>%
      rowwise() %>%
      mutate(CP = CP_Formula(pokemon = Pokemon, 
        base_stats = base_stats,
        levels = levels,
        level = Level,
        Attack_IV = `Attack IV`,
        Defence_IV = `Defence IV`,
        HP_IV = `HP IV`),
      .after = `Pokemon`)) %>%
      # Fixed columns
      hot_col("Pokemon", readOnly = TRUE) %>%
      hot_col("CP", readOnly = TRUE) %>%

      # Numeric editable
      hot_col("Level", type = "numeric", min = 1, max = 50) %>%
      hot_col("Attack IV", type = "numeric", min = 0, max = 15) %>%
      hot_col("Defence IV", type = "numeric", min = 0, max = 15) %>%
      hot_col("HP IV", type = "numeric", min = 0, max = 15) %>%

      # Dropdowns
      hot_col("Dust Status",
              type = "dropdown",
              source = c("Normal", "Lucky", "Shadow", "Purified")) %>%
      
      hot_col("Can Mega Evolve",
              type = "dropdown",
              source = c("Yes", "No")) %>%
        
      hot_col("Fast Move",
              type = "dropdown", 
            source = fast_moves) %>%
        
      hot_col("Charge1",
        type = "dropdown", source = charge_moves) %>%
        
      hot_col("Charge2",
        type = "dropdown", source = charge_moves) 
  })
}

shinyApp(ui, server)