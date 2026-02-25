# devtools::install("C:/Users/james/pokemonGoSim")

library(tidyverse)
library(googlesheets4)
library(shiny)
library(pokemonGoSim)

base_stats <- readRDS("data/base_stats.rds")
levels <- readRDS("data/levels.rds")
user_move_combinations <- readRDS("data/user_move_combinations.RDS")
user_pokemon <- readRDS("data/user_pokemon.RDS")

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
    ),

    mainPanel(

      textOutput("user_pokemon")
    )
  )
)

server <- function(input, output, session) {

  
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
                    choices = charge_move_options)
})
  
  results <- eventReactive(input$run, {

    CP_Finder(
      pokemon = input$pokemon,
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
}

shinyApp(ui, server)