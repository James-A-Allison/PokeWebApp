# devtools::install("C:/Users/james/pokemonGoSim")

library(tidyverse)
library(googlesheets4)
library(shiny)
library(pokemonGoSim)

base_stats <- readRDS("data/base_stats.rds")
levels <- readRDS("data/levels.rds")

ui <- fluidPage(
  titlePanel("Pokémon GO CP / IV Finder"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "pokemon",
        "Pokémon",
        choices = sort(unique(base_stats$name))
      ),

      numericInput("cp", "Target CP", value = 1500, min = 10, max = 10000),

      uiOutput("dust_ui"),

      selectInput(
        "status",
        "Status",
        choices = c("Normal", "Lucky", "Shadow", "Purified"),
        selected = "Normal"
      ),

      numericInput("visible_hp", "Visible HP (optional)", value = NA),
      numericInput("atk_iv", "Attack IV (optional)", value = NA, min = 0, max = 15),
      numericInput("def_iv", "Defence IV (optional)", value = NA, min = 0, max = 15),
      numericInput("hp_iv", "HP IV (optional)", value = NA, min = 0, max = 15),

      actionButton("run", "Find IVs", class = "btn-primary")
    ),

    mainPanel(
      h4("Possible matches"),
      tableOutput("results"),
      textOutput("result_count")
    )
  )
)

server <- function(input, output) {

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