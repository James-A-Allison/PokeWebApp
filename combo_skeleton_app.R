library(shiny)
library(shinydashboard)
library(pokemonGoSim)
library(tidyverse)


type_colours <- c(
  Normal   = "#A8A77A",
  Fire     = "#EE8130",
  Water    = "#6390F0",
  Electric = "#F7D02C",
  Grass    = "#7AC74C",
  Ice      = "#96D9D6",
  Fighting = "#C22E28",
  Poison   = "#A33EA1",
  Ground   = "#E2BF65",
  Flying   = "#A98FF3",
  Psychic  = "#F95587",
  Bug      = "#A6B91A",
  Rock     = "#B6A136",
  Ghost    = "#735797",
  Dragon   = "#6F35FC",
  Dark     = "#705746",
  Steel    = "#B7B7CE",
  Fairy    = "#D685AD"
)

base_stats <- readRDS("data/base_stats.rds") 

poke_types <- base_stats %>% select(`Type 1`) %>% distinct() %>% pull()

mega_table <- base_stats %>%
  left_join(readRDS("data/mega_table.RDS") %>%
            mutate(can_mega_evolve = "Yes") %>%
    select(name = base_name, can_mega_evolve)) %>%
  mutate(can_mega_evolve = if_else(is.na(can_mega_evolve), "No", can_mega_evolve))


levels <- readRDS("data/levels.rds")
user_move_combinations <- readRDS("./data/user_move_combinations.RDS")
user_pokemon <- readRDS("data/user_pokemon.RDS")

moves_formatted <- readRDS("data/moves_formatted.RDS")

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "PokeCoach"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Team Summary", tabName = "team_summary"),
      menuItem("Roster Builder", tabName = "roster_builder"),
      menuItem("Raid Boss Breakdowns", tabName = "boss_breakdowns"),
      menuItem("Power Up Simulator", tabName = "power_ups")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "team_summary",
        h2("Team Summary Content")),

      tabItem(tabName = "roster_builder",
        h2("Roster Builder Content"),
          fluidPage(
            titlePanel("Pokémon GO CP / IV Finder"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(
                    "pokemon",
                    "Pokémon",
                    choices = sort(unique(base_stats$name))
                  ),
      selectInput("can_mega", "Can Mega evolve?", choices = NULL),
      selectInput("can_dyanamax", "Can battle in Max Raids?", choices = NULL, selected = "No"),
       numericInput("cp", "Target CP", value = 1500, min = 10, max = 10000),


      selectInput(
        "status",
        "Status",
        choices = c("Normal", "Lucky", "Shadow", "Purified"),
        selected = "Normal"
      ),

      selectInput("fast_move", "Fast Move", choices = NULL),

      selectInput("charge_move_1", "Charge Move 1", choices = NULL),

      selectInput("charge_move_2", "Charge Move 2", choices = NULL),


      uiOutput("dust_ui"),

      numericInput("visible_hp", "Visible HP (optional)", value = NA),
      numericInput("atk_iv", "Attack IV (optional)", value = NA, min = 0, max = 15),
      numericInput("def_iv", "Defence IV (optional)", value = NA, min = 0, max = 15),
      numericInput("hp_iv", "HP IV (optional)", value = NA, min = 0, max = 15),

      actionButton("run", "Find IVs", class = "btn-primary")
    ),

        mainPanel(
      dataTableOutput("iv_results"),
      actionButton("add_new", "Add Pokemon", class = "btn-primary"),
      selectInput("filter_pokemon", "Pokemon",
            choices = c("All", sort(unique(user_move_combinations$Pokemon)))),
      dataTableOutput("user_pokemon"),
      actionButton("remove_pokemon", "Remove Pokemon", class = "btn-primary"),
      plotOutput("level_dotplot"),
      dataTableOutput("attacker_type_summaries"),
      dataTableOutput("pokemon_summaries")
    )
  ))
      ),

      tabItem(tabName = "boss_breakdowns",
      h2("Boss Breakdown Content")),

      tabItem(tabName = "power_ups",
      h3("Power-Up Summary"))

    )
  )
)

server <- function(input, output) { }

shinyApp(ui, server)